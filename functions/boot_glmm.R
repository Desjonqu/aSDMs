#Author: Roger Mundry
#Function to calculate estimates and confidence intervals for glmm models

require(lme4)
boot.glmm.pred<-function(model.res, excl.warnings=F, nboots=1000, para=F, resol=100, level=0.95, use=NULL, circ.var.name=NULL, circ.var=NULL, use.u=F){
	keepWarnings<-function(expr){
		localWarnings <- list()
		value <- withCallingHandlers(expr,
			warning = function(w) {
				localWarnings[[length(localWarnings)+1]] <<- w
				invokeRestart("muffleWarning")
			}
		)
		list(value=value, warnings=localWarnings)
	}
	if(excl.warnings){
		boot.fun<-function(x, model.res., keepWarnings., use.u.){
			xdone=F
			while(!xdone){
				i.res=keepWarnings(bootMer(x=model.res., FUN=fixef, nsim=1, use.u=use.u.)$t)
				if(length(unlist(i.res$warnings)$message)==0){
					xdone=T
				}
			}
			return(i.res$value)
		}
	}else{
		boot.fun<-function(y, model.res., keepWarnings., use.u.){
			#keepWarnings.(bootMer(x=model.res., FUN=fixef, nsim=1)$t)
			bootMer(x=model.res., FUN=fixef, nsim=1, use.u=use.u.)$t
		}
	}
	if(para){
		require(parallel)
		cl <- makeCluster(getOption("cl.cores", detectCores()))
		parLapply(cl=cl, 1:length(cl), fun=function(x){
		  library(lme4)
		  return(invisible(""))
		})
		all.res=parLapply(cl=cl, X=1:nboots, fun=boot.fun, model.res.=model.res, keepWarnings.=keepWarnings, use.u.=use.u)
		parLapply(cl=cl, X=1:length(cl), fun=function(x){rm(list=ls())})
		stopCluster(cl)
	}else{
    all.res=lapply(X=1:nboots, FUN=boot.fun, model.res.=model.res, use.u.=use.u)#, keepWarnings=excl.warnings)
	}
	if(length(use)>0){
		#extract fixed effects terms from the model:
		xcall=as.character(model.res@call)[2]
		model.terms=attr(terms(as.formula(xcall)), "term.labels")
		REs=names(ranef(model.res))
		for(i in 1:length(REs)){
			model.terms=model.terms[!grepl(x=model.terms, pattern="|", fixed=T)]
		}
		#build model wrt to the fixed effects:
		model=paste(model.terms, collapse="+")
		#exclude interactions and squared terms from model.terms:
		model.terms=model.terms[!grepl(x=model.terms, pattern=":", fixed=T)]
		model.terms=model.terms[!grepl(x=model.terms, pattern="^", fixed=T)]
		
		#create new data to be used to determine fitted values:
		ii.data=model.res@frame
		if(length(use)==0){use=model.terms}
		new.data=vector("list", length(model.terms))
		if(length(circ.var.name)==1){
			set.circ.var.to.zero=sum(circ.var.name%in%use)==0
		}else{
			set.circ.var.to.zero=F
		}
		usel=model.terms%in%use
		#if(length(use)>0)
		for(i in 1:length(model.terms)){
			if(is.factor(ii.data[, model.terms[i]])){
				new.data[[i]]=levels(ii.data[, model.terms[i]])
			}else if(!is.factor(ii.data[, model.terms[i]]) & usel[i]){
				new.data[[i]]=seq(from=min(ii.data[, model.terms[i]]), to=max(ii.data[, model.terms[i]]), length.out=resol)
			}else{
				new.data[[i]]=0
			}
		}
		names(new.data)=model.terms
		if(length(circ.var.name)==1){
			new.data=new.data[!(model.terms%in%paste(c("sin(", "cos("), circ.var.name, ")", sep=""))]
			if(sum(grepl(pattern=circ.var.name, x=use))>0){
				new.data=c(new.data, list(seq(min(circ.var, na.rm=T), max(circ.var, na.rm=T), length.out=resol)))
				names(new.data)[length(new.data)]=circ.var.name
			}else{
				new.data=c(new.data, list(0))
			}
			model.terms=model.terms[!(model.terms%in%paste(c("sin(", "cos("), circ.var.name, ")", sep=""))]
		}
		xnames=names(new.data)
		new.data=data.frame(expand.grid(new.data))
		#names(new.data)[1:length(model.terms)]=model.terms
		#browser()
		if(length(circ.var.name)==1){
			names(new.data)[ncol(new.data)]=circ.var.name
		}
		#create predictors matrix:
		r=runif(nrow(new.data))
		m.mat=model.matrix(object=as.formula(paste(c("r", model), collapse="~")), data=new.data)
		if(set.circ.var.to.zero){
			m.mat[,paste(c("sin(", circ.var.name, ")"), collapse="")]=0
			m.mat[,paste(c("cos(", circ.var.name, ")"), collapse="")]=0
		}
		m.mat=t(m.mat)
		#get the CIs for the fitted values:
		ci=lapply(all.res, function(x){
			return(apply(m.mat[names(fixef(model.res)), ]*as.vector(x), 2, sum))
		})
		ci=matrix(unlist(ci), ncol=nboots, byrow=F)
		ci=t(apply(ci, 1, quantile, prob=c((1-level)/2, 1-(1-level)/2), na.rm=T))
		colnames(ci)=c("lower.cl", "upper.cl")
		fv=apply(t(m.mat[names(fixef(model.res)),]*fixef(model.res)), 1, sum)
		if(class(model.res)[[1]]!="lmerMod"){
			if(model.res@resp$family$family=="binomial"){
				ci=exp(ci)/(1+exp(ci))
				fv=exp(fv)/(1+exp(fv))
			}else if(model.res@resp$family$family=="poisson"){
				ci=exp(ci)
				fv=exp(fv)
			}
		}
		result=data.frame(new.data, fitted=fv, ci)
	}else{
		result=NULL
	}
	ci.est=apply(matrix(unlist(all.res), ncol=length(fixef(model.res)), byrow=T), 2, quantile, prob=c((1-level)/2, 1-(1-level)/2), na.rm=T)
	#browser()
	ci.est=data.frame(orig=fixef(model.res), t(ci.est))
	return(list(ci.predicted=result, ci.estimates=ci.est))
}
###########################################################################################################
###########################################################################################################
boot.glmm<-function(model.res, excl.warnings=F, nboots=1000, para=F){
	keepWarnings<-function(expr) {
		localWarnings <- list()
		value <- withCallingHandlers(expr,
			warning = function(w) {
				localWarnings[[length(localWarnings)+1]] <<- w
				invokeRestart("muffleWarning")
			}
		)
		list(value=value, warnings=localWarnings)
	}
	if(excl.warnings){
		boot.fun<-function(x, model.res., keepWarnings.){
			xdone=F
			while(!xdone){
				i.res=keepWarnings.(bootMer(x=model.res., FUN=fixef, nsim=1)$t)
				if(length(unlist(i.res$warnings)$message)==0){
					xdone=T
				}
			}
			return(i.res$value)
		}
	}else{
		boot.fun<-function(x, model.res., keepWarnings.){
			bootMer(x=model.res, FUN=fixef, nsim=1)$t
		}
	}
	if(para){
		require(parallel)
    cl <- makeCluster(getOption("cl.cores", detectCores()))
    parLapply(cl=cl, 1:length(cl), fun=function(x){
      library(lme4)
      return(invisible(""))
    })
    all.coeffs=parLapply(cl=cl, X=1:nboots, fun=boot.fun, model.res.=model.res, keepWarnings.=keepWarnings)
    parLapply(cl=cl, X=1:length(cl), fun=function(x){rm(list=ls())})
    stopCluster(cl)
	}else{
    all.coeffs=lapply(X=1:nboots, FUN=boot.fun, model.res.=model.res, keepWarnings.=keepWarnings)
	}
	ci=matrix(unlist(all.coeffs), nrow=length(all.coeffs), byrow=T)
	colnames(ci)=names(fixef(model.res))
	ci=apply(ci, 2, quantile, prob=c(0.025, 0.975), na.rm=T)
	ci=data.frame(orig=fixef(model.res), t(ci))
	return(ci)
}


