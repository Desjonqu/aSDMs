#Author: Roger Mundry
#Function to check assumptions on model residuals 
#version for linear models course 2015

ranef.diagn.plot<-function(model.res, QQ=F){
  old.par = par(no.readonly = TRUE)
	n.plots=sum(unlist(lapply(ranef(model.res), length)))
	x=ifelse(n.plots%%2==1,n.plots+1,n.plots)
	xmat=outer(1:x, 1:(1+x/2), "*")-n.plots
	colnames(xmat)=1:(1+x/2)
	rownames(xmat)=1:x
	xmat=as.data.frame(as.table(xmat))
	xmat=subset(xmat, as.numeric(as.character(xmat$Var1))>=as.numeric(as.character(xmat$Var2)) & xmat$Freq>=0)
	sum.diff=as.numeric(as.character(xmat$Var1))-as.numeric(as.character(xmat$Var2))+xmat$Freq
	xmat=xmat[sum.diff==min(sum.diff),]
	xmat=xmat[which.min(xmat$Freq),]
	par(mfrow=c(xmat$Var2, xmat$Var1))
	par(mar=c(rep(2, 3), 1))
	par(mgp=c(1, 0.5, 0))
	for(i in 1:length(ranef(model.res))){
		to.plot=ranef(model.res)[[i]]
		for(k in 1:ncol(to.plot)){
			if(QQ){
				qqnorm(to.plot[,k], main="", tcl=-0.25, xlab="", ylab="", pch=19, col=grey(0.5, alpha=0.75))
				qqline(to.plot[,k])
			}else{
				hist(to.plot[,k], main="", tcl=-0.25, xlab="", ylab="")
			}
			mtext(text=paste(c(names(ranef(model.res)[i]), colnames(to.plot)[k]), collapse=", "), side=3)
		}
	}
	par(old.par)
}

how.many.uniques.aao<-function(fe, re, data){
	data=droplevels(as.data.frame(na.omit(data[, c(fe, re)])))
	to.do=data.frame(expand.grid(re, fe))
	names(to.do)=c("re", "fe")
	to.do$re=as.character(to.do$re)
	to.do$fe=as.character(to.do$fe)
	res.detailed=lapply(1:nrow(to.do), function(xrow){
		table(data[,to.do$re[xrow]], data[,to.do$fe[xrow]])
	})
	res.summary=lapply(res.detailed, function(xtab){
		table(apply(xtab>0, 1, sum))
	})
	xnames=paste(to.do$fe, to.do$re, sep="_within_")
	names(res.detailed)=xnames
	names(res.summary)=xnames
	return(list(detailed=res.detailed, summary=res.summary))
}

diagnostics.plot<-function(mod.res, col=grey(level=0.25, alpha=0.5)){
  old.par = par(no.readonly = TRUE)
  par(mfrow=c(2, 2))
  par(mar=c(3, 3, 1, 0.5))
  hist(residuals(mod.res), probability=T, xlab="", ylab="", main="")
  mtext(text="histogram of residuals", side=3, line=0)
  x=seq(min(residuals(mod.res)), max(residuals(mod.res)), length.out=100)
  lines(x, dnorm(x, mean=0, sd=sd(residuals(mod.res))))
  qqnorm(residuals(mod.res), main="", pch=19)
  qqline(residuals(mod.res))
  mtext(text="qq-plot of residuals", side=3, line=0)
  plot(fitted(mod.res), residuals(mod.res), pch=19, col=col)
  abline(h=0, lty=2)
  mtext(text="residuals against fitted values", side=3, line=0)
  par(old.par)
}

lev.thresh<-function(model.res){
	k=length(coefficients(model.res))
	n=length(residuals(model.res))
 return(2*(k+1)/n)
}

overdisp.test<-function(x){
  pr=residuals(x, type ="pearson")
  sum.dp=sum(pr^2)
  if(class(x)[[1]]=="mer"){
    xdf=length(residuals(x))-length(fixef(x))
  }else if(class(x)[[1]]=="glmerMod"){
    xdf=length(residuals(x))-length(fixef(x))
  }else{
    xdf=length(residuals(x))-length(x$coefficients)
  }
  return(data.frame(chisq=sum.dp, df=xdf, P=1-pchisq(sum.dp, xdf), dispersion.parameter=sum.dp/xdf))
}

how.many.uniques<-function(xfac, xcov){
  ii.data=data.frame(xfac=factor(xfac), xcov=xcov)
  ii.data=data.frame(na.omit(ii.data))
  ires=tapply(ii.data$xcov, ii.data$xfac, function(ii){
    length(unique(ii))
  })
  return(ires)
}
