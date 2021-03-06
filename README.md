# aSDMs

**GENERAL INFORMATION**

This repository contains the R code and data associated with the manuscript titled 'Acoustic Species Distribution Models (aSDMs): a framework to forecast shifts in calling behaviour under climate change' in revision in *Methods in Ecology and Evolution*.

The authors are:
Camille Desjonquères, Sara Villén-Pérez, Paulo De Marco, Rafael Márquez, Juan F. Beltrán, & Diego Llusia

Please contact [Camille Desjonquères](cdesjonqu@gmail.com) and [Diego Llusia](diego.llusia@uam.es) for any queries.

**Data requirement**
- Local calling behaviour (daily or hourly)
- Local weather (daily)
- Species distribution range (as a polygon or UTM)
- Present and future climate over the species distribution range

**Overall structure**

The root contains three files (the readme and two spatial data files for the Iberic Peninsula) and three folders (Ha, functions, and thermal-extremes).
The code is assuming that the working directory is '/Ha/data'

## Thermal-extreme folder

The Thermal-extreme folder contains one Rmd file containing R code to find sites that are at the climatic extremes of a species distribution.

## Functions folder

The functions folder contains 4 R files. 2 to format the hourly and daily data to monthly data. One to assess assumptions of a GLMM and one to calculate the confidence intervals of a GLMM.


## Ha folder

The Ha folder contains the core of the aSDMs code divided in 6 folders (1 for data and 5 for scripts) that we describe below.

### Data

Folder that contains all the datasets required to run the code: 
- hourly calling activity of the study species (Ha_act-hourly.csv) 
- coordinates of the recording sites (Ha_site_coordinates.csv) 
- species distribution range (UTM_Ha.csv)
- 5 weather files (4 aemetXX and ogimet-temp-2007-08571-Portalegre.csv)
- results of the 1000 iterations of the training/validation process (model_boot_ID.Rdata) obtained with the '220203_validationtemporelle.R' code in the '3_temporal-validation folder' (see below)
- 2 Rdata files containing Iberic Peninsula climate data downloaded from worldclim (spain_regc.Rdata and spain_regf.Rdata for current and future data respectively).

### 1_format

3 R codes to format the acoustic and environmental data:
- 200327_format_diara-dataset_aemet.R
- 200330_atlas-presabs.R
- 210203_format-datasets.R

### 2_suit-corr

4 R codes to calculate the environmental suitability for calling using the boundary and regression model:
- 200312_breadthfunctions.R
- 200818_callingbreadth.R
- 200818_model.R
- 211206_plot.R

### 3_temporal-validation

2 R codes to apply a cross-validation on both models and assess the results of these validations:
- 211206_valtemp-assessment.R 
- 220203_validationtemporelle.R 

### 4_spatiotemp-predictions

1 R code to compute the predictions of the models over the whole study region and period: 200820_predictions.R

### 5_future-vs-present

2 R codes to plot environmental suitability distribution and shifts:
- 200328_prepclock.R
- 211117_future-vs-present.R
