#Calculate stratified mean given survey data

#I AM COPYING FROM CALC_SRS_INDEX_SURVEY_BENS, which was adapted from Liz's code to create below
library(tibble)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(here)



#stop output from below using this option
options(dplyr.summarise.inform = FALSE)


#load file to calculate the stratified mean
source("Scripts/fn_srs_survey_BENS.R")


strata_data <- read.csv("Data/strata_area.csv")


#tow_data_species <- readRDS("tow_data_species.RDS")
common_names <- readRDS("Data/common_names_stocks.RDS")

#pull out specific tow data set to work with. Will eventually make this a loop

strat_mean_species <- list()


for(species in common_names$COMMON_NAME){
  
  tempp = subset(common_names,COMMON_NAME==species)
  stock_names = tempp$STOCK_ABBREV
  
for(unitt in stock_names){

tow_data <- tow_data_species[[species]][[unitt]]

#use only Temp_Hub values to start
#USE ALL DATA..??
#tow_data = subset(tow_data,!is.na(Temp_Hub))

#use up to 2019 because 2020 missing
tow_data = subset(tow_data,YEAR<2020)

#replace NA weight values with 0
tow_data[is.na(tow_data[,"CATCH_WT_CAL"]),"CATCH_WT_CAL"]=0
range(tow_data$CATCH_WT_CAL)


Strata = unique(tow_data$STRATUM)

#add column to Strata with stratum area
tow_data = merge(tow_data,strata_data, by="STRATUM")

str=as.data.frame(unique(tow_data$STRATUM))
colnames(str)="STRATUM"
str_tmp=merge(str,strata_data,by="STRATUM")
sv.area = as_tibble(data.frame(str_tmp$STRATUM,str_tmp$STRATUM_AREA))
names(sv.area)=c("STRATUM","STRATUM_AREA")

spp <- as_tibble(tow_data,header=T)


# get total area of stock ====
spp.strata <- unique(spp$STRATUM)
spp.area <- sum(sv.area$STRATUM_AREA[sv.area$STRATUM %in% spp.strata]) #TOTAL AREA OF ALL STRATA

temp =  srs_survey(df=spp, sa=sv.area, str=NULL, ta=0.01, sppname = CN  )

strat_mean_species[[species]][[unitt]] <- temp %>%
  mutate(mean.yr.absolute=mean.yr*spp.area/.01, sd.mean.yr.absolute=sd.mean.yr*spp.area/.01,
         CV.absolute=sd.mean.yr.absolute/mean.yr.absolute) # if strata=NULL, the function will use the unique strata set found in df



}
}


#save stratified mean by species
saveRDS(strat_mean_species,"Data/strat_mean_species.RDS")






