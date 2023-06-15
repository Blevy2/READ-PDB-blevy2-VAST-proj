library(ggplot2)

orig.dir <- getwd()

#set wd() as VAST directory
setwd("~/blevy_net/Bens_R_Projects/READ-PDB-blevy2-VAST-proj/VAST_runs/COD,ATLANTIC_GBK/obsmodel7/spring")

VAST_fit = read.csv("Index_wYear.csv", header=T)
VAST_fit$SEASON="SPRING"

#load in stratified mean data
setwd(orig.dir)
strat_mean_species <- readRDS("Data/strat_mean_species.RDS")

CN = "COD,ATLANTIC"
SA = "GBK"
SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="SPRING")

ggplot() +

  #plot VAST estimate with covariates 
  geom_errorbar(data=VAST_fit,aes(x=Year,y=Estimate,group=SEASON,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = "VAST W/ Cov"),width=.3) +
  geom_point(data=VAST_fit,aes(x=Year,y=Estimate,group=SEASON, color = "VAST W/ Cov"),size=2)+
  geom_line(data=VAST_fit,aes(x=Year,y=Estimate,group=SEASON, color = "VAST W/ Cov"))+
  
  #plot stratified calculation data 
  geom_errorbar(data=SM_est,aes(x=YEAR,y=mean.yr.absolute,group=SEASON,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean"),width=.3) +
  geom_point(data=SM_est,aes(x=YEAR,y=mean.yr.absolute,group=SEASON, color = "Strat Mean"))+
  geom_line(data=SM_est,aes(x=YEAR,y=mean.yr.absolute,group=SEASON, color = "Strat Mean"))+
  
  
  facet_wrap(~ SEASON, ncol =1) +
  # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FC=", toString(FC_spring), 
  #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
  #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
  #                                          "  FC=", toString(FC_fall),
  #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
  
  labs(x="Year",y="Biomass", title = paste0(CN," ",SA), color ="" )+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title=element_text(size=8))
