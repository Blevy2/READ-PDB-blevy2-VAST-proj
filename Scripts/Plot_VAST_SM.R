library(ggplot2)

orig.dir <- getwd()


#load in stratified mean data
setwd(orig.dir)
strat_mean_species <- readRDS("Data/strat_mean_species.RDS")


#all species names
#tow_data_wTemp <- readRDS("tow_data_wTemp.RDS")
#common_names <- unique(tow_data_wTemp[c("COMMON_NAME","SPECIES_ITIS","STOCK_ABBREV")])
#remove(tow_data_wTemp)

#specific species names
#common_names <- c("COD,ATLANTIC")

#all names in VAST_runs folder
common_names =  list.files(paste0(getwd(),"/VAST_runs"))

#stock areas 
#stock_areas <- c("GBK") #provided within the loop instead

#VAST obsmodels to use
#obsmodels <- c(7)

#seasons to run
#seasons <- c("FALL")

#use covariates in VAST?
#use_cov <- FALSE

pdf(file="Plots_Comparing_Estimates.pdf")


for(CN in common_names){
  
 stock_areas =  list.files(paste0(getwd(),"/VAST_runs/",CN))
  
  for(SA in stock_areas){

    obsmodels =  list.files(paste0(getwd(),"/VAST_runs/",CN,"/",SA))
    

  for(obsmodel in obsmodels){
    
      seasons =  list.files(paste0(getwd(),"/VAST_runs/",CN,"/",SA,"/",obsmodel))
         
        VAST_fit_unit <- data.frame()
         SM_est_unit <- data.frame()
  
        for(season in seasons){
          
          covs = list.files(paste0(getwd(),"/VAST_runs/",CN,"/",SA,"/",obsmodel,"/",season))
          covs = covs[covs %in% c("No_Cov","W_Cov")]
          
              #pull out stratified mean estimate
          if(season=="SPRING"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="SPRING")}
          if(season=="FALL"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="FALL")}
          
          #add to overall SM est for unit
          SM_est_unit <- rbind(SM_est_unit,SM_est)
          
          for(cov in covs){
      
      
    #pull out vast estimate        
VAST_fit_csv = read.csv(paste0(getwd(),"/VAST_runs/",CN,"/",SA,"/",obsmodel,"/",season,"/",cov,"/Index_wYearSeason.csv"), header=T)
VAST_fit_csv$SEASON = VAST_fit_csv$Season

#add to overall vast dataframe for unit
VAST_fit_unit <- rbind(VAST_fit_unit,VAST_fit_csv)


          }
        }

print(ggplot() +

  #plot VAST estimate with covariates 
  geom_errorbar(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = Covariate),width=.3) +
  geom_point(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON, color = Covariate),size=2)+
  geom_line(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON, color = Covariate))+
  
  #plot stratified calculation data 
  geom_errorbar(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean"),width=.3) +
  geom_point(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON, color = "Strat Mean"))+
  geom_line(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON, color = "Strat Mean"))+
  
  
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
        title=element_text(size=8)))


      }
    }
  }
  

dev.off()
