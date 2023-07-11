library(ggplot2)



# SET THE APPROPRIATE WORKING DIRECTORY MANUALLY


#READ IN CSV WITH combintations used
#note: if I forget to save the combos.RDS file, I can probably recreate the table by
#1)searching through each directory, saving the unique names
#2) create a table with all combinations of these names

combos <- readRDS("combos.RDS")
combos_names = colnames(combos)

#replace bias_correct with the corresponding folder name
combos$bias_cor= ifelse(combos$bias_cor==TRUE,"BC_BCY","BC_BCN")
#replace obsmodel value with the corresponding folder name
combos$obsmodel= paste0("obsmodel",combos$obsmodel)
#replace knot_number value with the corresponding folder name
combos$knot_number= paste0("KN_",combos$knot_number)

#check for covariate folders and build into the table
covs = list.files(paste0(combos[1,2],"/",combos[1,4],"/",combos[1,3]))
covs = covs[covs %in% c("No_Cov","W_Cov")]

if(length(covs)==1){
  combos = cbind(combos,rep(covs[1],length(combos[,1])))
  colnames(combos) = c(combos_names,"Covs")
}
if(length(covs)==2){
  combos1 = cbind(combos,rep(covs[1],length(combos[,1])))
  combos2 = cbind(combos,rep(covs[2],length(combos[,1])))
  colnames(combos1) = c(combos_names,"Covs")
  colnames(combos2) = c(combos_names,"Covs")

  combos = rbind(combos1,combos2)
}

#rearrange columns to match path name
combos = cbind(combos[,1],combos[,2],combos[,4],combos[,3],combos[,8],combos[,5],combos[,6],combos[,7])
newnames = c(combos_names[1],combos_names[2],combos_names[4],combos_names[3],"Covs",combos_names[5],combos_names[6],combos_names[7])
colnames(combos) = newnames
combos=as.data.frame(combos)

#load in stratified mean data
strat_mean_species <- readRDS("Data/strat_mean_species.RDS")





#create list of folders to use
folders = vector()

for(rows in seq(nrow(combos))){
  #pull out first value
  fd = combos[rows,1]
  
  for(cols in seq(2,ncol(combos))){
    
    fd = paste0(fd,"/",combos[rows,cols])   
    
  } 
  folders[rows] = fd
}





pdf(file="Plots_Comparing_Estimates.pdf")


CNs=unique(combos$COMMON_NAME)[!unique(combos$COMMON_NAME)%in% c("DOGFISH, SPINY", "DOGFISH, SMOOTH")]

VAST_fit_all <- data.frame()
run_time_info <- data.frame()

#for(CN in CNs){


for(folder in folders){
  
  #pull out each value
  path = strsplit(folder,"/")
  
  
  SA = path[[1]][2] #stock area
  
  seasons =  unique(combos$season)
  
  VAST_fit_unit <- data.frame()
  SM_est_unit <- data.frame()
  
  for(season in seasons){
    
    
    
    covs = unique(combos$Covs)
    covs = covs[covs %in% c("No_Cov","W_Cov")]
    
    #pull out stratified mean estimate
    if(season=="SPRING"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="SPRING")}
    if(season=="FALL"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="FALL")}
    
    #add to overall SM est for unit
    SM_est_unit <- rbind(SM_est_unit,SM_est)
    
    for(cov in covs){
      
      
      
      #pull out vast estimate        
      VAST_fit_csv = read.csv(paste0(getwd(),"/",folder,"/Index_wYearSeason.csv"), header=T)
      VAST_fit_csv$SEASON = VAST_fit_csv$Season
      VAST_fit_csv$scenario = folder
      
      #add to overall vast dataframe for unit
      VAST_fit_unit <- rbind(VAST_fit_unit,VAST_fit_csv)
      
      #read in parameter estimates
      load(paste0(getwd(),"/",folder,"/parameter_estimates.RData"))
      
      hrsss=parameter_estimates$time_for_run[[1]]/(60*60)
      secc=as.numeric(parameter_estimates$time_for_run[[1]])
      newinfo = c(secc,hrsss,unlist(strsplit(folder,"/")))
      run_time_info = rbind(run_time_info,newinfo)
      
      
    }
  }
  
  VAST_fit_all <- rbind(VAST_fit_all,VAST_fit_unit)
  
  
  #plot each relative to stratified mean
  print(ggplot() +
          
          #plot VAST estimate with covariates 
          geom_errorbar(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = Covariate),width=.3) +
          geom_point(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON, color = Covariate),size=2)+
          geom_line(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON, color = Covariate))+
          
          #plot stratified calculation data 
          geom_errorbar(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute), color = "Strat Mean"),width=.3) +
          geom_point(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON, color = "Strat Mean"))+
          geom_line(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON, color = "Strat Mean"))+
          
          
          facet_wrap(~ SEASON, ncol =1, scales = "free") +
          # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
          #                                          "  FC=", toString(FC_spring), 
          #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
          #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
          #                                          "  FC=", toString(FC_fall),
          #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
          
          labs(x="Year",y="Biomass", title = folder, color ="" )+
          
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=12),
                title=element_text(size=8)))
  
}
#}

namess = newnames[-1]
colnames(run_time_info) = c("run time (sec)", "run time (hours)",namess)
write.csv(run_time_info,"run time info.csv")

#put legend below to see plots
ggplot(data=VAST_fit_all) +
  geom_point(aes(x=Year,y=Estimate,group=scenario,color=scenario),size=2)+
  geom_line(aes(x=Year,y=Estimate, group=scenario,color=scenario))+
  geom_errorbar(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=scenario,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = scenario),width=.3) +
 facet_wrap(~ SEASON, ncol =1, scales = "free")+ 
  theme(legend.position="bottom")+
  labs(x="Year",y="Biomass", title = paste0(CN," ",SA), color ="" )+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title=element_text(size=8))



#put legend on side so can read them  
ggplot(data=VAST_fit_all) +
  geom_point(aes(x=Year,y=Estimate,group=scenario,color=scenario),size=2)+
  geom_line(aes(x=Year,y=Estimate, group=scenario,color=scenario))+
  geom_errorbar(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=scenario,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = scenario),width=.3) +
  
  facet_wrap(~ SEASON, ncol =1, scales = "free")+
  labs(x="Year",y="Biomass", title = paste0(CN," ",SA), color ="" )+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title=element_text(size=8))
  


dev.off()


