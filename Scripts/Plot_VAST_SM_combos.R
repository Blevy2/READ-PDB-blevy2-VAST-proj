library(ggplot2)



# SET THE APPROPRIATE WORKING DIRECTORY MANUALLY
#should be orig.dir

#READ IN CSV WITH combintations used
#note: if I forget to save the combos.RDS file, I can probably recreate the table by
#1)searching through each directory, saving the unique names
#2) create a table with all combinations of these names

###################################################################################
#read in appropriate combos file (make sure its the original)
combos <- readRDS("combos.RDS")
#if just one combos then there is just 1 covariate
#figure out with covariate and add to data table
covs = list.files(paste0(combos2[1,1],"/",combos2[1,2],"/",combos2[1,4],"/",combos2[1,3]))
covs = covs[covs %in% c("No_Cov","W_Cov")]
if(length(covs)==1){
  combos2 = cbind(combos2,rep(covs[1],length(combos2[,1])))
  colnames(combos2) = c(combos2_names,"Covs")
}



#or combine two combos (with and without covariates)
#reduce combos to those that finished
#if combining two, need to combined finished ones
combosA=combos_7_16  #W_Cov
combosB=combos_7_18_NoCov  #No_Cov
finished_allA = finished_all2
finished_allB = finished_all_noCov2

combosA$Covs="W_Cov"
combosB$Covs = "No_Cov"


scenario_numberA = seq(length(combosA$COMMON_NAME))
scenario_numberB = seq(length(combosB$COMMON_NAME))

combosA = subset(combosA,scenario_numberA%in%finished_allA$scenario_number)
combosB = subset(combosB,scenario_numberA%in%finished_allB$scenario_number)

combos2=rbind(combosA,combosB)


combos=rbind(combos,combos_7_16)


###################################################################################



#removed scenario_num from combos################################


combos2_names=colnames(combos2)

#replace bias_correct with the corresponding folder name
combos2$bias_corr= ifelse(combos2$bias_corr==TRUE,"BC_BCY","BC_BCN")
#replace obsmodel value with the corresponding folder name
combos2$obsmodel= paste0("obsmodel",combos2$obsmodel)
#replace knot_number value with the corresponding folder name
combos2$N_knots= paste0("KN_",combos2$N_knots)


# if(length(covs)==2){
#   combos3 = cbind(combos2,rep(covs[1],length(combos2[,1])))
#   combos4 = cbind(combos2,rep(covs[2],length(combos2[,1])))
#   colnames(combos3) = c(combos2_names,"Covs")
#   colnames(combos4) = c(combos2_names,"Covs")
# 
#   combos2 = rbind(combos3,combos4)
# }

#rearrange columns to match path name
combos2 = cbind(combos2[,1],combos2[,2],combos2[,4],combos2[,3],combos2[,8],combos2[,5],combos2[,6],combos2[,7])
newnames = c(combos2_names[1],combos2_names[2],combos2_names[4],combos2_names[3],"Covs",combos2_names[5],combos2_names[6],combos2_names[7])
colnames(combos2) = newnames
combos2=as.data.frame(combos2)

#load in stratified mean data
strat_mean_species <- readRDS("Data/strat_mean_species.RDS")



#sort alphabetically so folders object below comes out in correct order
combos2=combos2[with(combos2,order(COMMON_NAME,STOCK_ABBREV,rev(SEASON))),]


#THIS NOW INSIDE LOOP
# combosX = subset(combos2,COMMON_NAME==CN)
# 
# for(rows in seq(nrow(combosX))){
#   #pull out first value
#   fd = combosX[rows,1]
#   
#   for(cols in seq(2,ncol(combosX))){
#     
#     
#     #replace KN_N_Samp with correct number 
#     if(cols==7){
#       #print(fd)
#       tfile = list.files(paste0(getwd(),"/",fd))
#       combosX[rows,cols] = paste0("KN_",substr(tfile,4,6))
#     }
#     
#     
#     fd = paste0(fd,"/",combosX[rows,cols])   
#     
#   } 
#   folders[rows] = fd
# }

#CHANGE WORKING DIRECTORY TO WHERE VAST RUNS ARE. PROBABLY VAST_RUNS
setwd(paste0(orig.dir,"/VAST_runs"))

pdf(file="Plots_Comparing_Estimates.pdf")


#CNs=unique(combos$COMMON_NAME)[!unique(combos$COMMON_NAME)%in% c("DOGFISH, SPINY", "DOGFISH, SMOOTH")]
CNs = unique(combos2$COMMON_NAME)


VAST_fit_all <- data.frame()
run_time_info <- data.frame()

for(CN in CNs){
  
  #create list of folders to use
  folders = vector()
  combosX = subset(combos2,COMMON_NAME==CN)
  
  SAs = unique(combosX$STOCK_ABBREV)
  
  for(rows in seq(nrow(combosX))){
    #pull out first value
    fd = combosX[rows,1]
    
    for(cols in seq(2,ncol(combosX))){
      
      
      #replace KN_N_Samp with correct number 
      if(cols==7){
        #print(fd)
        tfile = list.files(paste0(getwd(),"/",fd))
        combosX[rows,cols] = paste0("KN_",substr(tfile,4,6))
      }
      
      
      fd = paste0(fd,"/",combosX[rows,cols])   
      
    } 
    folders[rows] = fd
  }
  
  folder_idx=1
 

#for(folder in folders){
  
  #pull out each value
 # path = strsplit(folder,"/")
  
#  CN = path[[1]][1] #species
 # SA = path[[1]][2] #stock area
 
  

  
   for(SA in SAs){ 
     
  seasons =  unique(subset(combos2,COMMON_NAME==CN&STOCK_ABBREV==SA))[["SEASON"]]
  seasons = unique(seasons)
     VAST_fit_unit <- data.frame()
  SM_est_unit <- data.frame()
  for(season in seasons){
    
    
    
    covs =  unique(subset(combos2,COMMON_NAME==CN&STOCK_ABBREV==SA&SEASON==season))[["Covs"]]
   
    #pull out stratified mean estimate
    if(season=="SPRING"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="SPRING")}
    if(season=="FALL"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="FALL")}
    
    #add to overall SM est for unit
    SM_est_unit <- rbind(SM_est_unit,SM_est)
    
    for(cov in covs){
      
      
      
      #pull out vast estimate        
      VAST_fit_csv = read.csv(paste0(getwd(),"/",folders[folder_idx],"/Index_wYearSeason.csv"), header=T)
      VAST_fit_csv$SEASON = VAST_fit_csv$Season
      VAST_fit_csv$scenario = folders[folder_idx]
      
      #add to overall vast dataframe for unit
      VAST_fit_unit <- rbind(VAST_fit_unit,VAST_fit_csv)
      
      #read in parameter estimates
      load(paste0(getwd(),"/",folders[folder_idx],"/parameter_estimates.RData"))
      
      hrsss=parameter_estimates$time_for_run[[1]]/(60*60)
      secc=as.numeric(parameter_estimates$time_for_run[[1]])
      newinfo = c(secc,hrsss,unlist(strsplit(folder,"/")))
      run_time_info = rbind(run_time_info,newinfo)
      
      folder_idx = folder_idx+1
    }
      
  VAST_fit_all <- rbind(VAST_fit_all,VAST_fit_unit)
  }
   }

  
  
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
          
          labs(x="Year",y="Biomass", title = paste0(CN," ",SA), color ="" )+
          
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=12),
                title=element_text(size=8)))
  

#}


#plot VAST only 
#put legend below to see plots
print(ggplot(data=VAST_fit_unit) +
  #plot VAST estimate with covariates 
  geom_errorbar(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = Covariate),width=.3) +
  geom_point(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON, color = Covariate),size=2)+
  geom_line(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=SEASON, color = Covariate))+
  
 facet_wrap(~ SEASON, ncol =1, scales = "free")+ 
  theme(legend.position="bottom")+
  labs(x="Year",y="Biomass", title = paste0(CN," ",SA," VAST ONLY"), color ="" )+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title=element_text(size=8)))


#plot SM only
print(ggplot() +
        
        #plot stratified calculation data 
        geom_errorbar(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
        geom_point(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON))+
        geom_line(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON))+
        
        
        facet_wrap(~ SEASON, ncol =1, scales = "free") +
        # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
        #                                          "  FC=", toString(FC_spring), 
        #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
        #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
        #                                          "  FC=", toString(FC_fall),
        #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
        
        labs(x="Year",y="Biomass", title = paste0(CN," ",SA,"SM ONLY"), color ="" )+
        
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=12),
              title=element_text(size=8)))


# #put legend on side so can read them  
# ggplot(data=VAST_fit_all) +
#   geom_point(aes(x=Year,y=Estimate,group=scenario,color=scenario),size=2)+
#   geom_line(aes(x=Year,y=Estimate, group=scenario,color=scenario))+
#   geom_errorbar(data=VAST_fit_unit,aes(x=Year,y=Estimate,group=scenario,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = scenario),width=.3) +
#   
#   facet_wrap(~ SEASON, ncol =1, scales = "free")+
#   labs(x="Year",y="Biomass", title = paste0(CN," ",SA," VAST ONLY"), color ="" )+
#   
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12),
#         title=element_text(size=8))
#   
}



dev.off()



colnames(run_time_info) = c("run time (sec)", "run time (hours)",newnames)
write.csv(run_time_info,"run time info.csv")
