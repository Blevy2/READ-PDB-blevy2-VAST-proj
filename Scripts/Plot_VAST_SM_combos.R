library(ggplot2)
library(VAST)
library(dplyr)



#load in stratified mean data
strat_mean_species <- readRDS("Data/strat_mean_species.RDS")

tow_data_species <- readRDS("tow_data_species.RDS")


scenario = "2xKnots_NMFSdata_SurveyTemp" #specific VAST_runs folder name
tt=strsplit(scenario,"_")
survey_type = tt[[1]][2]
cov_data_type = tt[[1]][3]

cov_type = ifelse(cov_data_type=="SurveyTemp","BOT_TEMP","Temp_Hub")

orig.dir = getwd()

#CHANGE WORKING DIRECTORY TO WHERE VAST RUNS ARE. PROBABLY VAST_RUNS
setwd(paste0(getwd(),"/VAST_runs/VAST_runs_",scenario))


#READ IN CSV WITH combintations used
#note: if I forget to save the combos.RDS file, I can probably recreate the table by
#1)searching through each directory, saving the unique names
#2) create a table with all combinations of these names

###################################################################################
###################################################################################
#read in appropriate combos file (make sure its the original)
combos <- readRDS("combos.RDS")

#if just ONE combos then there is just 1 covariate
#figure out with covariate and add to data table
covs = list.files(paste0(combos2[1,1],"/",combos2[1,2],"/",combos2[1,4],"/",combos2[1,3]))
covs = covs[covs %in% c("No_Cov","W_Cov")]
if(length(covs)==1){
  combos2 = cbind(combos2,rep(covs[1],length(combos2[,1])))
  colnames(combos2) = c(combos2_names,"Covs")
}



#or combine TWO combos (with and without covariates)
#reduce combos to those that finished
#if combining two, need to combined finished ones
combosA=combos_WCov  #W_Cov
combosB=combos_NoCov  #No_Cov
finished_allA = finished_all_WCov2
finished_allB = finished_all_NoCov2

combosA$Covs="W_Cov"
combosB$Covs = "No_Cov"


scenario_numberA = seq(length(combosA$COMMON_NAME))
scenario_numberB = seq(length(combosB$COMMON_NAME))

combosA = subset(combosA,scenario_numberA%in%finished_allA$scenario_number)
combosB = subset(combosB,scenario_numberA%in%finished_allB$scenario_number)

combos2=rbind(combosA,combosB)


#combos=rbind(combos,combos_7_16)




#or combine THREE combos (with and without covariates)
#reduce combos to those that finished
#if combining three, need to combined finished ones
combos_NoCov_alltows <- readRDS("combos_NoCov_alltows.RDS")
combos_NoCov <- readRDS("combos_NoCov.RDS")
combos_WCov <- readRDS("combos_WCov.RDS")
finished_all_WCov2 <- readRDS("finished_all_WCov2.RDS")
finished_all_NoCov2 <- readRDS("finished_all_NoCov2.RDS")
finished_all_NoCov2_alltows <- readRDS("finished_all_NoCov2_alltows.RDS")

combosA=combos_WCov  #W_Cov
combosB=combos_NoCov  #No_Cov
combosC=combos_NoCov_alltows 

finished_allA = finished_all_WCov2
finished_allB = finished_all_NoCov2
finished_allC = finished_all_NoCov2_alltows

combosA$Covs="W_Cov"
combosB$Covs = "No_Cov"
combosC$Covs = "No_Cov_alltows"


scenario_numberA = seq(length(combosA$COMMON_NAME))
scenario_numberB = seq(length(combosB$COMMON_NAME))
scenario_numberC = seq(length(combosC$COMMON_NAME))

combosA = subset(combosA,scenario_numberA%in%finished_allA$scenario_number)
combosB = subset(combosB,scenario_numberB%in%finished_allB$scenario_number)
combosC = subset(combosC,scenario_numberC%in%finished_allC$scenario_number)

combos2=rbind(combosA,combosB,combosC)





#or combine FOUR combos (with and without covariates)
#reduce combos to those that finished
#if combining three, need to combined finished ones
combos_NoCov_alltows <- readRDS("combos_NoCov_alltows.RDS")
combos_NoCov <- readRDS("combos_NoCov.RDS")
combos_WCov <- readRDS("combos_WCov.RDS")
combos_WCov_X1 <- readRDS("combos_WCov_X1.RDS")

finished_all_WCov2 <- readRDS("finished_all_WCov2.RDS")
finished_all_WCov_X1_2 <- readRDS("finished_all_W_Cov_X1_2.RDS")
finished_all_NoCov2 <- readRDS("finished_all_NoCov2.RDS")
finished_all_NoCov2_alltows <- readRDS("finished_all_NoCov2_alltows.RDS")

combosA=combos_WCov  #W_Cov
combosB=combos_NoCov  #No_Cov
combosC=combos_NoCov_alltows 
combosD=combos_WCov_X1  #W_Cov_X1


finished_allA = finished_all_WCov2
finished_allB = finished_all_NoCov2
finished_allC = finished_all_NoCov2_alltows
finished_allD = finished_all_WCov_X1_2

combosA$Covs="W_Cov"
combosB$Covs = "No_Cov"
combosC$Covs = "No_Cov_alltows"
combosD$Covs="W_Cov_X1"


scenario_numberA = seq(length(combosA$COMMON_NAME))
scenario_numberB = seq(length(combosB$COMMON_NAME))
scenario_numberC = seq(length(combosC$COMMON_NAME))
scenario_numberD = seq(length(combosD$COMMON_NAME))

combosA = subset(combosA,scenario_numberA%in%finished_allA$scenario_number)
combosB = subset(combosB,scenario_numberB%in%finished_allB$scenario_number)
combosC = subset(combosC,scenario_numberC%in%finished_allC$scenario_number)
combosD = subset(combosD,scenario_numberD%in%finished_allD$scenario_number)

combos2=rbind(combosA,combosB,combosC,combosD)


###################################################################################
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


pdf(file="Plots_Comparing_EstimatesNEW.pdf")


#CNs=unique(combos$COMMON_NAME)[!unique(combos$COMMON_NAME)%in% c("DOGFISH, SPINY", "DOGFISH, SMOOTH")]
CNs = unique(combos2$COMMON_NAME)

run_time_info <- data.frame()

VAST_fit_unit <- list()
VAST_fit_all <- data.frame()

VAST_fit_cov <- list()

tow_data_decrease <- data.frame() #to store the change in number of tows for non covariate runs


for(CN in CNs){
  print(CN)
  
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
 
 
  
  VAST_fit_unit[[CN]] <- list()
  
   for(SA in SAs){ 
     
  seasons =  unique(subset(combos2,COMMON_NAME==CN&STOCK_ABBREV==SA))[["SEASON"]]
  seasons = unique(seasons)
  VAST_fit_unit[[CN]][[SA]] <- data.frame()
  SM_est_unit <- data.frame()
  

  
  for(season in seasons){
    
    
    
    covs =  unique(subset(combos2,COMMON_NAME==CN&STOCK_ABBREV==SA&SEASON==season))[["Covs"]]
   
    #pull out stratified mean estimate
    if(season=="SPRING"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="SPRING")}
    if(season=="FALL"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="FALL")}
    
    #add to overall SM est for unit
    SM_est_unit <- rbind(SM_est_unit,SM_est)
    
    for(cov in covs){
      
      print(cov)
      print(folders[folder_idx])
      print(season)  
      
      #calculate loss of tows in non-covariate runs
      if(cov == "No_Cov_alltows"){
        
        tow_data <- tow_data_species[[CN]][[SA]]

      
      #use up to 2019 because 2020 missing
      tow_data = subset(tow_data,YEAR<2020)
      
      #replace NA weight values with 0
      tow_data[is.na(tow_data[,"CATCH_WT_CAL"]),"CATCH_WT_CAL"]=0
      #range(tow_data$CATCH_WT_CAL)
      #if(season=="FALL"){tow_data_season = subset(tow_data,SEASON=="FALL")}
      tow_data=subset(tow_data,SEASON==season)
      
      tow_data <- tow_data %>%
        filter(substr(SURVEY, 1, 4) =="NMFS") 
      
      if(cov_type=="Temp_Hub"){tow_data_cov_only = subset(tow_data,!is.na(Temp_Hub))}
      if(cov_type=="BOT_TEMP"){tow_data_cov_only = subset(tow_data,!is.na(BOT_TEMP),SEASON==season)}
      
      row_change = nrow(tow_data)-nrow(tow_data_cov_only)
       row_change_pct = row_change/nrow(tow_data)
       tow_data_decrease = rbind(tow_data_decrease,c(CN,SA,season,cov_data_type,nrow(tow_data),row_change,row_change_pct))
       colnames(tow_data_decrease) = c("Species","stock area","season","cov type","total tows","tows missing cov info", "percent change in tows")
      }
      
      #pull out each value
      path = strsplit(folders[folder_idx],"/")
      cov_real= path[[1]][5]
      
      VAST_fit_csv = read.csv(paste0(getwd(),"/",folders[folder_idx],"/Index_wYearSeason.csv"), header=T)
      VAST_fit_csv$SEASON = VAST_fit_csv$Season
      VAST_fit_csv$scenario = folders[folder_idx]
      
      #add to overall vast dataframe for unit
      VAST_fit_unit[[CN]][[SA]] <- rbind(VAST_fit_unit[[CN]][[SA]],VAST_fit_csv)
      
      #read in parameter estimates
      load(paste0(getwd(),"/",folders[folder_idx],"/parameter_estimates.RData"))
      
      if(cov_real=="W_Cov"){
      #plot covariate response, if needed
      #print("PLOTTING COVARIATE RESPONSE")  
      #pull out vast estimate 
      VAST_fit = readRDS(paste0(getwd(),"/",folders[folder_idx],"/VAST_fit_.RDS"))
      covariate_data_full = VAST_fit$effects$covariate_data_full
      catchability_data_full = VAST_fit$effects$catchability_data_full

      VAST_fit_cov[[CN]][[SA]][[season]][["X2"]] = Effect.fit_model( VAST_fit,
                               focal.predictors = c("Temp_Est"),
                               which_formula = "X2",
                               xlevels = 100,
                               transformation = list(link=identity, inverse=identity) )
      
      #ADD IN LATER??
      # VAST_fit_cov[[CN]][[SA]][[season]][["X1"]] = Effect.fit_model( VAST_fit,
      #                          focal.predictors = c("Temp_Est"),
      #                          which_formula = "X1",
      #                          xlevels = 100,
      #                          transformation = list(link=identity, inverse=identity) )
      # 
      #plot response if it doesnt exist
      #if(!file.exists(paste0(getwd(),"/",folders[folder_idx],"/_cov_res_",CN,"_",SA,"_",unique(VAST_fit_csv$Season),"_",cov_real,".pdf"))){
      #pdf(file=paste(getwd(),"/",folders[folder_idx],"/_cov_res_",CN,"_",SA,"_",unique(VAST_fit_csv$Season),".pdf",sep=""))
     # plot(VAST_fit_cov[[CN]][[SA]][[season]])
    #dev.off()
     # }
      
      remove(VAST_fit)
      }
      
      if(cov_real=="W_Cov_X1"){
        #plot covariate response, if needed
        #print("PLOTTING COVARIATE RESPONSE")  
        #pull out vast estimate 
        VAST_fit = readRDS(paste0(getwd(),"/",folders[folder_idx],"/VAST_fit_.RDS"))
        covariate_data_full = VAST_fit$effects$covariate_data_full
        catchability_data_full = VAST_fit$effects$catchability_data_full
        
        VAST_fit_cov[[CN]][[SA]][[season]][["X1_X2"]][["X1"]] = Effect.fit_model( VAST_fit,
                                                                       focal.predictors = c("Temp_Est"),
                                                                       which_formula = "X1",
                                                                       xlevels = 100,
                                                                       transformation = list(link=identity, inverse=identity) )
        
        
        VAST_fit_cov[[CN]][[SA]][[season]][["X1_X2"]][["X2"]] = Effect.fit_model( VAST_fit,
                                                                                  focal.predictors = c("Temp_Est"),
                                                                                  which_formula = "X2",
                                                                                  xlevels = 100,
                                                                                  transformation = list(link=identity, inverse=identity) )
        
        
        #ADD IN LATER??
        # VAST_fit_cov[[CN]][[SA]][[season]][["X1"]] = Effect.fit_model( VAST_fit,
        #                          focal.predictors = c("Temp_Est"),
        #                          which_formula = "X1",
        #                          xlevels = 100,
        #                          transformation = list(link=identity, inverse=identity) )
        # 
        #plot response if it doesnt exist
        #if(!file.exists(paste0(getwd(),"/",folders[folder_idx],"/_cov_res_",CN,"_",SA,"_",unique(VAST_fit_csv$Season),"_",cov_real,".pdf"))){
        #pdf(file=paste(getwd(),"/",folders[folder_idx],"/_cov_res_",CN,"_",SA,"_",unique(VAST_fit_csv$Season),".pdf",sep=""))
        # plot(VAST_fit_cov[[CN]][[SA]][[season]])
        #dev.off()
        # }
        
        remove(VAST_fit)
      }
      
      hrsss=parameter_estimates$time_for_run[[1]]/(60*60)
      secc=as.numeric(parameter_estimates$time_for_run[[1]])
      newinfo1 = c(secc,hrsss,unlist(strsplit(folders[folder_idx],"/")))
      newinfo2=c(newinfo1,strsplit(newinfo1[9],"_")[[1]][2])
      run_time_info = rbind(run_time_info,newinfo2)
      
      folder_idx = folder_idx+1
    }
      
  VAST_fit_all <- rbind(VAST_fit_all,VAST_fit_unit[[CN]][[SA]])
  }
   



  #plot each relative to stratified mean
  print(ggplot() +

          #plot VAST estimate with covariates
          geom_errorbar(data=VAST_fit_unit[[CN]][[SA]],aes(x=Year,y=Estimate,group=Covariate,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = Covariate),width=.3) +
          geom_point(data=VAST_fit_unit[[CN]][[SA]],aes(x=Year,y=Estimate,group=Covariate, color = Covariate),size=2)+
          geom_line(data=VAST_fit_unit[[CN]][[SA]],aes(x=Year,y=Estimate,group=Covariate, color = Covariate))+

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
# print(ggplot(data=VAST_fit_unit[[CN]][[SA]]) +
#   #plot VAST estimate with covariates 
#   geom_errorbar(data=VAST_fit_unit[[CN]][[SA]],aes(x=Year,y=Estimate,group=Covariate,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = Covariate),width=.3) +
#   geom_point(data=VAST_fit_unit[[CN]][[SA]],aes(x=Year,y=Estimate,group=Covariate, color = Covariate),size=2)+
#   geom_line(data=VAST_fit_unit[[CN]][[SA]],aes(x=Year,y=Estimate,group=Covariate, color = Covariate))+
#   
#  facet_wrap(~ SEASON, ncol =1, scales = "free")+ 
#   theme(legend.position="bottom")+
#   labs(x="Year",y="Biomass", title = paste0(CN," ",SA," VAST ONLY"), color ="" )+
#   
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12),
#         title=element_text(size=8)))
# 
# 
# #plot SM only
# print(ggplot() +
#         
#         #plot stratified calculation data 
#         geom_errorbar(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON,ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
#         geom_point(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON))+
#         geom_line(data=SM_est_unit,aes(x=YEAR,y=mean.yr.absolute,group=SEASON))+
#         
#         
#         facet_wrap(~ SEASON, ncol =1, scales = "free") +
#         # labs(x="year",y="Biomass", title = paste(folder,"  SeV=",round(VAST_Model_error[[s]][[folder]][["spring"]],digits=2),
#         #                                          "  FC=", toString(FC_spring), 
#         #                                          "  SeSM=",round(SRS_Model_error[[s]][[folder]][["spring"]],digits=2),
#         #                                          "  FeV=",round(VAST_Model_error[[s]][[folder]][["fall"]],digits=2),
#         #                                          "  FC=", toString(FC_fall),
#         #                                          "  FeSM=",round(SRS_Model_error[[s]][[folder]][["fall"]],digits=2),sep=""), color ="" )
#         
#         labs(x="Year",y="Biomass", title = paste0(CN," ",SA,"SM ONLY"), color ="" )+
#         
#         theme(axis.text=element_text(size=12),
#               axis.title=element_text(size=12),
#               title=element_text(size=8)))


# #put legend on side so can read them  
# ggplot(data=VAST_fit_all) +
#   geom_point(aes(x=Year,y=Estimate,group=scenario,color=scenario),size=2)+
#   geom_line(aes(x=Year,y=Estimate, group=scenario,color=scenario))+
#   geom_errorbar(data=VAST_fit_unit[[CN]][[SA]],aes(x=Year,y=Estimate,group=scenario,ymin=Estimate-(1.96*Std..Error.for.Estimate), ymax=Estimate+(1.96*Std..Error.for.Estimate), color = scenario),width=.3) +
#   
#   facet_wrap(~ SEASON, ncol =1, scales = "free")+
#   labs(x="Year",y="Biomass", title = paste0(CN," ",SA," VAST ONLY"), color ="" )+
#   
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12),
#         title=element_text(size=8))
#   
}
}


dev.off()

write.csv(tow_data_decrease,"tow_data_decrease.csv")


#SAVE RUNTIME INFORMATION
colnames(run_time_info) = c("run time (sec)", "run time (hours)",newnames,"Knots")
write.csv(run_time_info,"run time info.csv")





#plotting covariate responses

pdf(file="Plotting_Covariate_Response.pdf")

for(CN in CNs){
 
 
  combosX = subset(combos2,COMMON_NAME==CN)
  
  SAs = unique(combosX$STOCK_ABBREV)
  

  

  for(SA in SAs){ 
    
    seasons =  unique(subset(combos2,COMMON_NAME==CN&STOCK_ABBREV==SA))[["SEASON"]]
    seasons = unique(seasons)
   
    for(season in seasons){
     
      
      #pull out stratified mean estimate
      if(season=="SPRING"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="SPRING")}
      if(season=="FALL"){SM_est = subset(strat_mean_species[[CN]][[SA]],SEASON=="FALL")}
      
     
      
#start multiple plots per page
library(VAST)
plist<-list()
if(!is.null(VAST_fit_cov[[CN]][[SA]][[season]])&length(VAST_fit_cov[[CN]][[SA]][[season]])>0){
  #par(mfrow = c(2, 2))  ## set the layout to be 3 by 3

     print(CN)
    print(SA)
    print(season)
    #ADD IN LATER??
    # p1=plot(VAST_fit_cov[[CN]][[SA]][[season]][["X1"]])
    # print(p1)
    
    p=try(plot(VAST_fit_cov[[CN]][[SA]][[season]][["X2"]],main=paste0(CN," ",SA," ",season)))
    try(print(p))
    # plist[[season]] = try(plot(VAST_fit_cov[[CN]][[SA]][[season]]))
    try(remove(p))
    
    p1=try(plot(VAST_fit_cov[[CN]][[SA]][[season]][["X1_X2"]][["X1"]],main=paste0(CN," ",SA," ",season)))
    try(print(p1))
    # plist[[season]] = try(plot(VAST_fit_cov[[CN]][[SA]][[season]]))
    try(remove(p1))
    
    p2=try(plot(VAST_fit_cov[[CN]][[SA]][[season]][["X1_X2"]][["X2"]],main=paste0(CN," ",SA," ",season)))
    try(print(p2))
    # plist[[season]] = try(plot(VAST_fit_cov[[CN]][[SA]][[season]]))
    try(remove(p2))
  }
}

    
  }
}

dev.off()











#SEND EMAIL TO ALERT ME ITS DONE




#send an email telling me its done
library(blastula)

setwd(orig.dir)

#note the date
date_time <- add_readable_time()


email <-
  compose_email(
    body = md(glue::glue(
      
      "PLOTS ARE DONE"
    )),
    footer = md(glue::glue("Email sent on {date_time}."))
  )

# Sending email by SMTP using a credentials file
email |>
  smtp_send(
    to = "benjamin.levy@noaa.gov",
    from = "blevy6@gmail.com",
    subject = "ALL ARE DONE",
    credentials = creds_file("smtp")
  )
























#PLOTTING RUN TIME INFO
#read in run_time_info.csv file
run_time_info=read.csv("run time info.csv")

#add run time to differen column for plotting
run_time_info$rt=run_time_info$run.time..hours.
#create color column based on covariate value
run_time_info$color = ifelse(run_time_info$Covs=="W_Cov","red","blue")

plot(as.numeric(run_time_info$Knots),as.numeric(run_time_info$rt),
     xlab="Number Knots",ylab="Run time (hours)", main = "Survey Temp",col=run_time_info$color)

legend(200, 3, legend=c("W_Cov", "No_Cov"), 
       fill = c("red","blue"))
