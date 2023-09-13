



#setup function to run in parrallel
run_VAST <- 
  function(scenario_num){

    
    #for flagging issues
    fail_reason = "N/A"
    
    #leave artifact to check whether vast_fit was created
    VAST_fail_reason <- "N/A"
    VAST_fit = scenario_num
    
    CN=combos[scenario_num,1]
    SA=combos[scenario_num,2]
    season=combos[scenario_num,3]
    j=combos[scenario_num,4]
    BC = ifelse(combos[scenario_num,5]==TRUE,"BCY","BCN")
    BC_TF = combos[scenario_num,5]
    KN = combos[scenario_num,6]
    KM = combos[scenario_num,7]
    
    #this might track progress. If not check here: https://cran.r-project.org/web/packages/pbapply/pbapply.pdf
    #  system(paste("echo 'now processing: ",common_names,stock_areas,seasons,obsmodels,"'"))
    library(VAST)
    library(dplyr)
    #orig.dir <- getwd()
    
    #print(orig.dir)
   # print(getwd())
   # print(orig.dir)
    setwd(orig.dir)
    #read in new make_extrapolation_info script to avoid if() error in R 4.2.3
    
    #1: load file
    source("Scripts/Prepare_NWA_Extrapolation_Data_Fn_BENS.R") #my edited version that skips most things
    #2: allow the function to call other hidden functions from FishStatsUtils
    environment(Prepare_NWA_Extrapolation_Data_Fn_BENS) <- asNamespace('FishStatsUtils')
    #3: replace make_extrapolation_info with make_extrapolation_info_BENS in the VAST package
    assignInNamespace("Prepare_NWA_Extrapolation_Data_Fn", Prepare_NWA_Extrapolation_Data_Fn_BENS, ns = "FishStatsUtils")
    
    #read in new check_fit script to save any values converging to 0
    
    #1: load file
    source("Scripts/check_fit_BENS.R") #my edited version that skips most things
    #2: allow the function to call other hidden functions from FishStatsUtils
    environment(check_fit_BENS) <- asNamespace('VAST')
    #3: replace make_extrapolation_info with make_extrapolation_info_BENS in the VAST package
    assignInNamespace("check_fit", check_fit_BENS, ns = "VAST")
    
    
        cov.dir <- ifelse(use_cov,"W_Cov_X1","No_Cov_alltows")
        
        tow_data <- tow_data_species[[CN]][[SA]]
        if(length(tow_data[,1])==0){print(paste0("stopped become of " ,CN,SA,season,j,BC))}
        
        nrows_before = nrow(subset(tow_data,SEASON==season))
        
        
        if(use_cov==TRUE){
          
        #use only Temp_Hub values
        if(cov_type=="Hub"){tow_data = subset(tow_data,!is.na(Temp_Hub))}
      
        #use only survey temp values
        if(cov_type=="Survey"){tow_data = subset(tow_data,!is.na(BOT_TEMP))}
        }
        
        
        #use up to 2019 because 2020 missing
        tow_data = subset(tow_data,YEAR<2020)
        
        #replace NA weight values with 0
        tow_data[is.na(tow_data[,"CATCH_WT_CAL"]),"CATCH_WT_CAL"]=0
        range(tow_data$CATCH_WT_CAL)
        #if(season=="FALL"){tow_data_season = subset(tow_data,SEASON=="FALL")}
        
        #print(tow_data_season)
         FALLL = subset(tow_data,SEASON=="FALL")
         SPRINGG = subset(tow_data,SEASON=="SPRING")
        seasonnn=subset(tow_data,SEASON==season)
        
        #if using number of samples based on number of tows, extract number of tows
    if(KN=="N_Samp"){
      NTows_S = length(unique(SPRINGG$TOWDATE))
      NYrs_S = length(unique(SPRINGG$YEAR))

      NTows_F = length(unique(FALLL$TOWDATE))
      NYrs_F = length(unique(FALLL$YEAR))

      KN = round(mean(c(NTows_S/NYrs_S,NTows_F/NYrs_F),na.rm = T))

      KN = max(100,KN)
      #increase number to test difference
      KN=2*KN
    }

            
        
            # OLD ONES FROM CHUCK ADAMS' PAPER WITH CHRIS AND LIZ
            # if(j == 1) {obsmodel <- c(2, 0); run <- 1}
            # if(j == 2) {obsmodel <- c(2, 1); run <- 3} #model selection
            # if(j == 3) {obsmodel <- c(1, 0); run <- 4}
            # if(j == 4) {obsmodel <- c(1, 1); run <- 5}
            
            # NEW ONES BASED ON CHRIS C'S RECOMMENDATION
            if(j == 1) {obsmodel <- c(2, 0); run <- 1}
            if(j == 2) {obsmodel <- c(2, 1); run <- 2} #model selection
            if(j == 3) {obsmodel <- c(4, 0); run <- 3}
            if(j == 4) {obsmodel <- c(4, 1); run <- 4}
            if(j == 5) {obsmodel <- c(9, 0); run <- 5}
            if(j == 6) {obsmodel <- c(9, 1); run <- 6}
            if(j == 7) {obsmodel <- c(10, 2); run <- 7}
            
            setwd(orig.dir)
            
            #create directory for model specific output
 
            dir.create(file.path(paste0(getwd(),"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season)),recursive = TRUE)
            setwd(paste0(getwd(),"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season))

 #if no seasonal data, flag it for email and result
            # print(nrow(FALLL))
            
        nrows_after = nrow(seasonnn)
            
print(paste0(nrows_after,"_",CN,"_",SA,"_",season,"_",j,"_",BC))

if(nrow(seasonnn)==0){fail_reason = "No seasonal data"
                      setwd(orig.dir)}
            
            
if(nrow(seasonnn)>0){
            
            # format for use in VAST
            seasonal_tows <- seasonnn %>%
              filter(substr(SURVEY, 1, 4) =="NMFS") %>%   #only NMFS surveys
              #filter(SEASON == as.character(season)) %>%
              # filter(YEAR >= 2009) %>%
              mutate(mycatch = CATCH_WT_CAL) %>%
              select(Year = YEAR,
                     Catch_KG = mycatch,
                     Lat = LATITUDE,
                     Lon = LONGITUDE,
                     Stratum = STRATUM) %>%
              mutate(Vessel = "missing",
                     AreaSwept_km2 =  0.0112*3.4299) #from Chis C- .0112 nm converted to kilometers^2
            
            
            Strata = unique(seasonal_tows$Stratum)
  
            
            example <- list(seasonal_tows)
            example$Region <- "northwest_atlantic"
            example$strata.limits <- data.frame(STRATA = Strata) #THESE ARE COD STRATA
            
          
            #directory name to create 
            d_name = paste(getwd(),"/",cov.dir,"/BC_",BC,"/KN_",KN,"/",KM,sep="")
            dir.create(file.path(d_name),recursive = TRUE)
            
            #run model out of seasonal folder
            #setwd(paste0(getwd(),"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season))
            
            #run out of lowest level folder
            setwd(d_name)
            
              #check to see whether vast fit already exists
            checkk=scenario_num
           #print(class(checkk))
           try({
             (checkk <- read.csv("Index_wYearSeason.csv"))
           })
            
            #if already a file, change vast_fail_reason
            if(!(class(checkk)%in%c("integer","numeric"))){VAST_fail_reason = "Exists" }
            
            #only continue if vast fit doesnt already exist
            if(class(checkk)%in%c("integer","numeric")){
              
          
              
            #obsmodel ==  c(2, 1)
            if(obsmodel[1] ==  2){
              
                          settings <- make_settings(n_x = KN,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                                        Region=example$Region,
                                        purpose="index2",
                                        strata.limits=example$strata.limits,
                                        #bias.correct=TRUE,
                                        #FieldConfig= FC1,
                                        #RhoConfig = RhoConfig,
                                        ObsModel = obsmodel,
                                        knot_method = KM,
                                        bias.correct = BC_TF,
                                        
                                        Version = "VAST_v14_0_1")
            }
            
            #obsmodel ==  c(10,2)
            if(obsmodel[1] ==  10){         
              
            FC1 = c("Omega1" = 0, "Epsilon1" =0, "Omega2" = 1, "Epsilon2" = 1) 
            RhoConfig = c("Beta1" = 3, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 4)
                  
            
            settings <- make_settings(n_x = KN,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                                      Region=example$Region,
                                      purpose="index2",
                                      strata.limits=example$strata.limits,
                                      #bias.correct=TRUE,
                                      FieldConfig= FC1,
                                      RhoConfig = RhoConfig,
                                      ObsModel = obsmodel,
                                      knot_method = KM,
                                      bias.correct = BC_TF,
                                      
                                      Version = "VAST_v14_0_1")}
            

              
            #if rerunning the ones with errors, adjust settings
              #only setup for obsmodel (2,1)

              if(!is.null(start_not_finished[["errors"]])){
                row_num = which(start_not_finished$scenario_number==scenario_num,arr.ind=TRUE)
                if(nchar(start_not_finished[["errors"]][row_num])<30){


                #pull out errors
                ers = strsplit(start_not_finished[["errors"]][[row_num]],"/")
                if(ers[[1]][1]==""){ers = ers[[1]][-1]} #delete blank place

                #change settings based on errors
                for(i in seq(length(ers))){
                  print(ers[[i]])
                  if(ers[[i]]=="epsilon2"){settings$FieldConfig[2,2]=0}
                  if(ers[[i]]=="epsilon1"){settings$FieldConfig[2,1]=0}
                  if(ers[[i]]=="omega1"){settings$FieldConfig[1,1]=0}
                  if(ers[[i]]=="omega2"){settings$FieldConfig[1,2]=0}
                  if(ers[[i]]=="100% encounter"){settings$FieldConfig[,1]=0}
                  

                }
                print(settings$FieldConfig)
                }
              }
              # 
              # if(!is.null(zro)){
              #   if(nchar(zro[scenario_num])<30){
              #     
              #     
              #     #pull out errors
              #     ers = strsplit(zro[[scenario_num]],"/")
              #     if(ers[[1]][1]==""){ers = ers[[1]][-1]} #delete blank place
              #     
              #     #change settings based on errors
              #     for(i in seq(length(ers))){
              #       print(ers[[i]])
              #       if(ers[[i]]=="epsilon2"){settings$FieldConfig[2,2]=0}
              #       if(ers[[i]]=="epsilon1"){settings$FieldConfig[2,1]=0}
              #       if(ers[[i]]=="omega1"){settings$FieldConfig[1,1]=0}
              #       if(ers[[i]]=="omega2"){settings$FieldConfig[1,2]=0}
              #       
              #     }
              #     print(settings$FieldConfig)
              #   }
              # }
            #avoids specific error related to running on server
            #settings$Version <- 'VAST_v12_0_0'
   
            
            ifelse(use_cov==TRUE,
                   
                   #IF USING COVARIATES
                   { 
                     
                     if(cov_type=="Hub"){covdata <- seasonnn[,c("LATITUDE","LONGITUDE","YEAR","Temp_Hub")]}
                     
                     if(cov_type=="Survey"){covdata <- seasonnn[,c("LATITUDE","LONGITUDE","YEAR","BOT_TEMP")]}
                     
                     colnames(covdata) <- c("Lat","Lon","Year","Temp_Est")
                     #covariate formula
                     X2_formula = ~ poly(Temp_Est, degree=2 ) 
                     X1_formula = ~ poly(Temp_Est, degree=2 ) 
                     
                     VAST_fit <- fit_model(settings = settings,
                                           "Lat_i"=as.numeric(seasonal_tows[,'Lat']), 
                                           "Lon_i"=as.numeric(seasonal_tows[,'Lon']), 
                                           "t_i"=as.numeric(seasonal_tows[,'Year']), 
                                           "c_iz"=as.numeric(rep(0,nrow(seasonal_tows))), 
                                           "b_i"=as.numeric(seasonal_tows[,'Catch_KG']), 
                                           "a_i"=as.numeric(seasonal_tows[,'AreaSwept_km2']),
                                           X1_formula = X1_formula,
                                           X2_formula = X2_formula,
                                           covariate_data = covdata,
                                           optimize_args=list("lower"=-Inf,"upper"=Inf))
                     
                     #grab directory where 2 knot files will be 
                     run_directory = getwd()
                     #set directory to plot there
                     setwd(d_name)
                     
                     #plot covariate response
                     # print("PLOTTING COVARIATE RESPONSE")
                     # pdf(file=paste(getwd(),"/_cov_res_",season,".pdf",sep=""))
                     # fittt = VAST_fit
                     # covariate_data_full = fittt$effects$covariate_data_full
                     # catchability_data_full = fittt$effects$catchability_data_full
                     # 
                     # 
                     # pred = Effect.fit_model( fittt,
                     #                          focal.predictors = c("Temp_Est"),
                     #                          which_formula = "X2",
                     #                          xlevels = 100,
                     #                          transformation = list(link=identity, inverse=identity) )
                     # plot(pred)
                     # 
                     # dev.off()
                     # 
                     # remove(fittt)
                     
                     
                   },
                   
                   #NOT USING COVARIATES
                   { 
                     
                     VAST_fit <- fit_model(settings = settings,
                                           "Lat_i"=as.numeric(seasonal_tows[,'Lat']), 
                                           "Lon_i"=as.numeric(seasonal_tows[,'Lon']), 
                                           "t_i"=as.numeric(seasonal_tows[,'Year']), 
                                           "c_iz"=as.numeric(rep(0,nrow(seasonal_tows))), 
                                           "b_i"=as.numeric(seasonal_tows[,'Catch_KG']), 
                                           "a_i"=as.numeric(seasonal_tows[,'AreaSwept_km2']),
                                           
                                           optimize_args=list("lower"=-Inf,"upper"=Inf))
                     
                     #grab directory where 2 knot files and settings file will be 
                     run_directory = getwd()
                     
                     #set directory to plot there
                     setwd(d_name)})
            
         
            
            if(!(class(VAST_fit) %in% c("integer","numeric"))){
          try(saveRDS(VAST_fit,file = paste(d_name,"/VAST_fit_.RDS",sep="")))
            }
            
            #shouldnt need this as is
            # file.rename(from= paste0(run_directory,"/settings.txt") 
            #             ,to =paste(d_name,"/settings.txt",sep=""))
            # 
            # file.rename(from= paste(run_directory,"/parameter_estimates.txt",sep="") 
            #             ,to =paste(d_name,"/parameter_estimates.txt",sep=""))
            # 
            # file.rename(from= paste(run_directory,"/parameter_estimates.RData",sep="") 
            #             ,to =paste(d_name,"/parameter_estimates.RData",sep=""))
            # 
            # file.rename(from= paste(run_directory,"/packageDescription.txt",sep="") 
            #             ,to =paste(d_name,"/packageDescription.txt",sep=""))
            # 
            
            #  plot_biomass_index(VAST_fit)
            
         try( plot(VAST_fit))
            
            #add years and season to index csv
            try(index_csv <- read.csv("Index.csv"))
            try( index_csv$Year <- as.numeric(VAST_fit$year_labels))
            try( index_csv$Season <- rep(season,length(index_csv$Time)))
            try( index_csv$Covariate <- rep(cov.dir,length(index_csv$Time)))
            
            
           try(write.csv(index_csv,"Index_wYearSeason.csv"))
            
}
}
            #send an email telling me its done
            library(blastula)

          setwd(orig.dir)

            #note the date
            date_time <- add_readable_time()


         #create message if it doesnt work
          if(class(VAST_fail_reason)=="character"){

            #      email <-
            #        compose_email(
            #          body = md(glue::glue(
            #            paste0(scenario_num," Scenario ",CN," ",SA," ",season," ",j," ",BC," ",KN," ",KM," fail reason ",fail_reason,"VAST fail ", VAST_fail_reason)
            #          )),
            #          footer = md(glue::glue("Email sent on {date_time}."))
            #        )
            # 
            #         # Sending email by SMTP using a credentials file
            # email |>
            #   smtp_send(
            #     to = "benjamin.levy@noaa.gov",
            #     from = "blevy6@gmail.com",
            #     subject = "Container run failed",
            #     credentials = creds_file("smtp")
            #   )

            finished_run = c("YES",fail_reason, VAST_fail_reason)
          }




            #create message if it does work
            if(!(class(VAST_fit) %in% c("integer","numeric"))){

              # email <-
              #   compose_email(
              #     body = md(glue::glue(
              #       paste0(scenario_num," Scenario ",CN," ",SA," ",season," ",j," ",BC," ",KN," ",KM,fail_reason,"VAST fail ", VAST_fail_reason)
              #     )),
              #     footer = md(glue::glue("Email sent on {date_time}."))
              #   )
              # 
              # # Sending email by SMTP using a credentials file
              # email |>
              #   smtp_send(
              #     to = "benjamin.levy@noaa.gov",
              #     from = "blevy6@gmail.com",
              #     subject = "Container run complete",
              #     credentials = creds_file("smtp")
              #   )
              finished_run = c("No",fail_reason, VAST_fail_reason)
            }



    return(list(scenario =  paste0(scenario_num," Scenario ",CN," ",SA," ",season," ",j," ",BC," ",KN," ",KM),outcome=finished_run,knots=KN,errors=traceback(),rows_before=nrows_before,rows_after=nrows_after))
        

            
            
            remove(VAST_fit)
            
            
         #   rm(list = ls())
            
            # .rs.restartR()
            
            

            
          }


