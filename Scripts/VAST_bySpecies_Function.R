



#setup function to run in parrallel
run_VAST <- 
  function(scenario_num){
    
    
    
    CN=combos[scenario_num,1]
    SA=combos[scenario_num,2]
    season=combos[scenario_num,3]
    j=combos[scenario_num,4]
    
    #this might track progress. If not check here: https://cran.r-project.org/web/packages/pbapply/pbapply.pdf
    #  system(paste("echo 'now processing: ",common_names,stock_areas,seasons,obsmodels,"'"))
    library(VAST)
    library(dplyr)
    orig.dir <- getwd()
    
    #read in new make_extrapolation_info script to avoid if() error in R 4.2.3
    
    #1: load file
    source("Scripts/Prepare_NWA_Extrapolation_Data_Fn_BENS.R") #my edited version that skips most things
    #2: allow the function to call other hidden functions from FishStatsUtils
    environment(Prepare_NWA_Extrapolation_Data_Fn_BENS) <- asNamespace('FishStatsUtils')
    #3: replace make_extrapolation_info with make_extrapolation_info_BENS in the VAST package
    assignInNamespace("Prepare_NWA_Extrapolation_Data_Fn", Prepare_NWA_Extrapolation_Data_Fn_BENS, ns = "FishStatsUtils")
    
    
    
        
        tow_data <- tow_data_species[[CN]][[SA]]
        #use only Temp_Hub values to start
        tow_data = subset(tow_data,!is.na(Temp_Hub))
        
        #use up to 2019 because 2020 missing
        tow_data = subset(tow_data,YEAR<2020)
        
        #replace NA weight values with 0
        tow_data[is.na(tow_data[,"CATCH_WT_CAL"]),"CATCH_WT_CAL"]=0
        range(tow_data$CATCH_WT_CAL)
        
        
        tow_data_season[["SPRING"]] = subset(tow_data,SEASON=="SPRING")
        tow_data_season[["FALL"]] = subset(tow_data,SEASON=="FALL")
        
    
            
            print(paste0(c(CN,SA,season,j)))
            
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
            dir.create(paste(getwd(),"/VAST_runs/",CN,sep=""))
            
            dir.create(paste(getwd(),"/VAST_runs/",CN,"/",SA,sep=""))
            
            
            dir.create(paste(getwd(),"/VAST_runs/",CN,"/",SA,"/obsmodel",j,sep=""))
            setwd((paste(getwd(),"/VAST_runs/",CN,"/",SA,"/obsmodel",j,sep="")))
            
            
            #run model out of seasonal folder
            dir.create(paste(getwd(),"/",season,sep=""))
            setwd(paste(getwd(),"/",season,sep=""))
            
            
            # format for use in VAST
            seasonal_tows <- tow_data_season[[season]] %>%
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
            
            
            
            
            
            FC1 = c("Omega1" = 0, "Epsilon1" =0, "Omega2" = 1, "Epsilon2" = 1) 
            RhoConfig = c("Beta1" = 3, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 4)
            
            
            
            
            settings <- make_settings(n_x = 500,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                                      Region=example$Region,
                                      purpose="index2",
                                      strata.limits=example$strata.limits,
                                      bias.correct=TRUE,
                                      FieldConfig= FC1,
                                      RhoConfig = RhoConfig,
                                      ObsModel = obsmodel,
                                      knot_method = "samples",
                                      
                                      Version = "VAST_v14_0_1")
            
            #avoids specific error related to running on server
            #settings$Version <- 'VAST_v12_0_0'
            
            
            
            
            
            ifelse(use_cov==TRUE,
                   
                   #IF USING COVARIATES
                   {dir.create(paste(getwd(),"/",cov.dir,sep=""))
                     
                     covdata <- tow_data_season[[season]][,c("LATITUDE","LONGITUDE","YEAR","Temp_Hub")]
                     colnames(covdata) <- c("Lat","Lon","Year","Temp_Est")
                     #covariate formula
                     X2_formula = ~ poly(Temp_Est, degree=2 ) 
                     VAST_fit <- fit_model(settings = settings,
                                           "Lat_i"=as.numeric(seasonal_tows[,'Lat']), 
                                           "Lon_i"=as.numeric(seasonal_tows[,'Lon']), 
                                           "t_i"=as.numeric(seasonal_tows[,'Year']), 
                                           "c_iz"=as.numeric(rep(0,nrow(seasonal_tows))), 
                                           "b_i"=as.numeric(seasonal_tows[,'Catch_KG']), 
                                           "a_i"=as.numeric(seasonal_tows[,'AreaSwept_km2']),
                                           #   X1_formula = X1_formula,
                                           X2_formula = X2_formula,
                                           covariate_data = covdata,
                                           optimize_args=list("lower"=-Inf,"upper"=Inf))
                     #set directory to plot there
                     setwd(paste(getwd(),"/",cov.dir,sep=""))
                     
                     #plot covariate response
                     print("PLOTTING COVARIATE RESPONSE")
                     pdf(file=paste(getwd(),"/_cov_res_",season,".pdf",sep=""))
                     fittt = VAST_fit
                     covariate_data_full = fittt$effects$covariate_data_full
                     catchability_data_full = fittt$effects$catchability_data_full
                     
                     
                     pred = Effect.fit_model( fittt,
                                              focal.predictors = c("Temp_Est"),
                                              which_formula = "X2",
                                              xlevels = 100,
                                              transformation = list(link=identity, inverse=identity) )
                     plot(pred)
                     
                     dev.off()
                     
                     remove(fittt)
                     
                     
                   },
                   
                   #NOT USING COVARIATES
                   {dir.create(paste(getwd(),"/",cov.dir,sep=""))
                     
                     VAST_fit <- fit_model(settings = settings,
                                           "Lat_i"=as.numeric(seasonal_tows[,'Lat']), 
                                           "Lon_i"=as.numeric(seasonal_tows[,'Lon']), 
                                           "t_i"=as.numeric(seasonal_tows[,'Year']), 
                                           "c_iz"=as.numeric(rep(0,nrow(seasonal_tows))), 
                                           "b_i"=as.numeric(seasonal_tows[,'Catch_KG']), 
                                           "a_i"=as.numeric(seasonal_tows[,'AreaSwept_km2']),
                                           
                                           optimize_args=list("lower"=-Inf,"upper"=Inf))
                     #set directory to plot there
                     setwd(paste(getwd(),"/",cov.dir,sep=""))})
            
            
            
            
            saveRDS(VAST_fit,file = paste(getwd(),"/VAST_fit_.RDS",sep=""))
            
            
            
            file.rename(from= paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/settings.txt",sep="") 
                        ,to =paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/",cov.dir,"/settings.txt",sep=""))
            
            file.rename(from= paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/parameter_estimates.txt",sep="") 
                        ,to =paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/",cov.dir,"/parameter_estimates.txt",sep=""))
            
            file.rename(from= paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/parameter_estimates.RData",sep="") 
                        ,to =paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/",cov.dir,"/parameter_estimates.RData",sep=""))
            
            file.rename(from= paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/packageDescription.txt",sep="") 
                        ,to =paste(orig.dir,"/VAST_runs/",CN,"/",SA,"/obsmodel",j,"/",season,"/",cov.dir,"/packageDescription.txt",sep=""))
            
            
            #  plot_biomass_index(VAST_fit)
            
            plot(VAST_fit)
            
            #add years and season to index csv
            index_csv = read.csv("Index.csv")
            index_csv$Year = as.numeric(VAST_fit$year_labels)
            index_csv$Season = rep(season,length(index_csv$Time))
            index_csv$Covariate = rep(cov.dir,length(index_csv$Time))
            
            
            write.csv(index_csv,"Index_wYearSeason.csv")
            
            remove(VAST_fit)
          }


