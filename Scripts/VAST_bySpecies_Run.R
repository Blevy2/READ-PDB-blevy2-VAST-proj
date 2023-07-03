
library(VAST)
library(dplyr)



#read in new make_extrapolation_info script to avoid if() error in R 4.2.3

# #1: load file
# source("Scripts/make_extrapolation_info_BENS.R") #my edited version that skips most things
# #2: allow the function to call other hidden functions from VAST 
# environment(make_extrapolation_info_BENS) <- asNamespace('VAST')
# #3: replace make_extrapolation_info with make_extrapolation_info_BENS in the VAST package
# assignInNamespace("make_extrapolation_info", make_extrapolation_info_BENS, ns = "VAST")
# 
# #1: load file
# source("Scripts/fit_model_BENS.R") #my edited version that skips most things
# #2: allow the function to call other hidden functions from VAST 
# environment(fit_model_BENS) <- asNamespace('VAST') 
# environment(fit_model_BENS) <- asNamespace('FishStatsUtils')
# #3: replace make_extrapolation_info with make_extrapolation_info_BENS in the VAST package
# assignInNamespace("fit_model", fit_model_BENS, ns = "FishStatsUtils")

# #1: load file
# source("Scripts/Prepare_NWA_Extrapolation_Data_Fn_BENS.R") #my edited version that skips most things
# #2: allow the function to call other hidden functions from FishStatsUtils
# environment(Prepare_NWA_Extrapolation_Data_Fn_BENS) <- asNamespace('FishStatsUtils')
# #3: replace make_extrapolation_info with make_extrapolation_info_BENS in the VAST package
# assignInNamespace("Prepare_NWA_Extrapolation_Data_Fn", Prepare_NWA_Extrapolation_Data_Fn_BENS, ns = "FishStatsUtils")
# 


#Read in tow data that has both temperature data

tow_data_wTemp <- readRDS("tow_data_wTemp.RDS")



#THIS LOOP BREAKS UP THE TOW DATA BY SPECIES

# tow_data_species <- list()
# for(rw in seq(length(S_names$COMMON_NAME))){
#   
#   CN = S_names$COMMON_NAME[[rw]] #common name
#   SI = as.character(S_names$SPECIES_ITIS[[rw]]) #species_itis
#   SA = S_names$STOCK_ABBREV[[rw]] #stock_abbrev
#   
#   tow_data_species[[CN]][[SA]] <- subset(tow_data_wTemp, (COMMON_NAME == CN & SPECIES_ITIS == SI & STOCK_ABBREV == SA))
#   
#   
# }
# 

#saveRDS(tow_data_species,file="tow_data_species.RDS")


tow_data_species <- readRDS("tow_data_species.RDS")


#pull out specific tow data set to work with. Will eventually make this a loop

CN = "COD,ATLANTIC"
SA = "GBK"



#species names
common_names <- unique(tow_data_wTemp[c("COMMON_NAME","SPECIES_ITIS","STOCK_ABBREV")])
common_names <- c("HADDOCK")


#stock areas 
stock_areas <- c("GOM","GBK")

#VAST obsmodels to use
obsmodels <- c(7)

#seasons to run
seasons <- c("FALL","SPRING")

#use covariates in VAST?
use_cov <- TRUE

cov.dir <- ifelse(use_cov,"W_Cov","No_Cov")

orig.dir <- getwd()

tow_data_season <- list()

#1: load file
source("Scripts/VAST_bySpecies_Function.R") #my edited version that skips most things


#list different combinations to use
combos=data.frame()
idx=1

for(CN in common_names){
  
  
  
  for(SA in stock_areas){
    
    
    
    
    for(season in seasons){
      
      
      
      for(j in obsmodels){
        
        combos[idx,1] = CN
        combos[idx,2] = SA
        combos[idx,3] = season 
        combos[idx,4] = j
        idx=idx+1
        
      }
    }
  }
}

colnames(combos) = c("common_name","stock_area","season","obsmodel")



#run in parallel

library(parallel)


nCoresToUse <- length(combos[,1])



cl <- parallel::makeCluster(nCoresToUse,revtunnel = TRUE, outfile = "", verbose = TRUE, master=nsl(Sys.info()['nodename'])) #options from https://stackoverflow.com/questions/41639929/r-cannot-makecluster-multinode-due-to-cannot-open-the-connection-error


result <- list()

number_scenarios <- length(combos[,1])

#varlist are variables to make available to each node
parallel::clusterExport(cl, varlist= c("combos","tow_data_species","tow_data_season","use_cov","cov.dir"),envir = .GlobalEnv)


result <- parallel::parLapply(cl,1:number_scenarios,run_VAST)
parallel::stopCluster(cl)



