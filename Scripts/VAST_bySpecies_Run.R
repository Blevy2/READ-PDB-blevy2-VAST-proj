
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

##replace spiny dogfish and smooth dogfish UNIT with male and female
for(sp in c("DOGFISH, SMOOTH","DOGFISH, SPINY")){
  
  FM=tow_data_species[[sp]]$UNIT[tow_data_species[[sp]]$UNIT$SEX_TYPE=="FEMALE",]
  MA=tow_data_species[[sp]]$UNIT[tow_data_species[[sp]]$UNIT$SEX_TYPE=="MALE",]
  FM$STOCK_ABBREV="FEMALE"
  MA$STOCK_ABBREV="MALE"

  tow_data_species[[sp]]=list()
  tow_data_species[[sp]][["MALE"]] = MA
  tow_data_species[[sp]][["FEMALE"]] = FM
  
}

#replace all skates with unit stock
for(sp in names(tow_data_species)){
  if(substr(names(tow_data_species[sp])[1],1,5)=="SKATE"){
    temp = tow_data_species[[sp]]$UNIT
    tow_data_species[[sp]] = list()
    tow_data_species[[sp]]$UNIT = temp
}
}

#pull out specific tow data set to work with. Will eventually make this a loop
CN = "COD,ATLANTIC"
SA = "GBK"



#species names
common_names <- unique(tow_data_wTemp[c("COMMON_NAME","STOCK_ABBREV")])

#MAKE CORRECTIONS TO NAMES AND STOCK UNITS

##replace spiny dogfish and smooth dogfish UNIT with male and female
common_names[common_names$COMMON_NAME=="DOGFISH, SMOOTH",]$STOCK_ABBREV="MALE"
common_names=rbind(c("DOGFISH, SMOOTH","FEMALE"),common_names)
common_names[common_names$COMMON_NAME=="DOGFISH, SPINY",]$STOCK_ABBREV="MALE"
common_names=rbind(c("DOGFISH, SPINY","FEMALE"),common_names)
#replace all skates with unit stock
common_names$STOCK_ABBREV[substr(common_names$COMMON_NAME,1,5)=="SKATE"]="UNIT"
common_names=unique(common_names[c("COMMON_NAME","STOCK_ABBREV")])
#replace all scup with just UNIT
common_names$STOCK_ABBREV[substr(common_names$COMMON_NAME,1,4)=="SCUP"]="UNIT"
common_names=unique(common_names[c("COMMON_NAME","STOCK_ABBREV")])
#replace all summer flounder with single unit. also correct typo to delete
common_names$COMMON_NAME[substr(common_names$COMMON_NAME,9,12)==",SUM"]="FLOUNDER, SUMMER (FLUKE)"
common_names$STOCK_ABBREV[substr(common_names$COMMON_NAME,9,12)==", SU"]="UNIT"
common_names=unique(common_names[c("COMMON_NAME","STOCK_ABBREV")])
#replace all black sea bass with just UNIT
common_names$STOCK_ABBREV[substr(common_names$COMMON_NAME,1,4)=="SEA "]="UNIT"
common_names=unique(common_names[c("COMMON_NAME","STOCK_ABBREV")])
#remove GOMWF from winter flounder stock units
common_names$STOCK_ABBREV[substr(common_names$STOCK_ABBREV,1,5)=="GOMWF"]="GOM"
common_names=unique(common_names[c("COMMON_NAME","STOCK_ABBREV")])

#remove any you want to avoid
#common_names = subset(common_names, !(COMMON_NAME %in% c("SCUP","SEA BASS,BLACK","FLOUNDER,SUMMER (FLUKE)","FLOUNDER, SUMMER (FLUKE)","FLOUNDER, WINTER"))) 


#common_names <- c("HADDOCK")
#common_names <- unique(tow_data_wTemp$COMMON_NAME)

#stock areas 
stock_areas <- c("GBK")

#VAST obsmodels to use
obsmodels <- c(2)  #c(7,2)

#seasons to run
seasons <- c("FALL","SPRING")

#use bias correction in VAST?
bias_corr <- c("TRUE")

#use covariates in VAST?
use_cov <- FALSE

cov.dir <- ifelse(use_cov,"W_Cov","No_Cov")

#knot method
knot_methods = c("grid")  #c("samples","grid")

#knot number
#knots <- c(500,100)
knots=c("N_Samp")

orig.dir <- getwd()

tow_data_season <- list()

#####################################################################################
#list different combinations to use from unique common_names in table
combos=data.frame()
hold = common_names

temp1=data.frame()
temp2=data.frame()
temp3=data.frame()
temp4=data.frame()
temp5=data.frame()
for(season in seasons){temp=common_names
                      temp$SEASON=season
                      temp1=rbind(temp1,temp)}

for(j in obsmodels){temp=temp1
                    temp$obsmodel=j
                    temp2=rbind(temp2,temp)}

for(BC in bias_corr){temp=temp2
                    temp$bias_corr=BC
                    temp3=rbind(temp3,temp)}

for(KN in knots){temp=temp3
                temp$N_knots=KN
                temp4=rbind(temp4,temp)}

for(KM in knot_methods){temp=temp4
                        temp$knots_method=KM
                        temp5=rbind(temp5,temp)}

combos=temp5
###################################################################################################################


#####################################################################################
#list different combinations to use from your own combination
combos=data.frame()
idx=1

for(CN in common_names){
  
  
  
  for(SA in stock_areas){
    
    
    
    
    for(season in seasons){
      
      
      
      for(j in obsmodels){
        
        for(BC in bias_corr){
          
          for(KN in knots){
            
            for(KM in knot_methods){
        
        combos[idx,1] = CN
        combos[idx,2] = SA
        combos[idx,3] = season 
        combos[idx,4] = j
        combos[idx,5] = BC
        combos[idx,6] = KN
        combos[idx,7] = KM
        idx=idx+1
            }
          }
        }
      }
    }
  }
}
colnames(combos) = c("common_name","stock_area","season","obsmodel","bias_correct","knot_number","knot_method")

###################################################################################################################





#save combos for use later in plotting
saveRDS(combos,paste0(orig.dir,"/VAST_runs/",CN,"/combos.RDS"))

#run in parallel

library(parallel)

#1: load file
source("Scripts/VAST_bySpecies_Function.R") #my edited version that skips most things



nCoresToUse <- length(combos[,1])
nCoresToUse <- 18


cl <- parallel::makeCluster(nCoresToUse,revtunnel = TRUE, outfile = "", verbose = TRUE, master=nsl(Sys.info()['nodename'])) #options from https://stackoverflow.com/questions/41639929/r-cannot-makecluster-multinode-due-to-cannot-open-the-connection-error


result <- list()

number_scenarios <- length(combos[,1])

#varlist are variables to make available to each node
parallel::clusterExport(cl, varlist= c("combos","tow_data_species","tow_data_season","use_cov","cov.dir","bias_corr"),envir = .GlobalEnv)

result <- list()

result <- parallel::parLapply(cl,as.numeric(not_start_not_finish$scenario_number),run_VAST)

result <- parallel::parLapply(cl,1:number_scenarios,run_VAST_TEST)#for testing

parallel::stopCluster(cl)





 #send an email telling me its done
library(blastula)

setwd(orig.dir)

#note the date
date_time <- add_readable_time()


  email <-
    compose_email(
      body = md(glue::glue(
      
        "ALL ARE DONE"
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
  


