
library(VAST)
library(dplyr)



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
common_names <- c("COD,ATLANTIC")


#stock areas 
stock_areas <- c("GBK")

#VAST obsmodels to use
obsmodels <- c(7)

#seasons to run
seasons <- c("FALL")

#use covariates in VAST?
use_cov <- FALSE

tow_data_season <- list()

for(CN in common_names){

for(SA in stock_areas){
  
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




for(season in seasons){



for(j in obsmodels){
  
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
  
  
  
  #create directory for model specific output
dir.create(paste(getwd(),"/VAST_runs/",CN,"_",SA,sep=""))

dir.create(paste(getwd(),"/VAST_runs/",CN,"_",SA,"/obsmodel",j,sep=""))
setwd((paste(getwd(),"/VAST_runs/",CN,"_",SA,"/obsmodel",j,sep="")))
  




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
       
       #if using covariates
{covdata <- tow_data_season[[season]][,c("LATITUDE","LONGITUDE","YEAR","Temp_Hub")]
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
                            optimize_args=list("lower"=-Inf,"upper"=Inf))},

#not using covariates
{VAST_fit <- fit_model(settings = settings,
                        "Lat_i"=as.numeric(seasonal_tows[,'Lat']), 
                        "Lon_i"=as.numeric(seasonal_tows[,'Lon']), 
                        "t_i"=as.numeric(seasonal_tows[,'Year']), 
                        "c_iz"=as.numeric(rep(0,nrow(seasonal_tows))), 
                        "b_i"=as.numeric(seasonal_tows[,'Catch_KG']), 
                        "a_i"=as.numeric(seasonal_tows[,'AreaSwept_km2']),
                      
                        optimize_args=list("lower"=-Inf,"upper"=Inf))}
)


#create directory for season specific output
dir.create(paste(getwd(),"/",season,sep=""))
setwd(paste(getwd(),"/",season,sep=""))


saveRDS(VAST_fit,file = paste(getwd(),"/VAST_fit_.RDS",season,sep=""))

#  plot_biomass_index(VAST_fit)

plot(VAST_fit)


#add years to index csv
index_csv = read.csv("Index.csv")
index_csv$Year = as.numeric(VAST_fit$year_labels)

#add season once its in a loop?!?!?!?!

write.csv(index_csv,"Index_wYear.csv")


}
}
}
}
