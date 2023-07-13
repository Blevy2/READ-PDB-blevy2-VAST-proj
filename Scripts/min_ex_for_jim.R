

library(VAST)
library(dplyr)


tow_data <- readRDS("tow_data_bluefin.RDS")


tow_data_season[["FALL"]] = subset(tow_data,SEASON=="FALL")



# format for use in VAST
seasonal_tows <- tow_data_season[["FALL"]] %>%
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
example$strata.limits <- data.frame(c(3308,3410,3400)) #THESE ARE COD STRATA


settings <- make_settings(n_x = 100,  #NEED ENOUGH KNOTS OR WILL HAVE ISSUES WITH PARAMETER FITTING
                          Region=example$Region,
                          purpose="index2",
                          strata.limits=example$strata.limits,
                          ObsModel = c(2,1),
                          knot_method = "grid",
                          bias.correct = TRUE,
                          
                          Version = "VAST_v14_0_1")

#fails only when strata.limits is a data.frame!!

make_extrapolation_info( Region="northwest_atlantic", strata.limits = c(3308,3410,3400) )

                         