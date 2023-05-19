#reading in excel file for all tows and parsing it by species

library(tidync)


tow_data = read.csv("Tow_by_tow_survey_data_v_gis_o_20230112.csv")
#add column with day number
DateFormat <- "%m/%d/%Y %H:%M %p"
tow_data<-transform(tow_data, DayN=as.numeric(strftime(as.Date(TOWDATE, format=DateFormat), '%j')))

#some DayN have NA values because the dateformat is different. Fix them
DateFormat2 <- "%m/%d/%Y"
for(rw in seq(length(tow_data$DayN))){
  if(is.na(tow_data$DayN[[rw]])){
    #if an NA value exists use other date format
    tow_data$DayN[[rw]] <- as.numeric(strftime(as.Date(tow_data$TOWDATE[[rw]], format=DateFormat2), '%j'))
  }
}



#find list of unique species names
species <- unique(tow_data$COMMON_NAME)
species <- unique(tow_data$SPECIES_ITIS)


#check unique values in multiple columns
x <- unique(tow_data[c("COMMON_NAME","SPECIES_ITIS")])


xx <- unique(tow_data[c("COMMON_NAME","SPECIES_ITIS","STOCK_ABBREV")])




#how many NA values for Catch_wt
#530K/8940k
sum(is.na(tow_data$CATCH_WT_CAL))
range(tow_data$CATCH_WT_CAL,na.rm=T) #0 to 9584





#THIS LOOP ADDS TEMPERATURE VALUES TO SAMPLES FOR EACH UNIQUE DAY IN THE SURVEY


#single days sampled each year. use this to collect temperature data
Day_Year <- unique(tow_data[c("YEAR","DayN")])

filename = "E:\\duPontavice_bottom_temp_data\\bottom_temp_combined_product_1959_2022_v1.nc"

tow_data_byday <- data.frame()
tow_data_wTemp <- data.frame()

for(rw in seq(length(Day_Year$DayN))){
  if(rw%%50==0){print(rw)}
  YR = Day_Year$YEAR[[rw]]
  DY = Day_Year$DayN[[rw]]
  
  #pull out all tows for a given day/year
  tow_data_byday <- subset(tow_data, (YEAR == YR & DayN == DY))
  lat_lon_tows = cbind(tow_data_byday$LONGITUDE,tow_data_byday$LATITUDE)

  #pull out temp data from same day/year
  try(temp_tmp <- tidync::tidync(filename) %>%
                      tidync::hyper_filter(year=year%in%c(YR), day=day%in%c(DY))%>%    #, lat=lat%in%c(lt), lon=lon%in%c(ln)) %>%
                      tidync::hyper_tibble() %>%
                      as.data.frame())
  
  #if temp data exists, add it to table
  if(exists("temp_tmp")){ 
  #turn temp data into raster
  temp_tmp_rass <- terra::rast(cbind(temp_tmp$lon,temp_tmp$lat,temp_tmp$bt_temp),type="xyz")
  #extract temp data at tow locations
  tows_temp = raster::extract(temp_tmp_rass,lat_lon_tows)
  colnames(tows_temp) = c("Temp_Hub")
  #add Hubert temp data to table
  tow_data_byday = cbind(tow_data_byday,tows_temp)
  remove(temp_tmp)}
  
  #if no temp data exists, make fake data
    if(!exists("temp_tmp")){ 
    tow_data_byday = rep("NoData",length(tow_data_byday$COMMON_NAME))
    print(c(YR,DY))}
  
  
  #add temp data to tow data
  tow_data_wTemp <- rbind(tow_data_wTemp,tow_data_byday)
  
  
  
}



samp_ras <- terra::rast(cbind(tow_data_byday$LONGITUDE,tow_data_byday$LATITUDE))
terra::plot(samp_ras)


testt=raster::extract(rass,cbind(tow_data_byday$LONGITUDE,tow_data_byday$LATITUDE))

#turn temp data into raster
rass <- terra::rast(cbind(temp_tmp$lon,temp_tmp$lat,temp_tmp$bt_temp),type="xyz")
#plot temp data
terra::plot(rass)






#THIS LOOP BREAKS UP THE TOW DATA BY SPECIES
names <- xx #start with this one

tow_data_species <- list()
for(rw in seq(length(names$COMMON_NAME))){
  
  CN = names$COMMON_NAME[[rw]] #common name
  SI = as.character(names$SPECIES_ITIS[[rw]]) #species_itis
  SA = names$STOCK_ABBREV[[rw]] #stock_abbrev
  
  tow_data_species[[CN]][[SI]][[SA]] <- subset(tow_data, (COMMON_NAME == CN & SPECIES_ITIS == SI & STOCK_ABBREV == SA))
  
  
}



































##accesssing the temperature data

#BELOW SNIP IS FROM HUBERT FOR ACCESSING SPECIFIC DATA BY DAY AND YEAR

library(tidync)

filename = "E:\\duPontavice_bottom_temp_data\\bottom_temp_combined_product_1959_2022_v1.nc"


data <- tidync::tidync(filename) %>%
  tidync::hyper_filter(year=year%in%(2020:2021), day=day%in%(c((0:104),(200:999)))) %>%
  tidync::hyper_tibble() %>%
  as.data.frame() 


#metadata info:
library(ncdf4)
nc_open(filename)
