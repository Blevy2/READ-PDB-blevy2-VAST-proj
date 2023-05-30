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


miss_tmp_info <- data.frame()
  
for(rw in seq(length(Day_Year[,1]))){

  if(rw%%50==0){print(rw)}
  YR = Day_Year$YEAR[[rw]]
  DY = Day_Year$DayN[[rw]]
  
  #pull out all tows for a given day/year
  tow_data_byday <- subset(tow_data, (( (YEAR <2022) & (YEAR == YR) & (DayN == DY)))) #no data in some of 2022 days
  lat_lon_tows = cbind(tow_data_byday$LONGITUDE,tow_data_byday$LATITUDE)
  colnames(lat_lon_tows) <- c("Longitude","Latitude")

  #pull out temp data from same day/year
  try(temp_tmp <- tidync::tidync(filename) %>%
                      tidync::hyper_filter(year=year%in%c(YR), day=day%in%c(DY))%>%    #, lat=lat%in%c(lt), lon=lon%in%c(ln)) %>%
                      tidync::hyper_tibble() %>%
                      as.data.frame())
  
  #if no temp data exists, make fake data
  if(!exists("temp_tmp")){ 
  tows_temp = as.data.frame(rep("NoData",length(tow_data_byday$COMMON_NAME)))  
  colnames(tows_temp) = c("Temp_Hub")
  tow_data_byday = cbind(tow_data_byday, tows_temp)
  print(c(YR,DY))
 
  tmpp <- cbind(YR,DY)
  miss_tmp_info <- rbind(miss_tmp_info,tmpp)
  }
  
  
  #if temp data exists, add it to table
  if(exists("temp_tmp")){ 
  #turn temp data into raster
  temp_tmp_rass <- terra::rast(cbind(temp_tmp$lon,temp_tmp$lat,temp_tmp$bt_temp),type="xyz")
  #extract temp data at tow locations
  tows_temp = raster::extract(temp_tmp_rass,lat_lon_tows)
  colnames(tows_temp) = c("Temp_Hub")
  #add Hubert temp data to table
  tow_data_byday = cbind(tow_data_byday,tows_temp)
  remove(temp_tmp)
    #add temp data to tow data
  tow_data_wTemp <- rbind(tow_data_wTemp,tow_data_byday)  }
  
  # #plot things to verify 
  # pts = terra::vect(lat_lon_tows) #survey locations
  # try(terra::plot(temp_tmp_rass)) #plot temperature data, if it exists
  # terra::plot(pts,add=T) #plot points on top
  


  
}




#save in table if needed
#saveRDS(tow_data_wTemp,file="C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-VAST-proj/tow_data_wTemp.RDS")


#read in table if needed
#tow_data_wTemp <- readRDS("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-VAST-proj/tow_data_wTemp.RDS")




# #replace NA with -99
# tow_data_wTemp$BOT_TEMP[is.na(tow_data_wTemp$BOT_TEMP)] = -99999
# range(tow_data_wTemp$BOT_TEMP)
# 
# 
# #how many NA values for huberts temp data?
# sum(is.na(tow_data_wTemp$Temp_Hub)) #141,712 out of 804K
# #how many NoData values for huberts temp
# sum(tow_data_wTemp$Temp_Hub=="NoData",na.rm=T ) #14,942
# #how many NaN values for huberts temp
# sum(tow_data_wTemp$Temp_Hub=="NaN",na.rm=T) #11,724
# 
# #replace NA with 99
# tow_data_wTemp$Temp_Hub[is.na(tow_data_wTemp$Temp_Hub)] = 99
# #replace "NoData with 999
# tow_data_wTemp$Temp_Hub[tow_data_wTemp$Temp_Hub=="NoData"] = 99999
# #replace "NaN" with 9999
# tow_data_wTemp$Temp_Hub[tow_data_wTemp$Temp_Hub=="NaN"] = 99999999
# 
# range(tow_data_wTemp$Temp_Hub)




#split it into table with just bottom_temp data, just hubert data, neither, and both (for difference)
tow_data_jHubert <- subset(tow_data_wTemp, (Temp_Hub>-5) & (is.na(BOT_TEMP) ) )
tow_data_jBotTem <- subset(tow_data_wTemp, (is.na(Temp_Hub) & (BOT_TEMP>-5) ) )
tow_data_NoTempData <- subset(tow_data_wTemp, (is.na(Temp_Hub)) & (is.na(BOT_TEMP) ) )
tow_data_bothTemp <- subset(tow_data_wTemp, (!is.na(Temp_Hub)) & (!is.na(BOT_TEMP)))


#add column to table that is difference in temp estimates
#tow_data_wTemp$Temp_Diff = as.numeric(tow_data_wTemp$BOT_TEMP) - as.numeric(tow_data_wTemp$Temp_Hub)
#range(tow_data_wTemp$Temp_Diff)
tow_data_bothTemp$Temp_Diff = as.numeric(tow_data_bothTemp$BOT_TEMP) - as.numeric(tow_data_bothTemp$Temp_Hub)

range(tow_data_bothTemp$Temp_Diff)
mean(tow_data_bothTemp$Temp_Diff)
sd(tow_data_bothTemp$Temp_Diff)



#plot spatial difference in temperature estimates  
library(ggplot2)


pdf(file="plotting differences in temperature estimates.pdf")
 
# #build point raster with missing values and plot it
# ms_lat = tow_data_wTemp$LATITUDE[tow_data_wTemp$Temp_Diff< -20]
# ms_lon = tow_data_wTemp$LONGITUDE[tow_data_wTemp$Temp_Diff< -20]
# ms_diff = tow_data_wTemp$Temp_Diff[tow_data_wTemp$Temp_Diff< -20]
# ms_yrs = tow_data_wTemp$YEAR[tow_data_wTemp$Temp_Dif< -20]
#missing_tmp_rass <- terra::vect(cbind(ms_lon,ms_lat),atts=as.data.frame(ms_diff))

#plot missing value diff
# print(ggplot(data=as.data.frame(ms_diff),aes(x=ms_lon,y=ms_lat)) +
#   geom_point(aes(colour=abs(ms_diff)))+
#   scale_colour_gradient(low = "yellow", high = "red")+
#   ggtitle(paste("Missing values", " N = ", length(ms_lat))))

#both missing
print(ggplot(data=as.data.frame(tow_data_NoTempData),aes(x=LATITUDE,y=LONGITUDE)) +
  geom_point(aes(colour=factor(YEAR)))+
  ggtitle(paste("Missing BOTH Temp Values", " N = ", length(tow_data_NoTempData$LATITUDE), "~",round(100*length(tow_data_NoTempData$LATITUDE)/length(tow_data_wTemp$LATITUDE),0),"% of all entries")))

#just hubert (bot_temp missing)
print(ggplot(data=as.data.frame(tow_data_jHubert),aes(x=LATITUDE,y=LONGITUDE)) +
        geom_point(aes(colour=factor(YEAR)))+
        ggtitle(paste("Just Hubert (no bot_temp)", " N = ", length(tow_data_jHubert$LATITUDE), "~",round(100*length(tow_data_jHubert$LATITUDE)/length(tow_data_wTemp$LATITUDE),0),"% of all entries")))

#just bot_temp (hubert missing)
print(ggplot(data=as.data.frame(tow_data_jBotTem),aes(x=LATITUDE,y=LONGITUDE)) +
        geom_point(aes(colour=factor(YEAR)))+
        ggtitle(paste("Just Bot_Temp (no hubert data)", " N = ", length(tow_data_jBotTem$LATITUDE),"~",round(100*length(tow_data_jBotTem$LATITUDE)/length(tow_data_wTemp$LATITUDE),0),"% of all entries")))

# #plot years
# print(ggplot(data=as.data.frame(ms_yrs),aes(x=ms_lon,y=ms_lat)) +
#         geom_point(aes(colour=ms_yrs))+
#         scale_colour_gradient(low = "yellow", high = "red")+
#         ggtitle(paste("Missing values", " N = ", length(ms_lat))))
      
#build point raster with differences and plot it
temp_threshs = c(0.5,1,2,3,5,7)
ct=1
for(thresh in temp_threshs){
  ifelse(ct==length(temp_threshs),
{
  dif_lat = tow_data_bothTemp$LATITUDE[abs(tow_data_bothTemp$Temp_Diff)> thresh]
dif_lon = tow_data_bothTemp$LONGITUDE[abs(tow_data_bothTemp$Temp_Diff)> thresh]
dif_diff = tow_data_bothTemp$Temp_Diff[abs(tow_data_bothTemp$Temp_Diff)> thresh]
dif_yrs = tow_data_bothTemp$YEAR[abs(tow_data_bothTemp$Temp_Diff)> thresh]
},
{
dif_lat = tow_data_bothTemp$LATITUDE[(abs(tow_data_bothTemp$Temp_Diff)> thresh) & (abs(tow_data_bothTemp$Temp_Diff)< temp_threshs[ct+1])]
dif_lon = tow_data_bothTemp$LONGITUDE[(abs(tow_data_bothTemp$Temp_Diff)> thresh) & (abs(tow_data_bothTemp$Temp_Diff)< temp_threshs[ct+1])]
dif_diff = tow_data_bothTemp$Temp_Diff[(abs(tow_data_bothTemp$Temp_Diff)> thresh) & (abs(tow_data_bothTemp$Temp_Diff)< temp_threshs[ct+1])]
dif_yrs = tow_data_bothTemp$YEAR[(abs(tow_data_bothTemp$Temp_Diff)> thresh) & (abs(tow_data_bothTemp$Temp_Diff)< temp_threshs[ct+1])]
}
)
  
  


#plot difference as color
print(ggplot(data=as.data.frame(dif_diff),aes(x=dif_lon,y=dif_lat)) +
  geom_point(aes(colour=abs(dif_diff))) +
  scale_colour_gradient(low = "yellow", high = "red")+
    ggtitle(paste("temp difference > ",thresh, " N = ", length(dif_lat), "~", round(100*length(dif_lat)/length(tow_data_bothTemp$LATITUDE),0),"% of data w both temp")))

#plot year as color
print(ggplot(data=as.data.frame(dif_yrs),aes(x=dif_lon,y=dif_lat)) +
        geom_point(aes(colour=factor(dif_yrs)))+
        ggtitle(paste("temp difference > ",thresh, " N = ", length(dif_lat), "~", round(100*length(dif_lat)/length(tow_data_bothTemp$LATITUDE),0),"% of data w both temp"))) 

ct=ct+1
}

dev.off()










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
