#this function reads in tow data with lat/lon information, a column for BOttom temp and a column for "Temp_Hub" and plots 
#missing values from each data set as well as the difference between estimates when both values are included


plot_tow_NA_data <- function(TD=NA){
  
  #split it into table with just bottom_temp data, just hubert data, neither, and both (for difference)
  tow_data_jHubert <- subset(TD, (Temp_Hub>-5) & (is.na(BOT_TEMP) ) )
  tow_data_jBotTem <- subset(TD, (is.na(Temp_Hub) & (BOT_TEMP>-5) ) )
  tow_data_NoTempData <- subset(TD, (is.na(Temp_Hub)) & (is.na(BOT_TEMP) ) )
  tow_data_bothTemp <- subset(TD, (!is.na(Temp_Hub)) & (!is.na(BOT_TEMP)))
  
  
  #add column to table that is difference in temp estimates
  #tow_data_wTemp$Temp_Diff = as.numeric(tow_data_wTemp$BOT_TEMP) - as.numeric(tow_data_wTemp$Temp_Hub)
  #range(tow_data_wTemp$Temp_Diff)
  tow_data_bothTemp$Temp_Diff = as.numeric(tow_data_bothTemp$BOT_TEMP) - as.numeric(tow_data_bothTemp$Temp_Hub)
  
  range(tow_data_bothTemp$Temp_Diff)
  mean(tow_data_bothTemp$Temp_Diff)
  sd(tow_data_bothTemp$Temp_Diff)
  
  
  
  
  #both missing
  print(ggplot(data=as.data.frame(tow_data_NoTempData),aes(x=LONGITUDE,y=LATITUDE)) +
          geom_point(aes(colour=factor(YEAR)))+
          ggtitle(paste("Missing BOTH Temp Values", " N = ", length(tow_data_NoTempData$LATITUDE), "~",round(100*length(tow_data_NoTempData$LATITUDE)/length(tow_data_wTemp$LATITUDE),0),"% of all entries")))
  
  #just hubert (bot_temp missing)
  print(ggplot(data=as.data.frame(tow_data_jHubert),aes(x=LONGITUDE,y=LATITUDE)) +
          geom_point(aes(colour=factor(YEAR)))+
          ggtitle(paste("Just Hubert (no bot_temp)", " N = ", length(tow_data_jHubert$LATITUDE), "~",round(100*length(tow_data_jHubert$LATITUDE)/length(tow_data_wTemp$LATITUDE),0),"% of all entries")))
  
  #just bot_temp (hubert missing)
  print(ggplot(data=as.data.frame(tow_data_jBotTem),aes(x=LONGITUDE,y=LATITUDE)) +
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
}