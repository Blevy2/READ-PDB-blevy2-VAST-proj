# function to calculate the SRS mean for NEFSC Survey
# liz brooks  
# 26 may 2021

# edited by Ben Levy January 2022


srs_survey <- function(df, sa, str, ta=0.01, sppname = NULL  )  {
  
  # df is dataframe of survey data (tow by tow)  
  # sa is vector of stratum area
  # str is the matrix of strata info
  # ta is the tow swept area (default 0.01 square nautical miles)


  # add column to sa for number of tow units per stratum
  sa <- sa %>%
    mutate(STRATUM_UNITS = STRATUM_AREA/ta)
  
  if (is.null(str)) {  #grab unique strata by season
    
    strata <- df %>%
      filter(substr(SURVEY, 1, 4) =="NMFS") %>% #DONT THINK I NEED THIS
      dplyr::select(SEASON, STRATUM) %>%
      group_by(SEASON) %>%
      distinct(STRATUM) %>%
      nest()
    
  } # end if check for null strata vector
  
 
  #make stratum both integers so they are same type in following block
  sa$STRATUM <- as.integer(sa$STRATUM)
  df$STRATUM <- as.integer(df$STRATUM)
  
  # calculate total area for stock-specific strata by season
  tmp.total.area <- df %>%
    filter(substr(SURVEY, 1, 4) =="NMFS") %>%  #STILL DONT THINK I NEED THIS
    dplyr::select(SEASON, STRATUM) %>%
    group_by(SEASON) %>%
    distinct(STRATUM) %>% 
    left_join(sa, by="STRATUM") %>%
    dplyr::summarise(Total=sum(STRATUM_AREA))
  
  #DAY COLUMN IN DF IS CAUSING ISSUES AND NOT NEEDED
  #if(!is.null(df$day)){df <- subset(df,select = -c(day))}

  tmp.tibble <- df %>%
    filter(substr(SURVEY, 1, 4) =="NMFS") %>% #STILL DONT THINK I NEED THIS
    group_by(SEASON) %>%
    left_join(sa) %>%
    replace(is.na(.), 0) %>%
    pivot_longer(cols=c("CATCH_WT_CAL"), values_to="OBS_VALUE") %>%  #changing catchwt to obs_value
    dplyr::select(YEAR, SEASON, STRATUM, TOW, OBS_VALUE, STRATUM_AREA)

# Calculate null survey
surv.ind.str <- tmp.tibble %>%
  dplyr::select(YEAR, SEASON, STRATUM,  STRATUM_AREA, TOW, OBS_VALUE ) %>%
  dplyr::arrange(as.numeric(YEAR), SEASON, STRATUM, TOW) %>%
  group_by(YEAR, SEASON, STRATUM )  %>% #INDEX GOING DOWN TO STRATA
  dplyr::summarise(mean.str = mean(as.integer(OBS_VALUE)), var.samp.str=var(OBS_VALUE, na.rm=T), ntows.str = n() ) %>%
  replace(is.na(.), 0) 

surv.ind.yr <- surv.ind.str %>%
  left_join(sa) %>%
  left_join(tmp.total.area) %>%
  mutate(mean.yr.str = (mean.str*STRATUM_AREA/Total), var.mean.yr.str=( (STRATUM_AREA^2/Total^2)*(1-ntows.str/STRATUM_UNITS)*(var.samp.str/ntows.str) ) )%>%
  group_by(YEAR, SEASON) %>% #INDEX GOING DOWN TO YEAR?
  dplyr::summarise(mean.yr = sum(mean.yr.str), var.mean.yr=sum(var.mean.yr.str)) %>% #SUMMARIZE TO CREAT NEW VARIABLES
  mutate(sd.mean.yr=sqrt(var.mean.yr), CV=sd.mean.yr/mean.yr, season = ifelse(SEASON=="SPRING",1,2))  #MUTATE TO CREATE NEW VARIABLE FROM EXISTING VARIABLE

#EDIT OUT BELOW CHUNK TO PROVIDE ESTIMATES BY STRATUM

#   #remove chracter Season so we can summarize with Reduce
#   surv.ind.yr <- subset(surv.ind.yr,select = -c(Season))
# 
#   #calculate estimate by strata
#   mean.yr.strrr <- surv.ind.str %>%
#     left_join(sa,by="stratum") %>%
#     left_join(tmp.total.area,by="Season") %>%
#     mutate(mean.yr.str = (mean.str*STRATUM_AREA) ) %>%
#     group_by(year, Season) %>% #INDEX GOING DOWN TO YEAR?
#     dplyr::summarise(mean.yr.strr = mean.yr.str, stratum=stratum,season = ifelse(Season=="SPRING",1,2))
#   
#   out_put <- list(surv.ind.yr,surv.ind.str, mean.yr.strrr)
#   names(out_put) <- c("surv.ind.yr","surv.ind.str", "mean.yr.strrr")
# return(out_put)

return(surv.ind.yr)

} # end function srs_nefsc


