Prepare_NWA_Extrapolation_Data_Fn_BENS <- function (strata.limits = NULL, epu_to_use = c("All", "Georges_Bank", 
                                               "Mid_Atlantic_Bight", "Scotian_Shelf", "Gulf_of_Maine", 
                                               "Other")[1], projargs = NA, zone = NA, flip_around_dateline = FALSE, 
          ...) 
{
  if (is.null(strata.limits)) {
    strata.limits = list(All_areas = 1:1e+05)
  }
  message("Using strata ", strata.limits)
  if (any(tolower(epu_to_use) %in% "all")) {
    epu_to_use <- c("Georges_Bank", "Mid_Atlantic_Bight", 
                    "Scotian_Shelf", "Gulf_of_Maine", "Other")
  }
  utils::data(northwest_atlantic_grid, package = "FishStatsUtils")
  Data_Extrap <- northwest_atlantic_grid
  Tmp = cbind(BEST_DEPTH_M = 0, BEST_LAT_DD = Data_Extrap[, 
                                                          "Lat"], BEST_LON_DD = Data_Extrap[, "Lon"])
  if (length(strata.limits) == 1 && strata.limits[[1]][1] == "EPU") {
    Data_Extrap <- Data_Extrap[Data_Extrap$EPU %in% epu_to_use, 
    ]
    Data_Extrap$EPU <- droplevels(Data_Extrap$EPU)
    a_el = matrix(NA, nrow = nrow(Data_Extrap), ncol = length(epu_to_use), 
                  dimnames = list(NULL, epu_to_use))
    Area_km2_x = Data_Extrap[, "Area_in_survey_km2"]
    for (l in 1:ncol(a_el)) {
      a_el[, l] = ifelse(Data_Extrap[, "EPU"] %in% epu_to_use[[l]], 
                         Area_km2_x, 0)
    }
  }
  else {
    a_el = as.data.frame(matrix(NA, nrow = nrow(Data_Extrap), 
                                ncol = length(strata.limits), dimnames = list(NULL, 
                                                                              names(strata.limits))))
    Area_km2_x = Data_Extrap[, "Area_in_survey_km2"]
    for (l in 1:ncol(a_el)) {
      a_el[, l] = ifelse(Data_Extrap[, "stratum_number"] %in% 
                           strata.limits[[l]], Area_km2_x, 0)
    }
  }
  tmpUTM = project_coordinates(X = Data_Extrap[, "Lon"], Y = Data_Extrap[, 
                                                                         "Lat"], projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline)
  Data_Extrap = cbind(Data_Extrap, Include = 1)
  Data_Extrap[, c("E_km", "N_km")] = tmpUTM[, c("X", "Y")]
  Return = list(a_el = a_el, Data_Extrap = Data_Extrap, zone = attr(tmpUTM, 
                                                                    "zone"), projargs = attr(tmpUTM, "projargs"), flip_around_dateline = flip_around_dateline, 
                Area_km2_x = Area_km2_x)
  return(Return)
}
