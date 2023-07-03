make_extrapolation_info_BENS <- function (Region, projargs = NA, zone = NA, strata.limits = data.frame(STRATA = "All_areas"), 
          create_strata_per_region = FALSE, max_cells = NULL, input_grid = NULL, 
          observations_LL = NULL, grid_dim_km = c(2, 2), maximum_distance_from_sample = NULL, 
          grid_in_UTM = TRUE, grid_dim_LL = c(0.1, 0.1), region = c("south_coast", 
                                                                    "west_coast"), strata_to_use = c("SOG", "WCVI", "QCS", 
                                                                                                     "HS", "WCHG"), epu_to_use = c("All", "Georges_Bank", 
                                                                                                                                   "Mid_Atlantic_Bight", "Scotian_Shelf", "Gulf_of_Maine", 
                                                                                                                                   "Other")[1], survey = "Chatham_rise", surveyname = "propInWCGBTS", 
          flip_around_dateline, nstart = 100, area_tolerance = 0.05, 
          backwards_compatible_kmeans = FALSE, DirPath = paste0(getwd(), 
                                                                "/"), ...) 
{
  if (is.null(max_cells)) 
    max_cells = Inf
  for (rI in seq_along(Region)) {
    Extrapolation_List = NULL
    if (tolower(Region[rI]) == "california_current") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_WCGBTS_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                                surveyname = surveyname, projargs = projargs, 
                                                                zone = zone, flip_around_dateline = flip_around_dateline, 
                                                                ...)
    }
    if (tolower(Region[rI]) %in% c("wcghl", "wcghl_domain", 
                                   "west_coast_hook_and_line")) {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_WCGHL_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                               projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                               ...)
    }
    if (tolower(Region[rI]) == "british_columbia") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_BC_Coast_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                                  strata_to_use = strata_to_use, projargs = projargs, 
                                                                  zone = zone, flip_around_dateline = flip_around_dateline, 
                                                                  ...)
    }
    if (tolower(Region[rI]) == "eastern_bering_sea") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = TRUE
      Extrapolation_List = Prepare_EBS_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                             projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                             ...)
    }
    if (tolower(Region[rI]) == "northern_bering_sea") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_NBS_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                             projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                             ...)
    }
    if (tolower(Region[rI]) == "bering_sea_slope") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_BSslope_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                                 projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                                 ...)
    }
    if (tolower(Region[rI]) == "chukchi_sea") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_Chukchi_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                                 projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                                 ...)
    }
    if (tolower(Region[rI]) %in% c("st_matthews_island", 
                                   "smi")) {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = TRUE
      Extrapolation_List = Prepare_SMI_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                             projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                             ...)
    }
    if (tolower(Region[rI]) == "aleutian_islands") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = TRUE
      Extrapolation_List = Prepare_AI_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                            projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                            ...)
    }
    if (tolower(Region[rI]) == "gulf_of_alaska") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_GOA_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                             projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                             ...)
    }
    if (tolower(Region[rI]) == tolower("BFISH_MHI")) {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_BFISH_MHI_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                                   projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                                   ...)
    }
    if (tolower(Region[rI]) == "northwest_atlantic") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_NWA_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                             epu_to_use = epu_to_use, projargs = projargs, 
                                                             zone = zone, flip_around_dateline = flip_around_dateline, 
                                                             ...)
    }
    if (tolower(Region[rI]) == "south_africa") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_SA_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                            region = region, projargs = projargs, zone = zone, 
                                                            flip_around_dateline = flip_around_dateline, 
                                                            ...)
    }
    if (tolower(Region[rI]) == "gulf_of_st_lawrence") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_GSL_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                             projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                             ...)
    }
    if (tolower(Region[rI]) == "new_zealand") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_NZ_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                            survey = survey, projargs = projargs, zone = zone, 
                                                            flip_around_dateline = flip_around_dateline, 
                                                            ...)
    }
    if (tolower(Region[rI]) == "habcam") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_HabCam_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                                projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                                ...)
    }
    if (tolower(Region[rI]) == "gulf_of_mexico") {
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_GOM_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                             projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                             ...)
    }
    Shapefile_set = c("ATL-IBTS-Q1", "ATL-IBTS-Q4", "BITS", 
                      "BTS", "BTS-VIIA", "EVHOE", "IE-IGFS", "NIGFS", 
                      "NS_IBTS", "PT-IBTS", "SP-ARSA", "SP-NORTH", "SP-PORC", 
                      "CalCOFI_Winter-Spring", "CalCOFI-IMECOCAL_Winter-Spring", 
                      "IMECOCAL_Winter-Spring", "CalCOFI-IMECOCAL_Summer", 
                      "rockfish_recruitment_coastwide", "rockfish_recruitment_core")
    if (toupper(Region[rI]) %in% toupper(Shapefile_set)) {
      if (Region[rI] == "SP-ARSA") 
        stop("There's some problem with `SP-ARSA` which precludes it's use")
      Conversion = convert_shapefile(file_path = paste0(system.file("region_shapefiles", 
                                                                    package = "FishStatsUtils"), "/", toupper(Region[rI]), 
                                                        "/Shapefile.shp"), projargs_for_shapefile = "+proj=longlat +ellps=WGS84 +no_defs", 
                                     projargs = projargs, grid_dim_km = grid_dim_km, 
                                     area_tolerance = area_tolerance, ...)
      Extrapolation_List = list(a_el = matrix(Conversion$extrapolation_grid[, 
                                                                            "Area_km2"], ncol = 1), Data_Extrap = Conversion$extrapolation_grid, 
                                zone = NA, projargs = Conversion$projargs, flip_around_dateline = FALSE, 
                                Area_km2_x = Conversion$extrapolation_grid[, 
                                                                           "Area_km2"])
    }
    if (file.exists(Region[rI])) {
      Conversion = convert_shapefile(file_path = Region[rI], 
                                     projargs = projargs, grid_dim_km = grid_dim_km, 
                                     area_tolerance = area_tolerance, ...)
      Extrapolation_List = list(a_el = matrix(Conversion$extrapolation_grid[, 
                                                                            "Area_km2"], ncol = 1), Data_Extrap = Conversion$extrapolation_grid, 
                                zone = NA, projargs = Conversion$projargs, flip_around_dateline = FALSE, 
                                Area_km2_x = Conversion$extrapolation_grid[, 
                                                                           "Area_km2"])
    }
    if (tolower(Region[rI]) == "stream_network") {
      if (is.null(input_grid)) {
        stop("Because you're using a stream network, please provide 'input_grid' input")
      }
      if (!(all(c("Lat", "Lon", "Area_km2", "child_i") %in% 
                colnames(input_grid)))) {
        stop("'input_grid' must contain columns named 'Lat', 'Lon', 'Area_km2', and 'child_i'")
      }
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_User_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                              input_grid = input_grid, projargs = projargs, 
                                                              zone = zone, flip_around_dateline = flip_around_dateline, 
                                                              ...)
    }
    if (tolower(Region[rI]) == "user") {
      if (is.null(input_grid)) {
        stop("Because you're using a user-supplied region, please provide 'input_grid' input")
      }
      if (!(all(c("Lat", "Lon", "Area_km2") %in% colnames(input_grid)))) {
        stop("'input_grid' must contain columns named 'Lat', 'Lon', and 'Area_km2'")
      }
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_User_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                              input_grid = input_grid, projargs = projargs, 
                                                              zone = zone, flip_around_dateline = flip_around_dateline, 
                                                              ...)
    }
    if (is.null(Extrapolation_List)) {
      if (is.null(observations_LL)) {
        stop("Because you're using a new Region[rI], please provide 'observations_LL' input with columns named `Lat` and `Lon`")
      }
      if (missing(flip_around_dateline)) 
        flip_around_dateline = FALSE
      Extrapolation_List = Prepare_Other_Extrapolation_Data_Fn(strata.limits = strata.limits, 
                                                               observations_LL = observations_LL, grid_dim_km = grid_dim_km, 
                                                               maximum_distance_from_sample = maximum_distance_from_sample, 
                                                               grid_in_UTM = grid_in_UTM, grid_dim_LL = grid_dim_LL, 
                                                               projargs = projargs, zone = zone, flip_around_dateline = flip_around_dateline, 
                                                               ...)
    }
    if (rI == 1) {
      Return = Extrapolation_List
    }
    else {
      Return = combine_extrapolation_info(Return, Extrapolation_List, 
                                          create_strata_per_region = create_strata_per_region)
    }
  }
  loc_orig = Return$Data_Extrap[, c("E_km", "N_km")]
  loc_orig = loc_orig[which(Return$Area_km2_x > 0), ]
  if (max_cells < nrow(loc_orig)) {
    message("# Reducing extrapolation-grid from ", nrow(Return$Data_Extrap), 
            " to ", max_cells, " cells for Region(s): ", paste(Region, 
                                                               collapse = ", "))
    Kmeans = make_kmeans(n_x = max_cells, loc_orig = loc_orig, 
                         nstart = nstart, randomseed = 1, iter.max = 1000, 
                         DirPath = DirPath, Save_Results = TRUE, kmeans_purpose = "extrapolation", 
                         backwards_compatible_kmeans = backwards_compatible_kmeans)
    Kmeans[["cluster"]] = RANN::nn2(data = Kmeans[["centers"]], 
                                    query = Return$Data_Extrap[, c("E_km", "N_km")], 
                                    k = 1)$nn.idx[, 1]
    aggregate_vector = function(values_x, index_x, max_index, 
                                FUN = sum) {
      tapply(values_x, INDEX = factor(index_x, levels = 1:max_index), 
             FUN = FUN)
    }
    a_el = matrix(NA, nrow = max_cells, ncol = ncol(Return$a_el))
    for (lI in 1:ncol(Return$a_el)) {
      a_el[, lI] = aggregate_vector(values_x = Return$a_el[, 
                                                           lI], index_x = Kmeans$cluster, max_index = max_cells)
    }
    Area_km2_x = aggregate_vector(values_x = Return$Area_km2_x, 
                                  index_x = Kmeans$cluster, max_index = max_cells)
    Include = aggregate_vector(values_x = Return$Data_Extrap[, 
                                                             "Include"], index_x = Kmeans$cluster, max_index = max_cells, 
                               FUN = function(vec) {
                                 any(vec > 0)
                               })
    lonlat_g = project_coordinates(X = Kmeans$centers[, 
                                                      "E_km"], Y = Kmeans$centers[, "N_km"], projargs = "+proj=longlat +ellps=WGS84", 
                                   origargs = Return$projargs)
    Data_Extrap = cbind(Lon = lonlat_g[, 1], Lat = lonlat_g[, 
                                                            2], Include = Include, Kmeans$centers)
    Return = list(a_el = a_el, Data_Extrap = Data_Extrap, 
                  zone = Return$zone, projargs = Return$projargs, 
                  flip_around_dateline = Return$flip_around_dateline, 
                  Area_km2_x = Area_km2_x)
  }
  else {
    Return$a_el = as.matrix(Return$a_el)
  }
  if (length(Region) > 1 & create_strata_per_region == TRUE) {
    Return$a_el = cbind(Total = rowSums(Return$a_el), Return$a_el)
  }
  units(Return$a_el) = "km^2"
  units(Return$Area_km2_x) = "km^2"
  class(Return) = "make_extrapolation_info"
  return(Return)
}
