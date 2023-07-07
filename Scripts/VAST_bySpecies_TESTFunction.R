



#setup function to run in parrallel
run_VAST_TEST <- 
  function(scenario_num){
    setwd(paste0(getwd(), "/run08"))
    load("svdata.RData")
    
    library(VAST)
    
    settings = make_settings(n_x = 100, Region = "Northwest_Atlantic", purpose = "index2",
                             strata.limits = list(unique(svdata$stratum)),
                             n_categories = nlevels(svdata$age))
    ptm <- proc.time()
    fit = fit_model(settings = settings, 
                    Lat_i = svdata$latitude, 
                    Lon_i = svdata$longitude, 
                    t_i = svdata$year, 
                    c_i = as.numeric(svdata$age)-1,
                    b_i = svdata$expcatchwt, 
                    a_i = svdata$AreaSwept_km2)
   print(proc.time() - ptm)
    
#    plot(fit)
    
    # Save workspace
    save.image(paste0("run08_",scenario_num,".RData"))
    
          }


