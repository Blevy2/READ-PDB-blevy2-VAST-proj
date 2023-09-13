run_simulation <- 
  function(x){
    
    #this might track progress. If not check here: https://cran.r-project.org/web/packages/pbapply/pbapply.pdf
    system(paste("echo 'now processing:",x,"'"))
    
    
    source("R/run_sim.R")
    
    
    #to source a new go_fish where I edited to skips most things:
    #1: load file
    source("R/go_fish_Bens.R") #my edited version that skips most things
    #2: allow the function to call other hidden functions from mixfishsim 
    environment(go_fish_Bens) <- asNamespace('MixFishSim')
    #3: replace go_fish with go_fish_Bens in the MixFishSim package
    assignInNamespace("go_fish", go_fish_Bens, ns = "MixFishSim")
    
    #   #rebuild package
    #devtools::build()
    
    
    #load rcpp exports ON SERVER ONLY. WONT WORK ON PC
    Rcpp::sourceCpp(file= "src/Movement.cpp")
    Rcpp::sourceCpp(file= "src/RcppExports.cpp")
    Rcpp::compileAttributes() #this updates RcppExports.R file, which contains function definitions
    
    #ON PC JUST 1) REBUILD PACKAGE (ABOVE) THEN 2) RELOAD PACKAGE 
    library(MixFishSim) #each core needs to load library
    
    #load data from CPU
    #  load("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/20 year moveCov matrices/Final/Final_moveCov_12_9_Bensmethod.RData")
    
    #load constant temp data from Mars
    #load("/net/home5/blevy/Bens_R_Projects/READ-PDB-blevy2-toy/20 year moveCov matrices/Final/Final_constanttemp_20yr.RData")
    
    #load increase temp data from Mars
    load("/Results/GB_3species_IncPop_IncrTemp_environment.RData")
    
    
    #this starts the profiling
    # library(profvis)
    # 
    # profvis({
    
    
    start_time <- Sys.time() # record start time
    
    
    res<- run_sim(sim_init = sim,
                  pop_init = Pop,
                  move_cov = moveCov,
                  fleets_init = fleets,
                  hab_init = hab,
                  save_pop_bio = TRUE,
                  survey = NULL, #surv_random, will try to survey after simulation
                  closure = NULL,
                  InParallel = TRUE,
                  nz = nonzero_idx)  #does it runin parallel? Doesnt seem like it
    
    end_time <- Sys.time() # record end time
    tot_time <- end_time - start_time # calculate duration of lrren() example
    
    
    #})
    
    saveRDS(res,file = paste("/home/blevy/READ-PDB-blevy2-MFS2/Results/GB_3species_IncPop_IncrTemp_RESULTS_iter",x,".RData", sep = ""))
    
    remove(res)
    
  }



#run in parallel

library(parallel)

nCoresToUse <- detectCores() - 1   #this show 16 cores but I think I have 6??

nCoresToUse <- 15


cl <- parallel::makeCluster(nCoresToUse,revtunnel = TRUE, outfile = "", verbose = TRUE, master=nsl(Sys.info()['nodename'])) #options from https://stackoverflow.com/questions/41639929/r-cannot-makecluster-multinode-due-to-cannot-open-the-connection-error


parallel::parLapply(cl,1:100,run_simulation)
parallel::stopCluster(cl)



#save results
# save.image("/net/home5/blevy/Bens_R_Projects/READ-PDB-blevy2-MFS2/Results/GB_3species_IncPop_IncrTemp_RESULTS.RData")

# 
# #below throws an error
# #result <- foreach(i=1:3) %dopar% run_simulation(i)
# 
# #below throws a similar error: 3 nodes produced errors; first error: missing value where TRUE/FALSE needed
#  parLapply(cl,1:3,run_simulation)