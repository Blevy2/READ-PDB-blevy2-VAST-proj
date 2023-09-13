#go through folders described in combos and record any missing files

#load in stratified mean data
strat_mean_species <- readRDS("Data/strat_mean_species.RDS")

#SET THE APPROPRIATE WORKING DIRECTORY MANUALLY
setwd(paste0(getwd(),"/VAST_runs"))

#READ IN CSV WITH combintations used
#note: if I forget to save the combos.RDS file, I can probably recreate the table by
#1)searching through each directory, saving the unique names
#2) create a table with all combinations of these names

combos <- readRDS("combos.RDS")
combos_names = colnames(combos)



#replace bias_correct with the corresponding folder name
combos$bias_corr= ifelse(combos$bias_corr==TRUE,"BC_BCY","BC_BCN")
#replace obsmodel value with the corresponding folder name
combos$obsmodel= paste0("obsmodel",combos$obsmodel)
#replace knot_number value with the corresponding folder name
combos$N_knots= paste0("KN_",combos$N_knots)

#check for covariate folders and build into the table
covs = list.files(paste0(combos[1,1],"/",combos[1,2],"/",combos[1,4],"/SPRING"))
if(is.null(covs)){covs = list.files(paste0(combos[1,1],"/",combos[1,2],"/",combos[1,4],"/FALL"))}


####################################################################################################
#PICK ONE
covs = covs[covs %in% c("No_Cov","W_Cov")]

covs = covs[covs %in% c("No_Cov_alltows")]

covs = covs[covs %in% c("W_Cov_X1")]
####################################################################################################



if(length(covs)==1){
  combos = cbind(combos,rep(covs[1],length(combos[,1])))
  colnames(combos) = c(combos_names,"Covs")
}
if(length(covs)==2){
  combos1 = cbind(combos,rep(covs[1],length(combos[,1])))
  combos2 = cbind(combos,rep(covs[2],length(combos[,1])))
  colnames(combos1) = c(combos_names,"Covs")
  colnames(combos2) = c(combos_names,"Covs")
  
  combos = rbind(combos1,combos2)
}

#rearrange columns to match path name
combos = cbind(combos[,1],combos[,2],combos[,4],combos[,3],combos[,8],combos[,5],combos[,6],combos[,7])
newnames = c(combos_names[1],combos_names[2],combos_names[4],combos_names[3],"Covs",combos_names[5],combos_names[6],combos_names[7])
colnames(combos) = newnames
combos=as.data.frame(combos)





#create list of folders to use
folders = vector()

for(rows in seq(nrow(combos))){
  #pull out first value
  fd = combos[rows,1]
  
  for(cols in seq(2,ncol(combos))){
  
    #replace KN_N_Samp with correct number 
    if(cols==7){
      #print(fd)
      tfile = list.files(paste0(getwd(),"/",fd))
      combos[rows,cols] = paste0("KN_",substr(tfile,4,6))
      }
      
    fd = paste0(fd,"/",combos[rows,cols])   
    
  } 
  folders[rows] = fd
}




VAST_completed_info = data.frame(matrix(ncol=9,nrow=0))
colnames(VAST_completed_info)=c("species","stock_area","season","started","completed","scenario_number")


scenario_num = 1

for(folder in folders){
  
  #pull out each value
  path = strsplit(folder,"/")
  species = path[[1]][1]
  stock_area = path[[1]][2]
  sn = path[[1]][4]
  
  
  #check for settings file (indicates it started)
  started = ifelse("settings.txt" %in% list.files(folder),"Started","Not Started")
  
  #record number of knots
  knotss = "N/A"
  #record time it took to run
  if(started=="Started"){knotss=substr(path[[1]][7],4,6)}
  
  #if something didnt start is it because there are no seasonal tows?
  if(started=="Not Started"){ 
  tow_data <- tow_data_species[[species]][[stock_area]]
  seasonnn=subset(tow_data,SEASON==sn)
  if(nrow(seasonnn)==0){started = paste0(started," No seasonal data")}
  }
  
  #check for Index_wYearSeason.csv file (indicates it completed)
  completed = ifelse("Index_wYearSeason.csv" %in% list.files(folder),"Finished","Not Finished")
  
  run_time_info = c("N/A","N/A")
  #record time it took to run
  if(completed=="Finished"){load(paste0(getwd(),"/",folder,"/parameter_estimates.RData"))
    hrsss=parameter_estimates$time_for_run[[1]]/(60*60)
    secc=as.numeric(parameter_estimates$time_for_run[[1]])
    run_time_info = c(secc,hrsss)
   }
  
  VAST_completed_info=rbind(VAST_completed_info,c(species,stock_area,sn,started,completed,scenario_num,run_time_info[1],run_time_info[2],knotss))

  
  scenario_num =scenario_num+1
}
colnames(VAST_completed_info)=c("species","stock_area","season","started","completed","scenario_number","run_secs","run_hours","knot_N")


finished_all = subset(VAST_completed_info,started=="Started" & completed=="Finished")
start_not_finished = subset(VAST_completed_info,started=="Started" & completed=="Not Finished")
not_start_not_finish = subset(VAST_completed_info,started%in%c("Not Started","Not Started No seasonal data") & completed=="Not Finished")

                              

#check VAST errors
errs=data.frame()
for(i in seq(length(result))){
  if(!is.null(result[[i]]$message)){
    errs=rbind(errs,c(result[[i]]$message,i))
  }
}
colnames(errs)=c("message","result_n")

#add vast errors to start_not_finish
start_not_finished=subset(start_not_finished,as.numeric(scenario_number)<97)
start_not_finished=cbind(start_not_finished,errs)



###################################################################################################################
#DETERMINE NEW SETTINGS BASED ON ERROR MESSAGE 
zro=vector()


for(i in seq(length(start_not_finished$message))){
  
  #first see whether parameters going to zero
  if(stringr::str_sub(start_not_finished$message[i],start=1,end=48)[[1]]=="Please turn off factor-model variance parameters"){
    #just 1 epsilon parameter?
    if(stringr::str_sub(start_not_finished$message[i],start=-10,end=-4)=="epsilon"){zro[[i]]=stringr::str_sub(start_not_finished$message[i],start=-10,end=-3)} 
    #just 1 omega parameter?
    if(stringr::str_sub(start_not_finished$message[i],start=-8,end=-4)=="omega"){zro[[i]]=stringr::str_sub(start_not_finished$message[i],start=-8,end=-3)} 
    
    #see whether more than 1 is going to 0
    if(nchar(start_not_finished$message[i])>150){
      params=vector()
      msg = start_not_finished$message[i]
      #test to see which parameters are in there
      #omega 1 and 2
      if(grepl("omega2",msg)){params=paste0(params,"/omega2")}
      if(grepl("omega1",msg)){params=paste0(params,"/omega1")}
      if(grepl("epsilon2",msg)){params=paste0(params,"/epsilon2")}
      if(grepl("epsilon1",msg)){params=paste0(params,"/epsilon1")}
      
      zro[[i]] = params
    }
    
  }
  
  #sometimes message is not helpful
  if(stringr::str_sub(start_not_finished$message[i],start=1,end=13)=="Please change"){
    zro[[i]] = start_not_finished$message[i]
  }  
  
  #sometimes there are 100% encounters
  if(stringr::str_sub(start_not_finished$message[i],start=1,end=10)=="Some years"){
    zro[[i]] = "100% encounter"
  }
  
  #sometimes "systems is computationally singular" ???
  if(stringr::str_sub(start_not_finished$message[i],start=1,end=25)=="system is computationally"){
    zro[[i]] = start_not_finished$message[i]
    
  }
  
  #sometimes covariate info is missing
  if(stringr::str_sub(start_not_finished$message[i],start=1,end=4)=="Year"){
    yr = stringr::str_sub(start_not_finished$message[i],start=6,end=9)
    zro[[i]] = start_not_finished$message[i]
  }
  
}

start_not_finished$errors = zro
start_not_finished$scenario_number=as.numeric(start_not_finished$scenario_number)



###################################################################################################################



#save things from this run

saveRDS(result,"result_W_Cov_X1_2.RDS")
saveRDS(not_start_not_finish,"not_start_not_finish_W_Cov_X1_2.RDS")
saveRDS(start_not_finished,"start_not_finished_W_Cov_X1_2.RDS")
saveRDS(finished_all,"finished_all_W_Cov_X1_2.RDS")




