#this script compiles a list of all VAST runs for a given data source that either
# - converged
# - failed
# - never started


#SET THE WORKING DIRECTORY TO WHEREVER YOU WANT THE THE FILES TO BE COMPILED FROM


directory = strsplit(getwd(),"/")

temp = strsplit(directory[[1]][(length(directory[[1]]))],"_")[[1]]

temp_data_type = temp[[length(temp)]]


#column names to keep (get rid of messages)
good_col_names = c("species","stock_area","season","knot_N","scenario","message")




#FIRST COMPILE THOSE THAT STARTED BUT DID NOT FINISH

start_not_finished_WCov2 <- readRDS("start_not_finished_W_Cov2.RDS")
start_not_finished_WCov2$scenario = "WCov (just X2)"
start_not_finished_WCov2 = start_not_finished_WCov2[,good_col_names]

start_not_finished_W_Cov_X1_2 <- readRDS("start_not_finished_W_Cov_X1_2.RDS")
start_not_finished_W_Cov_X1_2$scenario <- "WCov (X1 and X2)"
start_not_finished_W_Cov_X1_2 = start_not_finished_W_Cov_X1_2[,good_col_names]

start_not_finished_NoCov2 <- readRDS("start_not_finished_NoCov2.RDS")
start_not_finished_NoCov2$scenario = paste0("No Cov, just data from ", temp_data_type )
start_not_finished_NoCov2 = start_not_finished_NoCov2[,good_col_names]

start_not_finished_NoCov2_alltows <- readRDS("start_not_finished_NoCov2_alltows.RDS")
start_not_finished_NoCov2_alltows$scenario = "No Cov all tows"
start_not_finished_NoCov2_alltows = start_not_finished_NoCov2_alltows[,good_col_names]


start_not_finished_all = rbind(start_not_finished_WCov2,
                               start_not_finished_W_Cov_X1_2,
                               start_not_finished_NoCov2,
                               start_not_finished_NoCov2_alltows)

start_not_finished_all = start_not_finished_all[,good_col_names]
  
start_not_finished_all$data_type = temp_data_type

write.csv(start_not_finished_all,paste0("start_not_finished_all_",temp_data_type,".csv"))




#NEXT COMPILE THOSE THAT DID NOT START BUT DID NOT FINISH

not_start_not_finished_WCov2 <- readRDS("not_start_not_finish_WCov2.RDS")
not_start_not_finished_WCov2$scenario = "WCov (just X2)"

not_start_not_finished_W_Cov_X1_2 <- readRDS("not_start_not_finish_W_Cov_X1_2.RDS")
not_start_not_finished_W_Cov_X1_2$scenario <- "WCov (X1 and X2)"

not_start_not_finished_NoCov2 <- readRDS("not_start_not_finish_NoCov2.RDS")
not_start_not_finished_NoCov2$scenario = paste0("No Cov, just data from ", temp_data_type )

not_start_not_finished_NoCov2_alltows <- readRDS("not_start_not_finish_NoCov2_alltows.RDS")
not_start_not_finished_NoCov2_alltows$scenario = "No Cov all tows"


#MAKE SURE THEY ALL HAVE THE SAME NUMBER OF COLUMNS. IF NOT, REMOVE ONES NOT NEEDED
#not_start_not_finished_NoCov2_alltows = not_start_not_finished_NoCov2_alltows[,-c(10,11)]


not_start_not_finished_all = rbind(not_start_not_finished_WCov2,
                               not_start_not_finished_W_Cov_X1_2,
                               not_start_not_finished_NoCov2,
                               not_start_not_finished_NoCov2_alltows)

not_start_not_finished_all$data_type = temp_data_type

write.csv(not_start_not_finished_all,paste0("not_start_not_finished_all_",temp_data_type,".csv"))





#COMPILE THOSE THAT DID NOT FINISH AT FIRST, BUT A CHANGE IN SETTINGS ALLOWED THEM TO FINISH
#(THESE ARE IN START_NOT_FINISH1 BUT NOT START_NOT_FINISH2)


start_not_finished_WCov <- readRDS("start_not_finished_WCov.RDS")
start_not_finished_WCov$scenario = "WCov (just X2)"
start_not_finished_WCov = start_not_finished_WCov[,good_col_names]

start_not_finished_W_Cov_X1 <- readRDS("start_not_finished_W_Cov_X1.RDS")
start_not_finished_W_Cov_X1$scenario <- "WCov (X1 and X2)"
start_not_finished_W_Cov_X1 = start_not_finished_W_Cov_X1[,good_col_names]

start_not_finished_NoCov <- readRDS("start_not_finished_NoCov.RDS")
start_not_finished_NoCov$scenario = paste0("No Cov, just data from ", temp_data_type )
start_not_finished_NoCov = start_not_finished_NoCov[,good_col_names]

start_not_finished_NoCov_alltows <- readRDS("start_not_finished_NoCov_alltows.RDS")
start_not_finished_NoCov_alltows$scenario = "No Cov all tows"
start_not_finished_NoCov_alltows = start_not_finished_NoCov_alltows[,good_col_names]


start_not_finished1 = rbind(start_not_finished_WCov,
                               start_not_finished_W_Cov_X1,
                               start_not_finished_NoCov,
                               start_not_finished_NoCov_alltows)

start_not_finished_all$data_type = temp_data_type


#remove rows that didnt finish, leaving just the ones that did

colIndx <- colnames(start_not_finished1)[colnames(start_not_finished1) %in% colnames(start_not_finished_all)]
rowIndx <- !as.character(interaction(start_not_finished1)) %in%  as.character(interaction(start_not_finished_all[colIndx]))
finished_with_change=start_not_finished1[rowIndx,]


write.csv(finished_with_change,paste0("finished_with_change_",temp_data_type,".csv"))
