library(sqldf)
library(caret)


source("Libs/graphs.R")
source("Libs/logs_into_features.R")
source("Approach/linearReg.r")

task_type <- "Images"
# =======================================
#TASK_ID <- 854546
#GOOGLE_SPREADSHEET <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=1916343054"

#TASK_ID <- 854432
#GOOGLE_SPREADSHEET <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=62480003"

TASK_ID <- 854753
GOOGLE_SPREADSHEET <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=94510901"
# =======================================
results <- prepareUnitResults(TASK_ID,task_type,GOOGLE_SPREADSHEET)

assignments <- prepareAssignments(TASK_ID)
gaveups <- prepareClicksActivityLogs(TASK_ID)

dumb_start_time <- as.POSIXct("07/01/2015 00:00:00", format='%m/%d/%Y %H:%M:%S')
assignments$first_execution_start <- min(assignments$dt_start)
assignments$re_execution_relative_end <- dumb_start_time + (assignments$dt_end - assignments$first_execution_start)
assignments$re_execution_relative_start <- dumb_start_time + (assignments$dt_start - assignments$first_execution_start)


logs_for_timeline <- sqldf(" \r
	select case when g.gave_up >0 then g.gave_up else 0 end as gave_up, a.assignment_id,r.re_unit_id,IFNULL(a.re_execution_relative_start,r.re_execution_relative_start) as re_execution_relative_start, IFNULL(a.re_execution_relative_end,r.re_execution_relative_end) as re_execution_relative_end, case when a.abandoned = 1 then case when case when g.gave_up >0 then gave_up else 0 end = 1 then -2 else -1 end else r.re_evaluation end as re_evaluation,r.re_index \r
	from results r \r
	left join assignments a on a.unit_id = r.re_unit_id \r
	left join gaveups g on a.assignment_id = g.assignment_id \r
	")

logs_for_timeline$re_evaluation <- as.factor(logs_for_timeline$re_evaluation)
logs_for_timeline$re_unit_id <- as.factor(logs_for_timeline$re_unit_id)
logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_end"] <- logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_start"]+1800

durat <- difftime(logs_for_timeline$re_execution_relative_end, logs_for_timeline$re_execution_relative_start, units = "secs") 
logs_for_timeline$re_duration_num <- as.numeric(durat)
ss <- data.frame(tp=NA, tn=NA, fp=NA,fn = NA,precision = NA, recall = NA,k_value=NA)[numeric(), ]

assignments_all <-logs_for_timeline
for (k in seq(0.0,1.0, by = 0.1)){
	relaunched <- predictAbandonedLinear(assignments_all,k,10,TASK_ID)
	relaunching_summary <- summaryAlgorithm(assignments_all,relaunched)
	relaunching_summary$k_value = k
	ss <- rbind(ss,relaunching_summary)
}
#saveTimelinePlot <- function(logs_for_timeline, TASK_ID, 10, 6)


#transcription2 <- prepareFeaturesDataset(854753,task_type,"https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=94510901")

#images1 <- prepareFeaturesDataset(854544,task_type,"https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=92134759")
#images2 <- prepareFeaturesDataset(854885,task_type,"https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=224720013")

#businessaddress1 <- prepareFeaturesDataset(854545,task_type,"https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=457902294")
#businessaddress2 <- prepareFeaturesDataset(854883,task_type,"https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=549204533")

