library(sqldf)
library(caret)
library(rpart)
library(rpart.plot)

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
tabs <- prepareTabActivityLogs(TASK_ID)
gaveups <- prepareClicksActivityLogs(TASK_ID)

dumb_start_time <- as.POSIXct("07/01/2015 00:00:00", format='%m/%d/%Y %H:%M:%S')
assignments$first_execution_start <- min(assignments$dt_start)
assignments$re_execution_relative_end <- dumb_start_time + (assignments$dt_end - assignments$first_execution_start)
assignments$re_execution_relative_start <- dumb_start_time + (assignments$dt_start - assignments$first_execution_start)

tabs$first_execution_start <- min(tabs$session_start)
tabs$re_execution_relative_end <- as.numeric(dumb_start_time) + (tabs$session_end - tabs$first_execution_start)
tabs$re_execution_relative_start <- as.numeric(dumb_start_time) + (tabs$session_start - tabs$first_execution_start)


query <- " \r
	select \r
		r.re_unit_id, \r
		IFNULL(a.re_execution_relative_start,r.re_execution_relative_start) as re_execution_relative_start, \r
		IFNULL(a.re_execution_relative_end,r.re_execution_relative_end) as re_execution_relative_end, \r
		r.re_execution_relative_end as res_end, \r
		ct.re_execution_relative_start as closed_tab, \r
		r.re_index, \r
		case when a.abandoned = 1 then case when case when g.gave_up >0 then gave_up else 0 end = 1 then -2 else -1 end else r.re_evaluation end as re_evaluation, \r
		case when a.abandoned = 1 or ct.sessions>1 then 1.0 else 0.0 end as relaunched, \r
		--case when abs(r.re_execution_relative_end - ct.re_execution_relative_start) >  then 1.0 else 0.0 end as relaunched, \r
		abs(r.re_execution_relative_end - ct.re_execution_relative_start) as dd, \r
		ct.sessions, \r
		a.assignment_id \r
	from results r \r
	left join assignments a on a.unit_id = r.re_unit_id \r
	left join (
		select 
			min(re_execution_relative_start) as re_execution_relative_start,
			count(*) as sessions,
			assignment_id
		from tabs 
		where status = ' closed'
		group by assignment_id
	) ct on a.assignment_id = ct.assignment_id \r
	left join gaveups g on a.assignment_id = g.assignment_id \r
	\r"

logs_for_timeline <- sqldf(query)


validation_summary <- sqldf("\r
		select \r
			sum(case when relaunched = 1 and re_evaluation = -1 then 1 else 0 end) as tp,\r
			sum(case when relaunched = 0 and re_evaluation in (-2,0,1) then 1 else 0 end) as tn,\r
			sum(case when relaunched = 1 and re_evaluation in (-2,0,1) then 1 else 0 end) as fp,\r
			sum(case when relaunched = 0 and re_evaluation = -1 then 1 else 0 end)  as fn\r
		from logs_for_timeline 
		")
	validation_summary <- sqldf("\r
		select \r
			v.*, 1.0*tp/(tp+fp) as precision, 1.0*tp/(tp+fn) as recall\r
		from validation_summary v
		")
	validation_summary
write.table(validation_summary, paste("Approach/closed_tabs_",TASK_ID,".csv",sep = ""),sep=", ")
#logs_for_timeline$re_evaluation <- as.factor(logs_for_timeline$re_evaluation)
#logs_for_timeline$re_unit_id <- as.factor(logs_for_timeline$re_unit_id)
#logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_end"] <- logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_start"]+1800
