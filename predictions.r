library(sqldf)
source("Libs/graphs.R")
source("Libs/collect_experiments.R")


prepareLogDataset <- function(JOB_ID, GOOGLE_SPREADSHEET_URL){
	experiment <- collectFromGoogleSpreadsheet(GOOGLE_SPREADSHEET_URL)
	experiment <- reformatExperimentData(experiment,JOB_ID,"Transcription",0)
	experiment <- filterEvaluation(experiment)
	p_data <- read.table(paste("Logs/",JOB_ID,"_page.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	p_data$dt_start <- as.POSIXct(as.numeric(p_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	p_data$dt_end <- as.POSIXct(as.numeric(p_data$dt_end)/1000, origin="1970-01-01",tz="GMT")

	p_data$task_id <- as.factor(p_data$task_id)
	p_data$unit_id <- as.factor(p_data$unit_id)
	p_data$session_id <- as.factor(p_data$session_id)

	page_activity <- sqldf("\r
		select \r 
		task_id,unit_id,assignment_id, session_id, sum(keyboard) as keyboard, sum(mouse) as mouse, sum(scroll) as scroll, count(keyboard) as amount \r
		from p_data \r
		group by task_id, unit_id, assignment_id, session_id")
	page_activity <- page_activity[page_activity$unit_id != "No data available",]
	page_activity <- page_activity[page_activity$unit_id != "Na",]

	t_data <- read.table(paste("Logs/",JOB_ID,"_tabs.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	t_data$dt_start <- as.POSIXct(as.numeric(t_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	t_data$dt_end <- as.POSIXct(as.numeric(t_data$dt_end)/1000, origin="1970-01-01",tz="GMT")
	t_data$status_duration <-round(as.numeric(difftime(t_data$dt_end,t_data$dt_start, units = "secs")))
	t_data$task_id <- as.factor(t_data$task_id)
	t_data$unit_id <- as.factor(t_data$unit_id)
	t_data$session_id <- as.factor(t_data$session_id)
	t_data[t_data$status==" opened","status"] <- as.factor(" active")

	#t_data[t_data$status_duration<0,c("unit_id","assignment_id","session_id","status","status_duration")]

	tabs_activity <- sqldf("\r
		select \r
		task_id,unit_id,assignment_id, session_id, status, sum(status_duration) as status_duration \r
		from t_data \r
		where status != ' closed' \r
		group by task_id, unit_id, assignment_id, session_id,status \r
		")
	#print(tabs_activity)

	assignments <- sqldf("\r
		select d.*, case when d.dt_end = a.dt then 0 else 1 end as abandoned from \r
		(select task_id, unit_id, assignment_id, session_id, min(dt_start) as dt_start, max(dt_start) as dt_end, max(dt_start) - min(dt_start) as duration \r
			from t_data group by task_id, unit_id, assignment_id, session_id ) d \r
		inner join (select \r 
		task_id,unit_id, max(dt_start) as dt \r
		from t_data \r
		group by task_id, unit_id) a on d.unit_id = a.unit_id")
	#		a.abandoned,\r 
	#		and a.abandoned = 0 \r
	final_log_data <- sqldf("\r
		select 
			a.abandoned, \r
			-- a.duration, \r 
			case when e.re_evaluation not null then e.re_evaluation else -10 end as re_evaluation, \r
			IFNULL(p.keyboard*1.0/p.amount,0) as page_kb,IFNULL(p.mouse*1.0/p.amount,0) as page_ms,IFNULL(p.scroll*1.0/p.amount,0) as page_sc, \r
			-- IFNULL(p.amount,0) as amount, \r
			IFNULL(ta.status_duration*1.0/(ta.status_duration+th.status_duration),0) as page_ac, IFNULL(th.status_duration*1.0/(ta.status_duration+th.status_duration),0) as tab_hd \r
		from assignments a\r
			left join experiment e on abandoned = 0 and a.unit_id = e.re_unit_id
			left join page_activity p on a.unit_id = p.unit_id and a.assignment_id = p.assignment_id and a.session_id = p.session_id
			left join tabs_activity ta on a.unit_id = ta.unit_id and a.assignment_id = ta.assignment_id and a.session_id = ta.session_id and ta.status like '%active%'
			left join tabs_activity th on a.unit_id = th.unit_id and a.assignment_id = th.assignment_id and a.session_id = th.session_id and th.status like '%hidden%'
		where \r
			ta.unit_id is not null and p.unit_id is not null \r
		")
	final_log_data
}
#where \r
#		p.unit_id is not null \r
#		and ta.unit_id is not null \r
#		and th.unit_id is not null \r
#		and case when e.re_evaluation not null then e.re_evaluation else -10 end != -10 \r

#final_log_data$abandoned <- as.factor(final_log_data$abandoned)
#final_log_data$re_evaluation <- as.factor(final_log_data$re_evaluation)

set1 <- prepareLogDataset(851103,"https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=1251457815")
set2 <- prepareLogDataset(851070,"https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=670378900")
set3 <- prepareLogDataset(851104,"https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=867177424")

#common_set <- rbind(set1,set2,set3)

eva_set <- set2
aband_set <- set2
#eva_set <- common_set

eva_set <- eva_set[eva_set$re_evaluation!=-10,]
eva_set$re_evaluation <- as.factor(eva_set$re_evaluation) 
eva_set <- eva_set[,names(eva_set)!="abandoned"]


aband_set$abandoned <- as.factor(aband_set$abandoned)
aband_set <- aband_set[,names(aband_set)!="re_evaluation"]

eva_model <-train(re_evaluation ~ ., method="rpart", data = eva_set)
aband_model <-train(abandoned ~ ., method="rpart", data = aband_set)

print("Accuracy prediction")
print(eva_model$finalModel)
print("Abandonce prediction")
print(aband_model$finalModel)