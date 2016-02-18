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
	test <- sqldf("select \r
			unit_id, assignment_id, session_id, min(dt_start) - max(dt_end) as session_r\r
		from p_data \r
		group by unit_id, assignment_id, session_id")
	#print(test)
	page_activity <- sqldf("\r
		select \r
			p.task_id,p.unit_id,p.assignment_id, p.session_id, \r
			count(p.unit_id) as amount,\r
			sum(case when p.dt_start > session_l+0.0*ps.block_size and p.dt_start < session_l+1.0*ps.block_size then p.keyboard else 0 end) as keyboard_1, \r
			sum(case when p.dt_start > session_l+1.0*ps.block_size and p.dt_start < session_l+2.0*ps.block_size then p.keyboard else 0 end) as keyboard_2, \r
			sum(case when p.dt_start > session_l+2.0*ps.block_size and p.dt_start < session_l+3.0*ps.block_size then p.keyboard else 0 end) as keyboard_3, \r
			sum(case when p.dt_start > session_l+3.0*ps.block_size and p.dt_start < session_l+4.0*ps.block_size then p.keyboard else 0 end) as keyboard_4, \r
			sum(case when p.dt_start > session_l+4.0*ps.block_size and p.dt_start < session_l+5.0*ps.block_size then p.keyboard else 0 end) as keyboard_5, \r
			sum(case when p.dt_start > session_l+5.0*ps.block_size and p.dt_start < session_l+6.0*ps.block_size then p.keyboard else 0 end) as keyboard_6, \r
			sum(case when p.dt_start > session_l+6.0*ps.block_size and p.dt_start < session_l+7.0*ps.block_size then p.keyboard else 0 end) as keyboard_7, \r
			sum(case when p.dt_start > session_l+7.0*ps.block_size and p.dt_start < session_l+8.0*ps.block_size then p.keyboard else 0 end) as keyboard_8, \r
			sum(case when p.dt_start > session_l+8.0*ps.block_size and p.dt_start < session_l+9.0*ps.block_size then p.keyboard else 0 end) as keyboard_9, \r
			sum(case when p.dt_start > session_l+9.0*ps.block_size and p.dt_start < session_l+10.0*ps.block_size then p.keyboard else 0 end) as keyboard_10, \r
\r
			sum(case when p.dt_start > session_l+0.0*ps.block_size and p.dt_start < session_l+1.0*ps.block_size then p.mouse else 0 end) as mouse_1, \r
			sum(case when p.dt_start > session_l+1.0*ps.block_size and p.dt_start < session_l+2.0*ps.block_size then p.mouse else 0 end) as mouse_2, \r
			sum(case when p.dt_start > session_l+2.0*ps.block_size and p.dt_start < session_l+3.0*ps.block_size then p.mouse else 0 end) as mouse_3, \r
			sum(case when p.dt_start > session_l+3.0*ps.block_size and p.dt_start < session_l+4.0*ps.block_size then p.mouse else 0 end) as mouse_4, \r
			sum(case when p.dt_start > session_l+4.0*ps.block_size and p.dt_start < session_l+5.0*ps.block_size then p.mouse else 0 end) as mouse_5, \r
			sum(case when p.dt_start > session_l+5.0*ps.block_size and p.dt_start < session_l+6.0*ps.block_size then p.mouse else 0 end) as mouse_6, \r
			sum(case when p.dt_start > session_l+6.0*ps.block_size and p.dt_start < session_l+7.0*ps.block_size then p.mouse else 0 end) as mouse_7, \r
			sum(case when p.dt_start > session_l+7.0*ps.block_size and p.dt_start < session_l+8.0*ps.block_size then p.mouse else 0 end) as mouse_8, \r
			sum(case when p.dt_start > session_l+8.0*ps.block_size and p.dt_start < session_l+9.0*ps.block_size then p.mouse else 0 end) as mouse_9, \r
			sum(case when p.dt_start > session_l+9.0*ps.block_size and p.dt_start < session_l+10.0*ps.block_size then p.mouse else 0 end) as mouse_10, \r
\r
			sum(case when p.dt_start > session_l+0.0*ps.block_size and p.dt_start < session_l+1.0*ps.block_size then p.scroll else 0 end) as scroll_1, \r
			sum(case when p.dt_start > session_l+1.0*ps.block_size and p.dt_start < session_l+2.0*ps.block_size then p.scroll else 0 end) as scroll_2, \r
			sum(case when p.dt_start > session_l+2.0*ps.block_size and p.dt_start < session_l+3.0*ps.block_size then p.scroll else 0 end) as scroll_3, \r
			sum(case when p.dt_start > session_l+3.0*ps.block_size and p.dt_start < session_l+4.0*ps.block_size then p.scroll else 0 end) as scroll_4, \r
			sum(case when p.dt_start > session_l+4.0*ps.block_size and p.dt_start < session_l+5.0*ps.block_size then p.scroll else 0 end) as scroll_5, \r
			sum(case when p.dt_start > session_l+5.0*ps.block_size and p.dt_start < session_l+6.0*ps.block_size then p.scroll else 0 end) as scroll_6, \r
			sum(case when p.dt_start > session_l+6.0*ps.block_size and p.dt_start < session_l+7.0*ps.block_size then p.scroll else 0 end) as scroll_7, \r
			sum(case when p.dt_start > session_l+7.0*ps.block_size and p.dt_start < session_l+8.0*ps.block_size then p.scroll else 0 end) as scroll_8, \r
			sum(case when p.dt_start > session_l+8.0*ps.block_size and p.dt_start < session_l+9.0*ps.block_size then p.scroll else 0 end) as scroll_9, \r
			sum(case when p.dt_start > session_l+9.0*ps.block_size and p.dt_start < session_l+10.0*ps.block_size then p.scroll else 0 end) as scroll_10 \r
		from p_data p \r
		left join (select \r
			unit_id, assignment_id, session_id, min(dt_start) as session_l, max(dt_end) as session_r, (max(dt_end) - min(dt_start))/10 as block_size\r
		from p_data \r
		group by unit_id, assignment_id, session_id) ps on p.unit_id = ps.unit_id and p.assignment_id = ps.assignment_id and p.session_id = ps.session_id   \r 
		group by p.task_id, p.unit_id, p.assignment_id, p.session_id,ps.session_r")
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
			a.duration, \r 
			case when e.re_evaluation not null then e.re_evaluation else -10 end as re_evaluation, \r
			1.0*p.keyboard_1/p.amount as kb_1,\r
			1.0*p.keyboard_2/p.amount as kb_2,\r
			1.0*p.keyboard_3/p.amount as kb_3,\r
			1.0*p.keyboard_4/p.amount as kb_4,\r
			1.0*p.keyboard_5/p.amount as kb_5,\r
			1.0*p.keyboard_6/p.amount as kb_6,\r
			1.0*p.keyboard_7/p.amount as kb_7,\r
			1.0*p.keyboard_8/p.amount as kb_8,\r
			1.0*p.keyboard_9/p.amount as kb_9,\r
			1.0*p.keyboard_10/p.amount as kb_10,\r
			\r
			1.0*p.mouse_1/p.amount as ms_1,\r
			1.0*p.mouse_2/p.amount as ms_2,\r
			1.0*p.mouse_3/p.amount as ms_3,\r
			1.0*p.mouse_4/p.amount as ms_4,\r
			1.0*p.mouse_5/p.amount as ms_5,\r
			1.0*p.mouse_6/p.amount as ms_6,\r
			1.0*p.mouse_7/p.amount as ms_7,\r
			1.0*p.mouse_8/p.amount as ms_8,\r
			1.0*p.mouse_9/p.amount as ms_9,\r
			1.0*p.mouse_10/p.amount as ms_10,\r
			\r
			1.0*p.scroll_1/p.amount as sc_1,\r
			1.0*p.scroll_2/p.amount as sc_2,\r
			1.0*p.scroll_3/p.amount as sc_3,\r
			1.0*p.scroll_4/p.amount as sc_4,\r
			1.0*p.scroll_5/p.amount as sc_5,\r
			1.0*p.scroll_6/p.amount as sc_6,\r
			1.0*p.scroll_7/p.amount as sc_7,\r
			1.0*p.scroll_8/p.amount as sc_8,\r
			1.0*p.scroll_9/p.amount as sc_9,\r
			1.0*p.scroll_10/p.amount as sc_10,\r
			\r
			case when keyboard_1 = 0 then 0 else 1 end kb_bin_1,\r
			case when keyboard_2 = 0 then 0 else 1 end kb_bin_2,\r
			case when keyboard_3 = 0 then 0 else 1 end kb_bin_3,\r
			case when keyboard_4 = 0 then 0 else 1 end kb_bin_4,\r
			case when keyboard_5 = 0 then 0 else 1 end kb_bin_5,\r
			case when keyboard_6 = 0 then 0 else 1 end kb_bin_6,\r
			case when keyboard_7 = 0 then 0 else 1 end kb_bin_7,\r
			case when keyboard_8 = 0 then 0 else 1 end kb_bin_8,\r
			case when keyboard_9 = 0 then 0 else 1 end kb_bin_9,\r
			case when keyboard_10 = 0 then 0 else 1 end kb_bin_10,\r
			\r
			case when mouse_1 = 0 then 0 else 1 end ms_bin_1,\r
			case when mouse_2 = 0 then 0 else 1 end ms_bin_2,\r
			case when mouse_3 = 0 then 0 else 1 end ms_bin_3,\r
			case when mouse_4 = 0 then 0 else 1 end ms_bin_4,\r
			case when mouse_5 = 0 then 0 else 1 end ms_bin_5,\r
			case when mouse_6 = 0 then 0 else 1 end ms_bin_6,\r
			case when mouse_7 = 0 then 0 else 1 end ms_bin_7,\r
			case when mouse_8 = 0 then 0 else 1 end ms_bin_8,\r
			case when mouse_9 = 0 then 0 else 1 end ms_bin_9,\r
			case when mouse_10 = 0 then 0 else 1 end ms_bin_10,\r	
			\r
			case when scroll_1 = 0 then 0 else 1 end sc_bin_1,\r
			case when scroll_2 = 0 then 0 else 1 end sc_bin_2,\r
			case when scroll_3 = 0 then 0 else 1 end sc_bin_3,\r
			case when scroll_4 = 0 then 0 else 1 end sc_bin_4,\r
			case when scroll_5 = 0 then 0 else 1 end sc_bin_5,\r
			case when scroll_6 = 0 then 0 else 1 end sc_bin_6,\r
			case when scroll_7 = 0 then 0 else 1 end sc_bin_7,\r
			case when scroll_8 = 0 then 0 else 1 end sc_bin_8,\r
			case when scroll_9 = 0 then 0 else 1 end sc_bin_9,\r
			case when scroll_10 = 0 then 0 else 1 end sc_bin_10,\r			
			\r
			-- IFNULL(p.keyboard*1.0/p.amount,0) as page_kb,IFNULL(p.mouse*1.0/p.amount,0) as page_ms,IFNULL(p.scroll*1.0/p.amount,0) as page_sc, \r
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



aband_set$abandoned <- as.factor(aband_set$abandoned)
aband_set <- aband_set[,names(aband_set)!="re_evaluation"]

eva_model <-train(re_evaluation ~ ., method="rpart", data = eva_set)
aband_model <-train(abandoned ~ ., method="rpart", data = aband_set)

print("Accuracy prediction")
print(eva_model$finalModel)
print("Abandonce prediction")
print(aband_model$finalModel)