source("Approach/simulations_lib.r")
dumb_start_time <- as.POSIXct("07/01/2015 00:00:00", format='%m/%d/%Y %H:%M:%S')
TASK_ID <-SIMULATION_TASKS[2,"id"]
GOOGLE_SPREADSHEET <- SIMULATION_TASKS[2,"spreadsheet"]
task_type <- "Images"
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
	logs_for_timeline[logs_for_timeline$re_evaluation == 0,"re_evaluation"] <- 1
	logs_for_timeline$re_unit_id <- as.factor(logs_for_timeline$re_unit_id)
	#logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_end"] <- logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_start"]+1800
	logs_for_timeline$re_unit_number <- logs_for_timeline$re_index
	logs_for_timeline$re_condition <- logs_for_timeline$re_index
	logs_for_timeline$re_task <- "Receipt transcription"

	logs_for_timeline <- logs_for_timeline[logs_for_timeline$re_evaluation %in% c(1,-1,-2),]
tl <- plotTimeline(logs_for_timeline,tl_bar_detail_show= F,tl_bar_detail="re_unit_id")
tl
ggsave(tl, file="Graphs/timeline_3.pdf", width=12, height=7)
