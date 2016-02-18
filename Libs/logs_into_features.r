source("Libs/collect_experiments.R")

prepareUnitResults <- function(JOB_ID, TASK_TYPE, GOOGLE_SPREADSHEET_URL){
	experiment <- collectFromGoogleSpreadsheet(GOOGLE_SPREADSHEET_URL)
	experiment <- reformatExperimentData(experiment,JOB_ID,TASK_TYPE,0)
	experiment <- filterEvaluation(experiment)
	experiment
}
preparePageActivityLogs <- function(JOB_ID, break_time = dumb_start_time <- as.POSIXct("07/01/2020 00:00:00", format='%m/%d/%Y %H:%M:%S')){
	p_data <- read.table(paste("Logs/",JOB_ID,"/",JOB_ID,"_page.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	p_data$dt_start <- as.POSIXct(as.numeric(p_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	p_data$dt_end <- as.POSIXct(as.numeric(p_data$dt_end)/1000, origin="1970-01-01",tz="GMT")

	p_data$task_id <- as.factor(p_data$task_id)
	p_data$unit_id <- as.factor(p_data$unit_id)
	p_data$session_id <- as.factor(p_data$session_id)

	page_activity <- sqldf(paste("\r
		select \r 
		p.task_id,p.unit_id,p.assignment_id, p.session_id, sum(p.keyboard) as keyboard, sum(p.mouse) as mouse, sum(p.scroll) as scroll, sum(p.text_selected) as text_selected, count(p.keyboard) as amount, \r
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
		where dt_start < '",break_time,"' \r
		group by unit_id, assignment_id, session_id) ps on p.unit_id = ps.unit_id and p.assignment_id = ps.assignment_id and p.session_id = ps.session_id   \r 
		where p.dt_start < '",break_time,"' \r
		group by p.task_id, p.unit_id, p.assignment_id, p.session_id,ps.session_r",sep=""))

	page_activity <- page_activity[page_activity$unit_id != "No data available",]
	page_activity <- page_activity[page_activity$unit_id != "Na",]
	page_activity
}

prepareTabActivityLogs <- function(JOB_ID, break_time = dumb_start_time <- as.POSIXct("07/01/2020 00:00:00", format='%m/%d/%Y %H:%M:%S')){
	t_data <- read.table(paste("Logs/",JOB_ID,"/",JOB_ID,"_tabs.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	t_data$dt_start <- as.POSIXct(as.numeric(t_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	t_data$dt_end <- as.POSIXct(as.numeric(t_data$dt_end)/1000, origin="1970-01-01",tz="GMT")
	t_data$status_duration <-round(as.numeric(difftime(t_data$dt_end,t_data$dt_start, units = "secs")))
	t_data$task_id <- as.factor(t_data$task_id)
	t_data$unit_id <- as.factor(t_data$unit_id)
	t_data$session_id <- as.factor(t_data$session_id)
	t_data[t_data$status==" opened","status"] <- as.factor(" active")

	#t_data[t_data$status_duration<0,c("unit_id","assignment_id","session_id","status","status_duration")]

	tabs_activity <- sqldf(paste("\r
		select \r
		task_id,unit_id,assignment_id, session_id, status, min(dt_start) as session_start, max(dt_start) as session_end, sum(status_duration) as status_duration \r
		from t_data \r
		where status != ' closed' \r
		and dt_start < '",break_time,"' \r
		group by task_id, unit_id, assignment_id, session_id,status \r
		",sep=""))
	tabs_activity
	#print(tabs_activity)
}
prepareKeysActivityLogs <- function(JOB_ID, break_time = dumb_start_time <- as.POSIXct("07/01/2020 00:00:00", format='%m/%d/%Y %H:%M:%S')){
	k_data <- read.table(paste("Logs/",JOB_ID,"/",JOB_ID,"_keys.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	k_data$dt_start <- as.POSIXct(as.numeric(k_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	k_data$task_id <- as.factor(k_data$task_id)
	k_data$unit_id <- as.factor(k_data$unit_id)
	k_data$session_id <- as.factor(k_data$session_id)
	k_data$key <- as.factor(k_data$key)

	#t_data[t_data$status_duration<0,c("unit_id","assignment_id","session_id","status","status_duration")]
	key_activity <- sqldf(paste("\r
		select \r
			sum(case when key in (8,46) then 1 else 0 end) as key_delete, \r
			sum(case when key in (9) then 1 else 0 end) as key_tab, \r
			sum(case when key in (13) then 1 else 0 end) as key_enter, \r
			sum(case when key in (16) then 1 else 0 end) as key_shift, \r
			sum(case when key in (17) then 1 else 0 end) as key_cntrl, \r
			sum(case when key in (18) then 1 else 0 end) as key_alt, \r
			sum(case when key in (19) then 1 else 0 end) as key_pause, \r
			sum(case when key in (20) then 1 else 0 end) as key_caps, \r
			sum(case when key in (27) then 1 else 0 end) as key_esc, \r
			sum(case when key in (33) then 1 else 0 end) as key_page_up, \r
			sum(case when key in (34) then 1 else 0 end) as key_page_down, \r
			sum(case when key in (35) then 1 else 0 end) as key_end, \r
			sum(case when key in (36) then 1 else 0 end) as key_home, \r
			sum(case when key in (37) then 1 else 0 end) as key_left, \r
			sum(case when key in (38) then 1 else 0 end) as key_up, \r
			sum(case when key in (39) then 1 else 0 end) as key_right, \r
			sum(case when key in (40) then 1 else 0 end) as key_down, \r
			sum(case when key in (45) then 1 else 0 end) as key_insert, \r
			sum(case when key >=48 and key <= 57 then 1 else 0 end) as key_digit, \r
			sum(case when key >=65 and key <= 90 then 1 else 0 end) as key_char, \r
			count(distinct case when key >=65 and key <= 90 then key else 0 end) as key_unique_char, \r
			\r
			sum(case when key in (106) then 1 else 0 end) as key_math_multiply, \r
			sum(case when key in (107) then 1 else 0 end) as key_math_add, \r
			sum(case when key in (109) then 1 else 0 end) as key_math_subtract, \r
			sum(case when key in (110) then 1 else 0 end) as key_math_decimal, \r
			sum(case when key in (111) then 1 else 0 end) as key_math_divide, \r
			sum(case when key in (186) then 1 else 0 end) as key_semicolon, \r
			sum(case when key in (187) then 1 else 0 end) as key_equal, \r
			sum(case when key in (188) then 1 else 0 end) as key_comma, \r
			sum(case when key in (189) then 1 else 0 end) as key_dash, \r
			sum(case when key in (190) then 1 else 0 end) as key_period, \r
			sum(case when key in (191) then 1 else 0 end) as key_slash_fwd, \r
			sum(case when key in (192) then 1 else 0 end) as key_grave_accent, \r
			sum(case when key in (219) then 1 else 0 end) as key_bracket_open, \r
			sum(case when key in (220) then 1 else 0 end) as key_slack_bck, \r
			sum(case when key in (221) then 1 else 0 end) as key_bracket_close, \r
			sum(case when key in (222) then 1 else 0 end) as key_quote_single, \r

			sum(case when key in (106,107,109,110,111,187) then 1 else 0 end) as key_math, \r
			sum(case when key in (110,190) then 1 else 0 end) as key_dots, \r
			sum(case when key in (110,186,188,189,190,192,219,221,222) then 1 else 0 end) as key_punctuation, \r
			sum(case when key in (191,220) then 1 else 0 end) as key_slash, \r
			\r
			count(key) as key_all, \r
			task_id,unit_id,assignment_id, session_id \r
		from k_data \r
		where dt_start < '",break_time,"' \r
		group by task_id, unit_id, assignment_id, session_id \r
		",sep=""))
	key_activity
}
prepareSessions <- function(JOB_ID, break_time = dumb_start_time <- as.POSIXct("07/01/2020 00:00:00", format='%m/%d/%Y %H:%M:%S')){
	# ---------------------------------------
	# BASED ON TABS ACTIVITY
	t_data <- read.table(paste("Logs/",JOB_ID,"/",JOB_ID,"_tabs.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	t_data$dt_start <- as.POSIXct(as.numeric(t_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	t_data$dt_end <- as.POSIXct(as.numeric(t_data$dt_end)/1000, origin="1970-01-01",tz="GMT")
	t_data$status_duration <-round(as.numeric(difftime(t_data$dt_end,t_data$dt_start, units = "secs")))
	t_data$task_id <- as.factor(t_data$task_id)
	t_data$unit_id <- as.factor(t_data$unit_id)
	t_data$session_id <- as.factor(t_data$session_id)
	t_data[t_data$status==" opened","status"] <- as.factor(" active")
	# ---------------------------------------
	assignments <- sqldf(paste("\r
		select d.*, case when d.dt_end = a.dt then 0 else 1 end as abandoned from \r
		(select task_id, unit_id, assignment_id, session_id, min(dt_start) as dt_start, max(dt_start) as dt_end, max(dt_start) - min(dt_start) as duration \r
			from t_data where dt_start < '",break_time,"' group by task_id, unit_id, assignment_id, session_id ) d \r
		inner join (select \r 
		task_id,unit_id, max(dt_start) as dt \r
		from t_data where dt_start < '",break_time,"' \r
		group by task_id, unit_id) a on d.unit_id = a.unit_id",sep=""))

	assignments
}
prepareClicksActivityLogs <- function(JOB_ID, break_time = dumb_start_time <- as.POSIXct("07/01/2020 00:00:00", format='%m/%d/%Y %H:%M:%S')){
	t_data <- read.table(paste("Logs/",JOB_ID,"/",JOB_ID,"_clicks.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	t_data$dt_start <- as.POSIXct(as.numeric(t_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	#t_data$dt_end <- as.POSIXct(as.numeric(t_data$dt_end)/1000, origin="1970-01-01",tz="GMT")
	#t_data$status_duration <-round(as.numeric(difftime(t_data$dt_end,t_data$dt_start, units = "secs")))
	t_data$task_id <- as.factor(t_data$task_id)
	t_data$unit_id <- as.factor(t_data$unit_id)
	t_data$session_id <- as.factor(t_data$session_id)
	t_data$element <- as.factor(t_data$element)

	#t_data[t_data$status_duration<0,c("unit_id","assignment_id","session_id","status","status_duration")]

	tabs_activity <- sqldf(paste("\r
		select \r
		task_id,unit_id,assignment_id, IFNULL(sum(case when element like '%give-up%' then 1 else 0 end),0) as gave_up \r
		from t_data \r
		where dt_start < '",break_time,"'
		group by task_id, unit_id, assignment_id \r
		",sep=""))
	tabs_activity
	#print(tabs_activity)
}
prepareAssignments <- function(JOB_ID, break_time = dumb_start_time <- as.POSIXct("07/01/2020 00:00:00", format='%m/%d/%Y %H:%M:%S')){
	# ---------------------------------------
	# BASED ON TABS ACTIVITY
	t_data <- read.table(paste("Logs/",JOB_ID,"/",JOB_ID,"_page.csv",sep=""), header=T, sep="," ,quote = "\"", comment.char = "")
	t_data$dt_start <- as.POSIXct(as.numeric(t_data$dt_start)/1000, origin="1970-01-01",tz="GMT")
	t_data$dt_end <- as.POSIXct(as.numeric(t_data$dt_end)/1000, origin="1970-01-01",tz="GMT")
	t_data$status_duration <-round(as.numeric(difftime(t_data$dt_end,t_data$dt_start, units = "secs")))
	t_data$task_id <- as.factor(t_data$task_id)
	t_data$unit_id <- as.factor(t_data$unit_id)
	t_data$session_id <- as.factor(t_data$session_id)
	#t_data[t_data$status==" opened","status"] <- as.factor(" active")
	# ---------------------------------------
	query <- paste("\r
		select d.*, case when d.dt_end = a.dt then 0 else 1 end as abandoned from \r
		(select task_id, unit_id, assignment_id, min(dt_start) as dt_start, max(dt_start) as dt_end, max(dt_start) - min(dt_start) as duration \r
			from t_data where dt_start < '",break_time,"' group by task_id, unit_id, assignment_id ) d \r
		inner join (select \r 
		task_id,unit_id, max(dt_start) as dt \r
		from t_data where dt_start < '",break_time,"' \r
		group by task_id, unit_id) a on d.unit_id = a.unit_id",sep="")
	assignments <- sqldf(query)

	assignments
}


prepareFeaturesDataset <- function(JOB_ID, TASK_TYPE, GOOGLE_SPREADSHEET_URL, break_time = dumb_start_time <- as.POSIXct("07/01/2020 00:00:00", format='%m/%d/%Y %H:%M:%S')){
	units <- prepareUnitResults(JOB_ID, TASK_TYPE, GOOGLE_SPREADSHEET_URL)
	page_activity <- preparePageActivityLogs(JOB_ID, break_time)
	tabs_activity <- prepareTabActivityLogs(JOB_ID, break_time)
	key_activity <- prepareKeysActivityLogs(JOB_ID, break_time)
	sessions <- prepareSessions(JOB_ID, break_time)
	
	#		a.abandoned,\r 
	#		and a.abandoned = 0 \r
	featuresDataset <- sqldf("\r
		select \r
			IFNULL(kall.key_delete,0) as key_delete, \r
			-- IFNULL(kall.key_tab,0) as key_tab, \r
			IFNULL(kall.key_enter,0) as key_enter, \r
			IFNULL(kall.key_shift,0) as key_shift, \r
			IFNULL(kall.key_cntrl,0) as key_cntrl, \r
			IFNULL(kall.key_alt,0) as key_alt, \r
			IFNULL(kall.key_pause,0) as key_pause, \r
			IFNULL(kall.key_caps,0) as key_caps, \r
			IFNULL(kall.key_esc,0) as key_esc, \r
			IFNULL(kall.key_page_up,0) as key_page_up, \r
			IFNULL(kall.key_page_down,0) as key_page_down, \r
			IFNULL(kall.key_end,0) as key_end, \r
			IFNULL(kall.key_home,0) as key_home, \r
			IFNULL(kall.key_left,0) as key_left, \r
			IFNULL(kall.key_up,0) as key_up, \r
			IFNULL(kall.key_right,0) as key_right, \r
			IFNULL(kall.key_down,0) as key_down, \r
			IFNULL(kall.key_insert,0) as key_insert, \r
			IFNULL(kall.key_digit,0) as key_digit, \r
			IFNULL(kall.key_char,0) as key_char, \r
			IFNULL(1.0*kall.key_unique_char/kall.key_all,0) as key_unique_part, \r
			IFNULL(kall.key_math,0) as key_math, \r
			IFNULL(kall.key_dots,0) as key_dots, \r
			IFNULL(kall.key_punctuation,0) as key_punctuation, \r
			IFNULL(kall.key_slash,0) as key_slash, \r
			IFNULL(kall.key_math_multiply,0) as key_math_multiply, \r
			IFNULL(kall.key_math_add,0) as key_math_add,  \r
			IFNULL(kall.key_math_subtract,0) as key_math_subtract, \r
			IFNULL(kall.key_math_decimal,0) as key_math_decimal, \r
			IFNULL(kall.key_math_divide,0) as key_math_divide, \r
			IFNULL(kall.key_semicolon,0) as key_semicolon, \r
			IFNULL(kall.key_equal,0) as key_equal, \r
			IFNULL(kall.key_comma,0) as key_comma, \r
			IFNULL(kall.key_dash,0) as key_dash, \r
			IFNULL(kall.key_period,0) as key_period, \r
			IFNULL(kall.key_slash_fwd,0) as key_slash_fwd, \r
			IFNULL(kall.key_grave_accent,0) as key_grave_accent, \r
			IFNULL(kall.key_bracket_open,0) as key_bracket_open, \r
			IFNULL(kall.key_slack_bck,0) as key_slack_bck, \r
			IFNULL(kall.key_bracket_close,0) as key_bracket_close, \r
			IFNULL(kall.key_quote_single,0) as key_quote_single, \r
			IFNULL(kall.key_all,0) as key_all, \r
			\r
			-- IFNULL(p.keyboard*1.0/p.amount,0) as page_kb,IFNULL(p.mouse*1.0/p.amount,0) as page_ms,IFNULL(p.scroll*1.0/p.amount,0) as page_sc, \r
			IFNULL(p.amount,0) as periodic_logs_collected, \r
			-- IFNULL(ta.status_duration*1.0/(ta.status_duration+th.status_duration),0) as page_ac, IFNULL(th.status_duration*1.0/(ta.status_duration+th.status_duration),0) as tab_hd, \r
			a.abandoned, \r
			a.duration, \r 
			/*1.0*p.keyboard_1/p.amount as kb_1,\r
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
			*/\r
			ta.session_start,
			ta.session_end,
			e.re_execution_relative_end, \r
			a.assignment_id, \r
			case when e.re_evaluation not null then e.re_evaluation else -10 end as re_evaluation \r
		from sessions a\r
			left join units e on abandoned = 0 and a.unit_id = e.re_unit_id
			left join page_activity p on a.unit_id = p.unit_id and a.assignment_id = p.assignment_id and a.session_id = p.session_id
			left join tabs_activity ta on a.unit_id = ta.unit_id and a.assignment_id = ta.assignment_id and a.session_id = ta.session_id and ta.status like '%active%'
			left join tabs_activity th on a.unit_id = th.unit_id and a.assignment_id = th.assignment_id and a.session_id = th.session_id and th.status like '%hidden%'
			left join key_activity kall on a.unit_id = kall.unit_id and a.assignment_id = kall.assignment_id and a.session_id = kall.session_id
		where \r
			ta.unit_id is not null and p.unit_id is not null \r
		order by e.re_execution_relative_end \r
		")
	featuresDataset
}


prepareEvaluationTrainingSet <- function(job_dataset){
	eva_set <- job_dataset
	eva_set <- eva_set[eva_set$re_evaluation!=-10,]
	eva_set$re_evaluation <- as.factor(eva_set$re_evaluation) 
	eva_set <- eva_set[,names(eva_set)!="abandoned"]
	eva_set
}

prepareAbandonenceTrainingSet <- function(job_dataset){
	aband_set <- job_dataset
	aband_set$abandoned <- as.factor(aband_set$abandoned)
	aband_set <- aband_set[,names(aband_set)!="re_evaluation"]
	aband_set
}
buildModel <- function(training_set, label_is_evaluation = T, method = "rpart"){
	if (label_is_evaluation){
		fit <- train(re_evaluation ~ ., method = method, data = training_set)
	}
	else{
		fit <- train(abandoned ~ ., method = method, data = training_set)
	}
	fit
}

drawDecisionTree <- function(fit,filename, title = "Decision tree"){
	pdf(paste("Predictions/",filename,".pdf",sep=""), width=6, height=3)
	prp(fit, title = "Accuracy", extra = 1)
	dev.off()
}

predictTaskAccuracy <- function(task_training, task_test){
	training_all <- prepareEvaluationTrainingSet(task_training)
	testing <- prepareEvaluationTrainingSet(task_test) #eval_train[- seq(1,border_index),]
	
	pred <- data.frame(alpha=NA, sensitivity=NA, specificity=NA, accuracy=NA)[numeric(), ]

	size <- nrow(training_all)
	for(i in seq(from=0.05, to=1.0, by=0.05)){
		border_index <- floor(size * i)
		
		training <- training_all[seq(1,border_index),]
		if (nrow(training[training$re_evaluation == 0,]) >1 && nrow(training[training$re_evaluation == 1,]) >1 ) {
			
			#testing <- eval_train[- seq(1,border_index),]
			eval_fit <- buildModel(training, T)
			print(eval_fit$finalModel)
			predictions <- predict(eval_fit, testing)

			cf <- confusionMatrix(predictions, testing$re_evaluation)

			measurement <- c(i,nrow(training),nrow(testing),cf$table[1,1],cf$table[1,2],cf$table[2,1],cf$table[2,2],round(as.numeric(cf$overall["Accuracy"]),2),round(as.numeric(cf$byClass['Specificity']),2),round(as.numeric(cf$byClass['Sensitivity']),2))
			pred <- rbind(pred,measurement)
		}
		
	}
	colnames(pred) <- c("alpha","train_size","test_size","tn","fn","fp","tp","accuracy","specificity","sensitivity")
	pred
}
