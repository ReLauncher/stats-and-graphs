library(sqldf)
library(caret)
library(rpart)
library(rpart.plot)
require(dplyr)

source("Approach/tasks_for_simulation.R")
source("Libs/graphs.R")
source("Libs/logs_into_features.R")
predictMaxInterval<- function(durations, last_index, prediction_width = 0.95){
	indexes <- c(1:length(durations))

	linear_regres_model = lm(durations ~ indexes)

	prediction <- predict(linear_regres_model, newdata=list(indexes=last_index), interval = "prediction", level=prediction_width)
	current_limit <- round(as.numeric(prediction[1,'upr']))

	current_limit
}


summaryAlgorithm <- function(assignments_all,assignments_been_relaunched){
	validation <- sqldf("\r
		select a.*, case when br.assignment_id is null then 0 else 1 end as relaunched
		from assignments_all a\r
		left join assignments_been_relaunched br on a.assignment_id = br.assignment_id\r
		")
	validation_summary <- sqldf("\r
		select \r
			sum(case when relaunched = 1 and re_evaluation = -1 then 1 else 0 end) as tp,\r
			sum(case when relaunched = 0 and re_evaluation in (-2,0,1) then 1 else 0 end) as tn,\r
			sum(case when relaunched = 1 and re_evaluation in (-2,0,1) then 1 else 0 end) as fp,\r
			sum(case when relaunched = 0 and re_evaluation = -1 then 1 else 0 end)  as fn\r
		from validation 
		")

	validation_summary
}

predictAbandonedLinear <- function(assignments_all, variable_start, variable_interval,task_id, pred_int = 0.95){
	assignments_completed <- filter(assignments_all, re_evaluation==0 | re_evaluation==1)
	#assignments_completed <- assignments_all[assignments_all$re_evaluation == 0 || assignments_all$re_evaluation == 1,]

	time_start <- min(assignments_all$re_execution_relative_start)
	time_end <- max(assignments_all$re_execution_relative_end)
	time_duration <- ceiling(as.numeric(difftime(time_end, time_start, units = "secs")))
	empty_template <- assignments_all
	empty_template$relaunched <- NA
	empty_template$with_limit <- NA
	assignments_been_relaunched <- empty_template[FALSE,]

	begin_assignment_index <- floor(variable_start*nrow(assignments_completed))
	if (begin_assignment_index == 0){
		begin_assignment_index <- 1
	} 
	begin_point <- ceiling(as.numeric(difftime(assignments_completed[begin_assignment_index,"re_execution_relative_start"],time_start, units = "secs")))
	print(begin_point)
	for (i in seq(begin_point, time_duration, by=variable_interval)){
		current_time <- time_start+i
		# -----------------------------
		# calculate MAX DURATION
		assignments_train <- filter(assignments_completed,re_execution_relative_end<= current_time)
		durations_train <-assignments_train$re_duration_num
		#print(length(durations_train))

		if (length(durations_train)>0){
			#maxdur <- 1681
			maxdur <- predictMaxInterval(durations_train,nrow(assignments_completed),pred_int)
			# ----------------------------
			not_relaunched <- anti_join(assignments_all,select(assignments_been_relaunched, -(relaunched:with_limit)))
			
			assignments_relaunched <- filter(not_relaunched, re_execution_relative_start < current_time, re_execution_relative_end > current_time, re_execution_relative_start < current_time - maxdur)
			#print(nrow(assignments_relaunched))
			if (nrow(assignments_relaunched) > 0){
				assignments_relaunched$relaunched <- i
				assignments_relaunched$with_limit <- maxdur
				assignments_been_relaunched <- rbind(assignments_been_relaunched,assignments_relaunched)
			}
			#print(maxdur)
		}
	}
	write.table(assignments_been_relaunched, file = paste("Approach/Simulations/Details/",task_id,"_speed_",variable_start,"_linear.csv",sep=""),sep=",",row.names = F)
	assignments_been_relaunched
}

predictAbandonedLinearTime <- function(assignments_all, variable_start, variable_interval,task_id, pred_int = 0.95){
	assignments_completed <- filter(assignments_all, re_evaluation==0 | re_evaluation==1)
	#assignments_completed <- assignments_all[assignments_all$re_evaluation == 0 || assignments_all$re_evaluation == 1,]

	time_start <- min(assignments_all$re_execution_relative_start)
	time_end <- max(assignments_all$re_execution_relative_end)
	time_duration <- ceiling(as.numeric(difftime(time_end, time_start, units = "secs")))
	empty_template <- assignments_all
	empty_template$relaunched <- NA
	empty_template$with_limit <- NA
	assignments_been_relaunched <- empty_template[FALSE,]

	begin_point <- time_start + variable_start
	if (begin_point>time_end){
		begin_point <- time_end
	}
	print(begin_point)
	
	for (current_time in seq(begin_point, time_end, by=variable_interval)){
		# -----------------------------
		# calculate MAX DURATION
		assignments_train <- filter(assignments_completed,re_execution_relative_end<= current_time)
		durations_train <-assignments_train$re_duration_num
		#print(length(durations_train))

		if (length(durations_train)>0){
			#maxdur <- 1681
			maxdur <- predictMaxInterval(durations_train,nrow(assignments_completed),pred_int)
			# ----------------------------
			not_relaunched <- anti_join(assignments_all,select(assignments_been_relaunched, -(relaunched:with_limit)))
			
			assignments_relaunched <- filter(not_relaunched, re_execution_relative_start < current_time, re_execution_relative_end > current_time, re_execution_relative_start < current_time - maxdur)
			#print(nrow(assignments_relaunched))
			if (nrow(assignments_relaunched) > 0){
				assignments_relaunched$relaunched <- i
				assignments_relaunched$with_limit <- maxdur
				assignments_been_relaunched <- rbind(assignments_been_relaunched,assignments_relaunched)
			}
			#print(maxdur)
		}
	}
	write.table(assignments_been_relaunched, file = paste("Approach/Simulations/Details/",task_id,"_speed_",variable_start,"_linear.csv",sep=""),sep=",",row.names = F)
	assignments_been_relaunched
}

augmentPrecisionRecall <- function(simulation){
	simulation <- sqldf("select tp,tn,fp,fn, \r
				round(1.0*tp/(tp+fp),2) as precision, round(1.0*tp/(tp+fn),2) as recall, k_value, pred_int\r
			from simulation v")
	simulation
}
saveSimulationResults <- function(simulation,filename, folder = "Simulations/"){
	simulation<-augmentPrecisionRecall(simulation)
	write.table(simulation, paste("Approach/",folder, filename,".csv",sep = ""),sep=",",row.names = F)
}
getPercentile <- function(distribution, perc){
	q <-quantile(distribution, c(perc))
	as.numeric(q[paste(floor(perc*100),"%",sep="")])
}

simulate_speed_linear <- function(TASK_ID, GOOGLE_SPREADSHEET){
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
	logs_for_timeline$re_unit_id <- as.factor(logs_for_timeline$re_unit_id)
	logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_end"] <- logs_for_timeline[logs_for_timeline$re_evaluation == -1,"re_execution_relative_start"]+1800

	durat <- difftime(logs_for_timeline$re_execution_relative_end, logs_for_timeline$re_execution_relative_start, units = "secs") 
	logs_for_timeline$re_duration_num <- as.numeric(durat)
	simulations <- data.frame(tp=NA, tn=NA, fp=NA,fn = NA,precision = NA, recall = NA,k_value=NA,pred_int = NA)[numeric(), ]

	for (pred_int in c(0.9,0.95,0.99)){
		assignments_all <-logs_for_timeline
		#for (k in seq(0.1,0.9, by = 0.2)){
		for (k in seq(from=0.1,to=0.9,by=0.1)){
			relaunched <- predictAbandonedLinear(assignments_all,k,10,TASK_ID, pred_int)
			relaunching_summary <- summaryAlgorithm(assignments_all,relaunched)
			relaunching_summary$k_value = k
			relaunching_summary$pred_int = pred_int
			simulations <- rbind(simulations,relaunching_summary)
		}
	}
	#saveSimulationResults(simulations, paste("speed_linear_",TASK_ID,sep=""), "Simulations/")
	saveSimulationResults(simulations, paste("speed_linear_",TASK_ID,sep=""), "Simulations/Time/")
}
