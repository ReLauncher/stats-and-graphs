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

predictMaxLocals<- function(durations, last_index, prediction_width = 0.95){
	x <- durations
	# get assignment indexes
	indexes <- c(1:length(durations))

	i <-1

	y <-c()
	y[1] <- x[1]
	y_indexes <-c(1)
	while(i < length(x)){
		i<- i+1
		if (x[i]>y[length(y_indexes)]){
			y_indexes[length(y_indexes)+1]<- i
			y[length(y_indexes)] <- x[i]
		}
	}

	current_slope <- 0
	linear_regres_model = lm(y ~ y_indexes)
	current_slope <-linear_regres_model$coefficients['y_indexes']
	
	current_prediction <- (last_index*current_slope)+linear_regres_model$coefficients['(Intercept)']
	current_prediction <- round(as.numeric(current_prediction))
	
	print("Current prediction is:")
	print(current_prediction)
	#abline(hypothesis1.lm, col=alpha('grey', 0.5))
	#abline(current_prediction,0, col=alpha('blue', 0.5))
	#dev.off()
	current_prediction
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
	#validation_summary <- sqldf("\r
	#	select \r
	#		v.*, 1.0*tp/(tp+fp) as precision, 1.0*tp/(tp+fn) as recall\r
	#	from validation_summary v
	#	")
	validation_summary
}
saveTimelinePlot <- function(timeline_data, task_id, width, height){
	tl <- plotTimeline(timeline_data, width = width, height = height, faceting = F, 
		tl_bar_detail_show = F,
		tl_bar_detail = "re_unit_id", 
		tl_bar_color = "re_evaluation", 
		tl_bar_detail_color = "green",
		tl_y = "re_index",	
		tl_y_coord = "re_index",	
		tl_facet_a = "assignment_id",	
		tl_facet_b = "assignment_id",	
		tl_breaks_major = "10 min",
		tl_breaks_minor = "5 min",
		tl_breaks_format = "%H:%M",
		tl_title_x = "Time since the task launch",
		tl_title_y = "Unit ID")
	ggsave(tl, file=paste("Approach/timeline_",task_id,".pdf",sep=""), width=width, height=height)
}

augmentPrecisionRecall <- function(simulation){
	simulation <- sqldf("select tp,tn,fp,fn, \r
				round(1.0*tp/(tp+fp),2) as precision, round(1.0*tp/(tp+fn),2) as recall, k_value\r
			from simulation v")
	simulation
}
saveSimulationResults <- function(simulation,filename){
	simulation<-augmentPrecisionRecall(simulation)
	write.table(simulation, paste("Approach/Simulations/",filename,".csv",sep = ""),sep=",",row.names = F)
}
getPercentile <- function(distribution, perc){
	q <-quantile(distribution, c(perc))
	result <- as.numeric(q[paste(round(perc*100,0),"%",sep="")])

	result
}

simulate_speed_closed_tabs <- function(TASK_ID,GOOGLE_SPREADSHEET){
	task_type <- "Images"
	results <- prepareUnitResults(TASK_ID,task_type,GOOGLE_SPREADSHEET)

	assignments <- prepareAssignments(TASK_ID)
	tabs <- prepareTabActivityLogs(TASK_ID)
	gaveups <- prepareGiveUpClicks(TASK_ID)

	dumb_start_time <- as.POSIXct("07/01/2015 00:00:00", format='%m/%d/%Y %H:%M:%S')
	assignments$first_execution_start <- min(assignments$dt_start)
	assignments$re_execution_relative_end <- dumb_start_time + (assignments$dt_end - assignments$first_execution_start)
	assignments$re_execution_relative_start <- dumb_start_time + (assignments$dt_start - assignments$first_execution_start)

	tabs$first_execution_start <- min(tabs$session_start)
	tabs$re_execution_relative_end <- as.numeric(dumb_start_time) + (tabs$session_end - tabs$first_execution_start)
	tabs$re_execution_relative_start <- as.numeric(dumb_start_time) + (tabs$session_start - tabs$first_execution_start)

	simulations <- data.frame(tp=NA, tn=NA, fp=NA,fn = NA,precision = NA, recall = NA,k_value=NA)[numeric(), ]
	for (k in seq(from=0,to=5,by=0.5)){
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
				abs(r.re_execution_relative_end - ct.re_execution_relative_start) as dd, \r
				ct.sessions, \r
				a.assignment_id \r
			from results r \r
			left join assignments a on a.unit_id = r.re_unit_id \r
			left join (
				select 
					dt_start,
					count(*) as sessions,
					assignment_id
				from tabs 
				where status = ' closed' and dt_start < '",break_time,"' \r
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
		validation_summary$k_value <- 0
	}
		
	saveSimulationResults(validation_summary, paste("speed_closed_",TASK_ID,sep=""))
}
