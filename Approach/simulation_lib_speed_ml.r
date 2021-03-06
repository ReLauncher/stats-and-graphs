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

predictAbandonedLinear <- function(assignments_all, variable_start, variable_interval,task_id){
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
			maxdur <- predictMaxInterval(durations_train,nrow(assignments_completed))
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
predictAbandonedTree <- function(assignments_all, variable_start, variable_interval, task_id){
	time_start <- min(assignments_all$re_execution_relative_start)
	time_end <- max(assignments_all$re_execution_relative_end)
	time_duration <- ceiling(as.numeric(difftime(time_end, time_start, units = "secs")))
	
	assignments_completed <- filter(assignments_all, re_execution_relative_end < time_start+variable_start)
	empty_template <- assignments_all
	empty_template$relaunched <- NA
	empty_template$with_limit <- NA
	assignments_been_relaunched <- empty_template[FALSE,]	
}
createAbandonedTree <- function(assignments_all, variable_start, variable_interval, task_id){

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
	validation_summary$k_value <- 0
	saveSimulationResults(validation_summary, paste("speed_closed_",TASK_ID,sep=""))
}

simulate_speed_ml_runtime <- function(TASK_ID_TEST, GOOGLE_SPREADSHEET_TEST,TASK_ID_TRAIN, GOOGLE_SPREADSHEET_TRAIN, start = 1){
	task_type <- "Images"
	test_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TEST, task_type, GOOGLE_SPREADSHEET_TEST) #
	test_all <- prepareAbandonenceTrainingSet(test_dataset)
	test_filt <- select(test_all, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))
				
	training_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TRAIN, task_type, GOOGLE_SPREADSHEET_TRAIN) #
	pred <- data.frame(tn = NA, fn = NA, fp = NA, tp = NA, k_value= NA)[numeric(), ]

	filename <- paste("speed_ml_",TASK_ID_TEST,"_based_on_",TASK_ID_TRAIN,sep="")
	size <- nrow(training_dataset)
	if (1 ==1 ){
		for(i in seq(from=start, to=1.0, by=0.1)){
			for (j in seq(1:10)){
				border_index <- floor(size * i)
				training_subset <- training_dataset[seq(1,border_index),]
				aband_num <- nrow(training_subset[training_subset$abandoned == 1,])
				not_ab_num <- nrow(training_subset[training_subset$abandoned != 1,])
				print(i)
				if (aband_num >1 &&  not_ab_num >1 ) {
					training_clean <- prepareAbandonenceTrainingSet(training_subset)
					training_clean <- select(training_clean, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))
					
					print(paste(aband_num,not_ab_num,sep=" "))
					# print(nrow(training_clean))
					abandonence_model <- buildModel(training_clean,F,"rpart")
					print(paste("levels",nrow(abandonence_model$results),sep=" "))
					pdf(paste("Approach/Trees/",filename,".pdf",sep=""), width=6, height=3)
					prp(abandonence_model$finalModel,extra = 3)
					dev.off()
					print(abandonence_model$finalModel)
					predictions <- predict(abandonence_model, test_filt)

					cf <- confusionMatrix(predictions, test_all$abandoned)
					print(cf$table)
					measurement <- c(cf$table[1,1],cf$table[1,2],cf$table[2,1],cf$table[2,2],i)
					pred <- rbind(pred,measurement)

				}else{
					print("none")
					#measurement <- c(0,nrow(test_all[test_all$abandoned != 1,]),nrow(test_all[test_all$abandoned == 1,]),0,i)	
				}
			}
		}
	}
	colnames(pred) <- c("tn","fn","fp","tp","k_value")
	saveSimulationResults(pred, filename)
}

