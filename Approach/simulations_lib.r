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
			sum(case when relaunched = 1 and re_evaluation = -10 then 1 else 0 end) as tp,\r
			sum(case when relaunched = 0 and re_evaluation in (-2,0,1) then 1 else 0 end) as tn,\r
			sum(case when relaunched = 1 and re_evaluation in (-2,0,1) then 1 else 0 end) as fp,\r
			sum(case when relaunched = 0 and re_evaluation = -10 then 1 else 0 end)  as fn\r
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
	simulations <- data.frame(tp=NA, tn=NA, fp=NA,fn = NA,precision = NA, recall = NA,k_value=NA)[numeric(), ]

	for (pred_int in seq(0.90, 0.99,by = 0.01)){
		assignments_all <-logs_for_timeline
		for (k in seq(0.1,1.0, by = 0.1)){
			relaunched <- predictAbandonedLinear(assignments_all,k,10,TASK_ID)
			relaunching_summary <- summaryAlgorithm(assignments_all,relaunched)
			relaunching_summary$k_value = k
			relaunching_summary$pred_int = pred_int
			simulations <- rbind(simulations,relaunching_summary)
		}
	}
	
	saveSimulationResults(simulations, paste("speed_linear_",TASK_ID,sep=""))
}


simulate_speed_ml <- function(TASK_ID_TEST, GOOGLE_SPREADSHEET_TEST,TASK_ID_TRAIN, GOOGLE_SPREADSHEET_TRAIN, start = 1){
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

simulate_accuracy_outliers <- function(TASK_ID,GOOGLE_SPREADSHEET){
	task_type <- "Images"
	results <- prepareUnitResults(TASK_ID,task_type,GOOGLE_SPREADSHEET)
	durations <- results$re_duration_num
	print(durations)
	simulations <- data.frame(tp=NA, tn=NA, fp=NA,fn = NA,precision = NA)[numeric(), ]

	for (i in c(seq(from=0.0, to=0.2, by=0.01),seq(from=0.25, to=1, by=0.05))){
		limit <- getPercentile(durations, i)
		print(i)
		print("Limit")
		print(limit)
		subset <- results
		subset$relaunched <- 0
		subset[subset$re_duration_num<=limit,"relaunched"] <- 1
		
		validation_summary <- sqldf("\r
			select \r
				sum(case when relaunched = 1 and re_evaluation = 0 then 1 else 0 end) as tp,\r
				sum(case when relaunched = 0 and re_evaluation = 1 then 1 else 0 end) as tn,\r
				sum(case when relaunched = 1 and re_evaluation = 1 then 1 else 0 end) as fp,\r
				sum(case when relaunched = 0 and re_evaluation = 0 then 1 else 0 end) as fn\r
			from subset 
			")
		validation_summary$k_value = i
		simulations <- rbind(simulations,validation_summary)	
	}
	saveSimulationResults(simulations, paste("accuracy_outliers_",TASK_ID,sep=""))

}

simulate_accuracy_ml <- function(TASK_ID_TEST, GOOGLE_SPREADSHEET_TEST,TASK_ID_TRAIN, GOOGLE_SPREADSHEET_TRAIN, start = 1){
	task_type <- "Images"
	test_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TEST, task_type, GOOGLE_SPREADSHEET_TEST) #
	test_all <- prepareEvaluationTrainingSet(test_dataset)

	filename <- paste("accuracy_ml_",TASK_ID_TEST,"_based_on_",TASK_ID_TRAIN,sep="")
	print(test_all)
	if (length(levels(test_all$re_evaluation))>1){
		test_filt <- select(test_all, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))		
		training_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TRAIN, task_type, GOOGLE_SPREADSHEET_TRAIN) #
		pred <- data.frame(tp = NA, fp = NA, fn = NA, tn = NA, k_value= NA)[numeric(), ]

		size <- nrow(training_dataset)
		for(i in seq(from=start, to=1.0, by=0.1)){
			for (j in seq(1:10)){
				border_index <- floor(size * i)
				training_subset <- training_dataset[seq(1,border_index),]
				quality_high <- nrow(training_subset[training_subset$re_evaluation == '1',])
				quality_low <- nrow(training_subset[training_subset$re_evaluation == '0',])
				print(i)
				print(quality_high)
				print(quality_low)
				if (quality_high >1 && quality_low >1) {
					training_clean <- prepareEvaluationTrainingSet(training_dataset)
					training_clean <- select(training_clean, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))
					print(str(training_clean))
					print(paste(quality_high,quality_low,sep=" "))

					quality_model <- buildModel(training_clean,T,"rpart")
					print(quality_model$finalModel)
					pdf(paste("Approach/Trees/",filename,".pdf",sep=""), width=6, height=3)
					prp(quality_model$finalModel,extra = 3)
					dev.off()
					predictions <- predict(quality_model, test_filt)

					cf <- confusionMatrix(predictions, test_all$re_evaluation)
					print(cf$table)
					measurement <- c(cf$table[1,1],cf$table[1,2],cf$table[2,1],cf$table[2,2],i)
					pred <- rbind(pred,measurement)
				}else{
					print("none")	
				}
			}
		}
		colnames(pred) <- c("tp","fp","fn","tn","k_value")
		saveSimulationResults(pred, filename)
	}else{
		print("the length of test is smaller than 1")
	}
}
