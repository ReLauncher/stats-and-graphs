require(dplyr)
source("Approach/tasks_for_simulation.R")

predictMax<- function(durations, last_index){
	x <- durations
	indexes <- c(1:length(durations))

	i <-1

	current_slope <- 0
	slopes <- c()
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

	#points(y_indexes, y, col='red')

	hypothesis1.lm = lm(y ~ y_indexes)
	current_slope <-hypothesis1.lm$coefficients['y_indexes']

	current_prediction <- (last_index*current_slope)+hypothesis1.lm$coefficients['(Intercept)']
	current_prediction <- round(current_prediction)
	
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
			maxdur <- predictMax(durations_train,nrow(assignments_completed))
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
	write.table(assignments_been_relaunched, file = paste("Approach/",task_id,"_speed_",variable_start,"_linear.csv",sep=""))
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
