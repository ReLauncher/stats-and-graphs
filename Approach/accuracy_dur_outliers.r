library(sqldf)
library(caret)

source("Libs/graphs.R")
source("Libs/logs_into_features.R")
source("Approach/linearReg.r")


simulate_accuracy_outliers <- function(TASK_ID,GOOGLE_SPREADSHEET){
	task_type <- "Images"
	results <- prepareUnitResults(TASK_ID,task_type,GOOGLE_SPREADSHEET)
	durations <- results$re_duration_num
	simulations <- data.frame(tp=NA, tn=NA, fp=NA,fn = NA,precision = NA)[numeric(), ]

	for (i in seq(from=0.0, to=0.2, by=0.01)){
		limit <- getPercentile(durations, i)
		print(i)
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

for(i in seq(1:nrow(SIMULATION_TASKS))){
	simulate_accuracy_outliers(SIMULATION_TASKS[i,"id"],SIMULATION_TASKS[i,"spreadsheet"])
}