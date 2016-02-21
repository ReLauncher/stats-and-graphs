library(sqldf)
library(caret)
library(rpart)
library(rpart.plot)

source("Libs/graphs.R")
source("Libs/logs_into_features.R")
source("Approach/linearReg.r")

simulate_accuracy_ml <- function(TASK_ID_TEST, GOOGLE_SPREADSHEET_TEST,TASK_ID_TRAIN, GOOGLE_SPREADSHEET_TRAIN){
	task_type <- "Images"
	
	test_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TEST, task_type, GOOGLE_SPREADSHEET_TEST) #
	test_all <- prepareEvaluationTrainingSet(test_dataset)
	test_filt <- select(test_all, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))
				
	training_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TRAIN, task_type, GOOGLE_SPREADSHEET_TRAIN) #
	pred <- data.frame(tn = NA, fn = NA, fp = NA, tp = NA, k_value= NA)[numeric(), ]

	size <- nrow(training_dataset)
	for(i in seq(from=0.00, to=1.0, by=0.1)){
		border_index <- floor(size * i)
		training_subset <- training_dataset[seq(1,border_index),]
		quality_high <- nrow(training_subset[training_subset$re_evaluation == 1,])
		quality_low <- nrow(training_subset[training_subset$re_evaluation == 0,])
		print(i)
		if (quality_high >1 &&  quality_low >1 ) {
			training_clean <- prepareEvaluationTrainingSet(training_subset)
			training_clean <- select(training_clean, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))
			print(paste(quality_high,quality_low,sep=" "))
			quality_model <- buildModel(training_clean,T,"rpart")
			predictions <- predict(quality_model, test_filt)

			cf <- confusionMatrix(predictions, test_all$re_evaluation)
			print(cf$table)
			measurement <- c(cf$table[1,1],cf$table[1,2],cf$table[2,1],cf$table[2,2],i)
			pred <- rbind(pred,measurement)
		}else{
			print("none")	
		}
	}
	colnames(pred) <- c("tn","fn","fp","tp","k_value")
	saveSimulationResults(pred, paste("accuracy_ml_",TASK_ID_TEST,"_based_on_",TASK_ID_TRAIN,sep=""))
}

for(m in seq(2:nrow(SIMULATION_TASKS))){
	for(k in seq(2:nrow(SIMULATION_TASKS))){
		print(paste(m,k,sep=" "))
		simulate_accuracy_ml(SIMULATION_TASKS[m,"id"],SIMULATION_TASKS[m,"spreadsheet"],SIMULATION_TASKS[k,"id"],SIMULATION_TASKS[k,"spreadsheet"])
	}
}



