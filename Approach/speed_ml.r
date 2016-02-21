library(sqldf)
library(caret)
library(rpart)
library(rpart.plot)

source("Libs/graphs.R")
source("Libs/logs_into_features.R")
source("Approach/linearReg.r")

task_type <- "Images"

simulate_speed_ml <- function(TASK_ID_TEST, GOOGLE_SPREADSHEET_TEST,TASK_ID_TRAIN, GOOGLE_SPREADSHEET_TRAIN){
	test_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TEST, task_type, GOOGLE_SPREADSHEET_TEST) #
	test_all <- prepareAbandonenceTrainingSet(test_dataset)
	test_filt <- select(test_all, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))
				
	training_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TRAIN, task_type, GOOGLE_SPREADSHEET_TRAIN) #
	pred <- data.frame(tn = NA, fn = NA, fp = NA, tp = NA, k_value= NA)[numeric(), ]


	size <- nrow(training_dataset)
	if (1 ==1 ){
		for(i in seq(from=0.00, to=1.0, by=0.1)){
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
				#print(abandonence_model$finalModel)
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
	colnames(pred) <- c("tn","fn","fp","tp","k_value")
	saveSimulationResults(pred, paste("speed_ml_",TASK_ID_TEST,"_based_on_",TASK_ID_TRAIN,sep=""))
}

for(i in seq(1:nrow(SIMULATION_TASKS))){
	for(j in seq(1:nrow(SIMULATION_TASKS))){
		simulate_speed_ml(SIMULATION_TASKS[i,"id"],SIMULATION_TASKS[i,"spreadsheet"],SIMULATION_TASKS[j,"id"],SIMULATION_TASKS[j,"spreadsheet"])
	}
}

