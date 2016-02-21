library(sqldf)
library(caret)
library(rpart)
library(rpart.plot)

source("Libs/graphs.R")
source("Libs/logs_into_features.R")
source("Approach/linearReg.r")

task_type <- "Images"
# =======================================
TASK_ID_TRAIN <- 854432
GOOGLE_SPREADSHEET_TRAIN <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=62480003"

#TASK_ID_TRAIN <- 854546
#GOOGLE_SPREADSHEET_TRAIN <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=1916343054"
TASK_ID_TEST <- 854753
GOOGLE_SPREADSHEET_TEST <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=94510901"

# =======================================


test_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TEST, task_type, GOOGLE_SPREADSHEET_TEST) #
test_all <- prepareEvaluationTrainingSet(test_dataset)
test_filt <- select(test_all, -(assignment_id),-(re_execution_relative_end),-(assignment_start),-(assignment_end))
			
training_dataset <- prepareFeaturesAssignmentsDataset(TASK_ID_TRAIN, task_type, GOOGLE_SPREADSHEET_TRAIN) #
pred <- data.frame(tn = NA, fn = NA, fp = NA, tp = NA, k_value= NA)[numeric(), ]


size <- nrow(training_dataset)
if (1 ==1 ){
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
			# print(nrow(training_clean))
			quality_model <- buildModel(training_clean,T,"rpart")
			#print(abandonence_model$finalModel)
			predictions <- predict(quality_model, test_filt)

			cf <- confusionMatrix(predictions, test_all$re_evaluation)
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
saveSimulationResults(pred, paste("accuracy_ml_",TASK_ID_TEST,"_based_on_",TASK_ID_TRAIN,sep=""))

