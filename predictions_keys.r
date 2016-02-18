library(sqldf)
library(caret)
source("Libs/graphs.R")
source("Libs/logs_into_features.R")

task_type <- "Images"

transcription1 <- prepareFeaturesDataset(854432,task_type,"https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=62480003")
transcription2 <- prepareFeaturesDataset(854753,task_type,"https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=94510901")

images1 <- prepareFeaturesDataset(854544,task_type,"https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=92134759")
images2 <- prepareFeaturesDataset(854885,task_type,"https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=224720013")
businessaddress1 <- prepareFeaturesDataset(854545,task_type,"https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=457902294")
businessaddress2 <- prepareFeaturesDataset(854883,task_type,"https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=549204533")

pred <- predictTaskAccuracy(transcription1,transcription2)
print(pred)


#eval_fit <- buildModel(eval_train, T)
#aban_fit <- buildModel(aban_set, F)

#eval_test <- prepareEvaluationTrainingSet(transcription1)
#predictions <- predict(eval_fit, eval_test)

#confusionMatrix(predictions, eval_test$re_evaluation)
#drawDecisionTree(eval_fit,aban_fit,paste(task_type,854885))
