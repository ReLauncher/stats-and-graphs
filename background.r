source("Libs/collect.R")
source("Libs/graphs.R")
require(scales)

task_labeling_1 <- collectResultsCrowdFlower(837728, "Labeling", F,F,";")
task_labeling_2 <- collectResultsCrowdFlower(837900, "Labeling", F,F,";")
task_labeling <- rbind(task_labeling_1,task_labeling_2)
task_labeling$Evaluation <- as.factor(task_labeling$Evaluation)
str(task_labeling)
plotHistogram(task_labeling)
plotDotsDurationVSIndexAccuracy(task_labeling)

task_transcription_1 <- collectResultsCrowdFlower(843958, "Transcription", F,F,";")
task_transcription_2 <- collectResultsCrowdFlower(843959, "Transcription", F,F,";")
task_transcription <- rbind(task_transcription_1,task_transcription_2)
plotHistogram(task_transcription)
plotDotsDurationVSIndexAccuracy(task_transcription)

task_search_1 <- collectResultsCrowdFlower(843920, "Searching", F,F,";")
task_search_2 <- collectResultsCrowdFlower(843953, "Searching", F,F,";")
task_search <- rbind(task_search_1,task_search_2)
plotHistogram(task_search)
plotDotsDurationVSIndexAccuracy(task_search)