source("graphs.R")
library(RCurl)
require(scales)
library("caret")
source("collect_experiments.R")
library(downloader)
library(lubridate)


GOOGLE_SPREADSHEET_URL <-"https://docs.google.com/spreadsheets/d/1DR4_TytGVSE-KyaZGiSpRmd8Ep-wMotK1JxPcPkNBXg/edit#gid=0"
experiments <- collectFromGoogleSpreadsheet(GOOGLE_SPREADSHEET_URL)
experiments$evaluation_url <- as.character(experiments$evaluation_url)

execution_data <- FALSE
for (i in 1:nrow(experiments)){
	experiment <- collectFromGoogleSpreadsheet(experiments[i,"evaluation_url"])
	print(experiments[i,])
	# ============================================
	experiment <- reformatExperimentData(experiment)
	plotHistogram(experiment)
	#predictEvaluation(experiment);

	if (execution_data == FALSE){
		execution_data <- experiment
	}else{
		execution_data <- rbind(execution_data, experiment)
	}
}

execution_data
# training <- execution_data[,grep("re_", names(execution_data), value=TRUE)]
