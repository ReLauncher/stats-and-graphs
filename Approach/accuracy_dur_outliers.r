library(sqldf)
library(caret)

source("Libs/graphs.R")
source("Libs/logs_into_features.R")
source("Approach/linearReg.r")

task_type <- "Images"
# =======================================
#TASK_ID <- 854546
#GOOGLE_SPREADSHEET <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=1916343054"

#TASK_ID <- 854432
#GOOGLE_SPREADSHEET <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=62480003"

TASK_ID <- 854753
GOOGLE_SPREADSHEET <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=94510901"
# =======================================
results <- prepareUnitResults(TASK_ID,task_type,GOOGLE_SPREADSHEET)
durations <- results$re_duration_num
simulations <- data.frame(tp=NA, tn=NA, fp=NA,fn = NA,precision = NA)[numeric(), ]

getPercentile <- function(distribution, perc){
	q <-quantile(distribution, c(perc))
	as.numeric(q[paste(floor(perc*100),"%",sep="")])
}
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
