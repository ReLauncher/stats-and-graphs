#####################################################################
# Data Collection Module
# STATS-AND-GRAPHS
#####################################################################

#library(downloader)
#library(lubridate)
#library(scales)
#library(digest)

source("secret.R")

dumb_start_time <- as.POSIXct("01/01/2015 00:00:00", format='%m/%d/%Y %H:%M:%S')

collectResultsCrowdFlower <- function(job_id, title = "Task", download = T){
	destfile = paste(DATASET_FOLDER,"/",job_id,".zip", sep="")

	if (download){
		download(paste("https://api.crowdflower.com/v1/jobs/",job_id,".csv?type=full&key=",CROWDFLOWER_SECRET_KEY, sep = ""), mode = "wb", destfile = destfile)
		print(paste("File downloaded: ",destfile, sep=" "))
	}
	
	data <- read.table(unz(destfile, paste("f",job_id,".csv", sep="")), header=T, sep=",",quote = "\"",comment.char = "")
	data <- processResultsCrowdFlower(data, job_id, title);
	
	print(paste("The collected dataset has rows:",nrow(data), sep=" "))
	
	data
}
processResultsCrowdFlower <- function(data, job_id, title){
	# METADATA
	data$job_id <- job_id
	data$task <- title
	data$worker_id <-as.character(substring(data$X_worker_id,5,8))
	data$platform <- "CrowdFlower"
	
	# DATETIMES
	data$execution_end <- mdy_hms(data$X_created_at)
	data$execution_start <- mdy_hms(data$X_started_at)

	data$duration <- difftime(data$execution_end, data$execution_start, units = "secs") 
	data$duration_num <- as.numeric(data$duration)
	data$first_execution_start <- min(data$execution_start)

	data$execution_relative_end <- dumb_start_time + (data$execution_end - data$first_execution_start)
	data$execution_relative_start <- data$execution_relative_end - data$duration
	data$execution_relative_start_num <- as.numeric(data$execution_relative_start - dumb_start_time)
	data$execution_relative_end_num <- as.numeric(data$execution_relative_end - dumb_start_time)
	
	# STORE UNIT INDEXES
	data$unit_id <- factor(data$X_unit_id, levels = data$X_unit_id)
	data <- data[order(data$unit_id),] 
	data$unit_number <- as.numeric(rownames(data))
	# in order to preserve the order
	# bug fix - if the duration is in minutes and not seconds
	#data[data$duration_num<10,]$duration_num<-data[data$duration_num<10,]$duration_num*60
	
	data

}
