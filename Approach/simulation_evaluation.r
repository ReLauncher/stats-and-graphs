source("Approach/tasks_for_simulation.r")
require(dplyr)
library(plyr)
library(ggplot2)
library(latticeExtra)

library(scatterplot3d)

getPrecisionRecall <- function(string_1, task_id_1,string_2="",task_id_2=""){
	filename <- paste("Approach/Simulations/",string_1,task_id_1,string_2,task_id_2,".csv",sep="")
	result <- -21
	if (file.exists(filename)){
		p_data <- read.table(filename, header=T, sep="," ,quote = "\"", comment.char = "")	
		if (nrow(p_data)>0){
			#print(p_data)
			result <- p_data
		}
	}

	result
}



evaluations<- -21
if (1 == 2){
file_type_1 <- "accuracy_outliers_"
file_type_2 <- ""
	for(k in seq(1:length(TASK_ID))){
		#print(paste(m,k,sep=" "))
		new_evaluation<- getPrecisionRecall(file_type_1,TASK_ID[k])
		print(new_evaluation)
		if (new_evaluation != -21){
			new_evaluation$task_id_1 <- TASK_ID[k]
			if (evaluations == -21){
				evaluations<- new_evaluation
			}else{
				evaluations<- rbind(evaluations,new_evaluation)
			}
		}
	}
}


if (1 == 1){
file_type_1 <- "speed_ml_"
file_type_2 <- "_based_on_"
	for(m in seq(1:length(TASK_ID))){
		for(k in seq(1:length(TASK_ID))){
			if (m!=k){
				#print(paste(m,k,sep=" "))
				new_evaluation<- getPrecisionRecall(file_type_1,TASK_ID[m],file_type_2,TASK_ID[k])
				print(new_evaluation)
				if (new_evaluation != -21){
					new_evaluation$task_id_1 <- TASK_ID[m]
					new_evaluation$task_id_2 <- TASK_ID[k]
					if (evaluations == -21){
						evaluations<- new_evaluation
					}else{
						
						evaluations<- rbind(evaluations,new_evaluation)
					}
				}
			}
		}
	}
}


print(evaluations)
evaluations<- na.omit(evaluations)
evaluations$k_value <- as.factor(evaluations$k_value)

evaluations_agr <- ddply(evaluations,~k_value,summarise, precision = median(precision), recall = median(recall), precision_Q1 = as.numeric(quantile(precision)[2]), precision_Q3 = as.numeric(quantile(precision)[4]), recall_Q1 = as.numeric(quantile(recall)[2]), recall_Q3 = as.numeric(quantile(recall)[4]))

print(evaluations_agr)

pl <- ggplot(evaluations_agr)
pl <- pl+
	geom_path(aes(x=recall, y=precision),color = "grey", size=1)+
	geom_rect(aes(color = k_value, xmin = recall_Q1, xmax = recall_Q3, ymin = precision_Q1, ymax = precision_Q3),fill = NA, size=0.5)+
	geom_point(aes(x=recall, y=precision),color = "grey", size=10, alpha = 0.8)+
	geom_text(aes(x=recall, y=precision,label=k_value))+
	scale_x_continuous(limits = c(0.0, 1.0))+scale_y_continuous(limits = c(0.0, 1.0))+
	scale_color_discrete(guide=FALSE)

print(pl)
ggsave(pl, file="Approach/Plots/speed_ml.pdf", width=12, height=4)