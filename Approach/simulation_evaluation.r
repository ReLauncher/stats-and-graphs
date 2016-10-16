source("Approach/tasks_for_simulation.r")
require(dplyr)
library(plyr)
library(ggplot2)
library(ggrepel)
library(latticeExtra)

library(scatterplot3d)

task_list <- getSimulationTaskList()

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
if (TRUE){
	file_type_1 <- "Speed_closed_test_"
	file_type_2 <- ""
	for(k in seq(1:9)){
		print(k)
		new_evaluation<- getPrecisionRecall(file_type_1,task_list[k,1])
		print(new_evaluation)
		if (new_evaluation != -21){
			new_evaluation$task_id_1 <- task_list[k,1]
			new_evaluation$task_type <- task_list[k,3]
			
			if (evaluations == -21){
				evaluations<- new_evaluation
			}else{
				evaluations<- rbind(evaluations,new_evaluation)
			}
		}
	}
}


if (FALSE){
file_type_1 <- "accuracy_ml_"
file_type_2 <- "_based_on_"
	for(m in seq(1:9)){
		for(k in seq(1:9)){
			if (m!=k){
				#print(paste(m,k,sep=" "))
				new_evaluation<- getPrecisionRecall(file_type_1,task_list[m,1],file_type_2,task_list[k,1])
				print(new_evaluation)
				if (new_evaluation != -21){
					new_evaluation$task_id_1 <- task_list[m,1]
					new_evaluation$task_id_2 <- task_list[k,1]
					new_evaluation$task_type <- task_list[m,3]
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
#evaluations <- evaluations[evaluations$k_value != 0,]
#evaluations$k_value <- as.factor(evaluations$k_value)
evaluations_agr <- ddply(evaluations,c("k_value", "task_type"),summarise, precision = mean(precision), recall = mean(recall))
#evaluations_agr <- evaluations_agr[evaluations_agr$pred_int >=0.9,]
print(evaluations_agr)

#evaluations_agr$k_value <- as.factor(evaluations_agr$k_value)
evaluations_agr$k_value <- (evaluations_agr$k_value)
#evaluations_agr$pred_int <- as.factor(evaluations_agr$pred_int)
pl <- ggplot(evaluations_agr,aes(x=recall, y=precision))

pl <- pl+
	geom_line(size = 1, alpha = 0.8)+
	#scale_colour_brewer(palette = "Oranges")+
	scale_size(guide = FALSE)+
	#scale_colour_continuous(low="red", high="blue")+
	#geom_rect(aes(color = k_value, xmin = recall_Q1, xmax = recall_Q3, ymin = precision_Q1, ymax = precision_Q3),fill = NA, size=0.5)+
	facet_grid(. ~ task_type) +
	#geom_line(size = 2, alpha = 0.7)+
	geom_point(size = 3, color = rgb(red= 58,green=145,blue= 255, maxColorValue = 255))+
	geom_point(size = 3, shape = 1, color = "black")+
	geom_text_repel(aes(label = paste(k_value,' mins',sep="")),size = 4) +
	#geom_text(aes(x=recall, y=precision,label=k_value), size = 4, color = "black", alpha = 0.7)+
	scale_x_continuous(limits = c(0.0, 1.0))+scale_y_continuous(limits = c(0.0, 1.0))+
	theme(legend.position = "bottom", legend.box = "horizontal", legend.key.size = unit(0.1, "cm"), strip.text = element_text(size = 16),text = element_text(size=14, color = "black"), axis.text = element_text(size=10, color = "black"))
	#labs(color='Prediction interval', shape = "Waiting time, minutes") 
	#guides(colour=guide_colourbar(barheight=10,label.position="right"))

print(pl)
filename = file_type_1
write.table(evaluations_agr, file = paste("Approach/Plots/",filename,".csv", sep=""), sep=",")
ggsave(pl, file = paste("Approach/Plots/time_",filename,".pdf", sep=""), width=9, height=3)