#####################################################################
# Graph Generation Module
# STATS-AND-GRAPHS
#####################################################################

# download required libraries
# https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/
library(ggplot2)
library(latticeExtra)
source("secret.R")

# library(scatterplot3d)
# limit_left <- as.POSIXct("01/01/2015 00:00:00", format='%m/%d/%Y %H:%M:%S')
# limit_right <- as.POSIXct("01/01/2015 02:00:00", format='%m/%d/%Y %H:%M:%S')

theme_settings <- theme(
	text = element_text(size=8, color = "black"), 
	title = element_text(size=12),
	axis.text.y = element_text(size=4, color = "black"),
	axis.text.x = element_text(size=4, color = "black"), 
	strip.text = element_text(size = 8), 
	strip.text.y = element_text(angle = 90),
	axis.title.y=element_text(vjust=1.5)) + 
	theme(legend.position="top")

plotTimeline <- function(data, filename, width = 20, height = 5, faceting = F, 
	tl_bar_detail = "worker_id", 
	tl_bar_start = "execution_relative_start",
	tl_bar_end = "execution_relative_end",	
	tl_y = "unit_id",	
	tl_y_coord = "unit_number",	
	tl_facet_a = "platform",	
	tl_facet_b = "variation",	
	tl_breaks_major = "10 min",
	tl_breaks_minor = "5 min",
	tl_breaks_format = "%H:%M",
	tl_title_x = "Time since the task launch",
	tl_title_y = "Unit ID"
	){

	# FORMULATE DATASET ACCORDING TO INPUT PARAMETERS
	timeline_data = data
	timeline_data$tl_bar_detail = timeline_data[,tl_bar_detail]
	timeline_data$tl_bar_start = timeline_data[,tl_bar_start]
	timeline_data$tl_bar_end = timeline_data[,tl_bar_end]
	timeline_data$tl_y = timeline_data[,tl_y]
	timeline_data$tl_y_coord = timeline_data[,tl_y_coord]

	# FORM GRAPH
	graph_timeline <- ggplot(timeline_data)
	graph_timeline <- graph_timeline + geom_rect(color = "white", aes(fill = tl_bar_detail, xmin = tl_bar_start, xmax = tl_bar_end, y = tl_y, ymin = tl_y_coord - 0.475  , ymax = tl_y_coord + 0.475))
	graph_timeline <- graph_timeline + geom_text(data=timeline_data, aes(x= execution_relative_end - (duration_num/2), y=unit_id, label=tl_bar_detail), size=2,color = "black")
	graph_timeline <- graph_timeline + scale_x_datetime(breaks = date_breaks(tl_breaks_major), minor_breaks = date_breaks(tl_breaks_minor),labels = date_format(tl_breaks_format))
	graph_timeline <- graph_timeline + xlab(tl_title_x) + ylab(tl_title_y)
	graph_timeline <- graph_timeline + scale_fill_discrete(name=tl_bar_start,guide=FALSE)
	
	# FACETING
	if (faceting){
		timeline_data$timeline_facet_a = timeline_data[,timeline_facet_a]
		timeline_data$timeline_facet_b = timeline_data[,timeline_facet_b]
		graph_timeline <- graph_timeline + facet_grid(platform ~ variation)
	}
	
	# APPLY THEMES TO GRAPH
	graph_timeline <- graph_timeline + theme_settings
	
	# CONSTRACT A FILENAME
	destfile = paste(GRAPHS_FOLDER,"/timeline_",timeline_data[1,'job_id'],".pdf", sep="")
	
	# SAVE GRAPH INTO THE FILE
	ggsave(graph_timeline, file=destfile, width=width, height=height)
	print(paste("Graph created: ",destfile, sep=" "))
}