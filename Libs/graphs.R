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

#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#scale_fill_manual(values=cbPalette)
#scale_colour_manual(values=cbPalette)

#myColors <- brewer.pal(5,"Set1")
#names(myColors) <- levels(c("0","1","2","3","-1"))
#colScale <- scale_colour_manual(name = "re_evaluation",values = myColors)
evaluation_color <- scale_colour_manual(guide=FALSE,
  values = c("-2" = "black", "-1" = "orange", "0" = "#FF5555","1" = "black","2" = "#eeeeee"))
evaluation_fill <- scale_fill_manual(
  values = c("-2" = "black", "-1" = "orange", "0" = "#FF5555","1" = "black","2" = "#eeeeee"))

theme_settings <- theme(
	text = element_text(size=8, color = "black"), 
	title = element_text(size=12),
	axis.text.y = element_text(size=8, color = "black"),
	axis.text.x = element_text(size=8, color = "black"), 
	strip.text = element_text(size = 14), 
	strip.text.y = element_text(angle = 90),
	axis.title.y=element_text(vjust=1.5)) + 
	theme(legend.position="top")

plotTimeline <- function(data, width = 20, height = 5, faceting = F, 
	tl_bar_detail_show = T,
	tl_bar_detail = "X_worker_id", 
	tl_bar_detail_color = "black",
	tl_bar_color = "re_evaluation", 
	tl_bar_start = "re_execution_relative_start",
	tl_bar_end = "re_execution_relative_end",	
	tl_y = "re_unit_number",	
	tl_y_coord = "re_unit_number",	
	tl_facet_a = "re_condition",	
	tl_facet_b = "re_task",	
	tl_breaks_major = "10 min",
	tl_breaks_minor = "5 min",
	tl_breaks_format = "%H:%M",
	tl_title_x = "Time since the task launch",
	tl_title_y = "Unit ID"
	){

	# FORMULATE DATASET ACCORDING TO INPUT PARAMETERS
	timeline_data <- data
	timeline_data$tl_bar_color <- timeline_data[,tl_bar_color]
	timeline_data$tl_bar_detail <- timeline_data[,tl_bar_detail]
	timeline_data$tl_bar_start <- timeline_data[,tl_bar_start]
	timeline_data$tl_bar_end <- timeline_data[,tl_bar_end]
	timeline_data$tl_y <- timeline_data[,tl_y]
	timeline_data$tl_y_coord <- timeline_data[,tl_y_coord]
	timeline_data$tl_facet_a  <- timeline_data[,tl_facet_a]
	timeline_data$tl_facet_b  <- timeline_data[,tl_facet_b]
	timeline_data$re_duration_num <- difftime(timeline_data$tl_bar_end, timeline_data$tl_bar_start, units = "secs") 

	# FORM GRAPH
	graph_timeline <- ggplot(timeline_data)
	graph_timeline <- graph_timeline + geom_rect(color = "white", aes(fill = tl_bar_color,  xmin = tl_bar_start, xmax = tl_bar_end, y = tl_y, ymin = tl_y_coord - 0.475  , ymax = tl_y_coord + 0.475))
	if (tl_bar_detail_show){
		graph_timeline <- graph_timeline + geom_text(data=timeline_data, aes(x= tl_bar_start + (re_duration_num/2), y=tl_y_coord, label=tl_bar_detail), size=2,color = tl_bar_detail_color)	
	}
	graph_timeline <- graph_timeline + scale_x_datetime(breaks = date_breaks(tl_breaks_major), minor_breaks = date_breaks(tl_breaks_minor),labels = date_format(tl_breaks_format))
	graph_timeline <- graph_timeline + xlab(tl_title_x) + ylab(tl_title_y)
	# graph_timeline <- graph_timeline + scale_fill_discrete(name=tl_bar_start,guide=FALSE)
	
	# FACETING
	if (faceting == TRUE){
		graph_timeline <- graph_timeline + facet_grid(tl_facet_a ~ tl_facet_b)
	}
	
	# APPLY THEMES TO GRAPH
	graph_timeline <- graph_timeline + theme_settings
	graph_timeline <- graph_timeline + evaluation_fill + evaluation_color
	
	
	# CONSTRACT A FILENAME
	destfile = paste(GRAPHS_FOLDER,"/timeline_",timeline_data[1,'task'],".pdf", sep="")
	
	# SAVE GRAPH INTO THE FILE
	# ggsave(graph_timeline, file=destfile, width=width, height=height)
	# print(paste("Graph created: ",destfile, sep=" "))
	graph_timeline
}

plotHistogram<- function(data, width = 10, height = 5, faceting = F){
	
	hist_plot <- ggplot(data, aes(x=re_duration_num))
	#hist_plot <- hist_plot + geom_histogram(binwidth=5, colour="black", fill="white")
	hist_plot <- hist_plot + geom_histogram(aes(y=..density.., fill = re_evaluation),alpha =0.8, binwidth = 25)
	hist_plot <- hist_plot + scale_x_continuous(breaks = c(125*0:8))
	#hist_plot <- hist_plot + scale_x_continuous(breaks=seq(0,max(data$this_column_to_use),by=25))
	#hist_plot <- hist_plot + geom_rect(color = "black", aes(fill = worker_id, xmin = execution_relative_end - duration, xmax = execution_relative_end, y = unit_id, ymin = unit_number - 0.45  , ymax = unit_number + 0.45))
	#hist_plot <- hist_plot+ geom_text(data=data, aes(x= execution_relative_end - (duration/2), y=unit_id, label=worker_id), size=3,color = "black")
	#hist_plot <- hist_plot + scale_x_datetime(breaks = date_breaks("2 min"), minor_breaks = date_breaks("1 min"),labels = date_format("%H:%M"), limit = c(limit_left,limit_left+10*60))
	hist_plot <- hist_plot + xlab("Assignments duration, seconds") + ylab("Density")
	#hist_plot <- hist_plot + ggtitle(paste("Execution of the task id=",data[1,"job_id"], " on ", data[1,"platform"], sep=""))
	#hist_plot <- hist_plot + scale_fill_discrete(name="Worker ID")
	#hist_plot <- hist_plot + facet_grid(units ~ .)
	hist_plot <- hist_plot + theme_settings
	hist_plot <- hist_plot + evaluation_fill
	if (faceting == T){
		hist_plot <- hist_plot + facet_grid(re_condition ~ re_task)
	}
	#hist_plot <- hist_plot + theme(text = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"), title = element_text(size=20),strip.text = element_text(size = 20), strip.text.y = element_text(angle = 90),axis.title.y=element_text(vjust=1.5)) + theme(legend.position="top")
	
	# hist_plot
	# save the plot into the file

	# CONSTRACT A FILENAME
	destfile = paste(GRAPHS_FOLDER,"/histogram_",data[1,'task'],".pdf", sep="")
	
	#ggsave(hist_plot, file=destfile, width=width, height=height)
	hist_plot
}
plotDotsDurationVSIndexAccuracy <- function(data,width=10, height=5, faceting = F){
	#data$this_column_to_use <- data[,field]

	#hypothesis1.lm = lm(yy ~ y_indexes)
	#slope <-round(hypothesis1.lm$coefficients['y_indexes'],3)
	#intercept <-round(hypothesis1.lm$coefficients['(Intercept)'],3)
	#df = data.frame(yy, y_indexes)

	hist_plot <- ggplot()
	#hist_plot <- hist_plot + scale_y_continuous(limit = c(0.6,1))
	#hist_plot <- hist_plot + scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))
	#hist_plot <- hist_plot + geom_histogram(binwidth=5, colour="black", fill="white")
	#hist_plot <- hist_plot + geom_point(data=data, aes(index,duration_num), color =rgb(red= 58,green=145,blue= 255, maxColorValue = 255))
	hist_plot <- hist_plot + geom_point(data=data, aes(re_index,re_duration_num, color = re_evaluation))
	#hist_plot <- hist_plot + geom_point(data=df, aes(y_indexes, yy))
	#hist_plot <- hist_plot + scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))
	## 43, 45
	#hist_plot <- hist_plot + geom_text(data=df, x=83,y=355, label=paste('duration = ',slope,'index + ',intercept,sep=''),size=5,fontface=3) #color = rgb(red= 58,green=145,blue= 255, maxColorValue = 255)
	#hist_plot <- hist_plot + geom_text(data=df, x=89,y=slope*nrow(data)+intercept+47, label=paste('max duration limit = ',round(slope*nrow(data)+intercept),' seconds',sep=''),size=5,fontface=3) #,color = 'red'
	#hist_plot <- hist_plot + geom_abline(intercept = intercept, slope = slope) #, color = rgb(red= 58,green=145,blue= 255, maxColorValue = 255)
	#hist_plot <- hist_plot + geom_abline(intercept = slope*nrow(data)+intercept, slope = 0, color = 'red')
	hist_plot <- hist_plot + scale_x_continuous(breaks = c(50*0:6))
	hist_plot <- hist_plot + xlab("Assignments index") + ylab("Assignments duration, seconds")
	hist_plot <- hist_plot + theme_settings
	hist_plot <- hist_plot + evaluation_color
	if (faceting == T){
		hist_plot <- hist_plot + facet_grid(re_condition ~ re_task)
	}
	#hist_plot <- hist_plot + theme(text = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"), title = element_text(size=20),strip.text = element_text(size = 20), strip.text.y = element_text(angle = 90),axis.title.y=element_text(vjust=1.5)) + theme(legend.position="top")
	
	#hist_plot <- hist_plot + stat_smooth()
	#hist_plot <- hist_plot + geom_rect(color = "black", aes(fill = worker_id, xmin = execution_relative_end - duration, xmax = execution_relative_end, y = unit_id, ymin = unit_number - 0.45  , ymax = unit_number + 0.45))
	#hist_plot <- hist_plot+ geom_text(data=data, aes(x= execution_relative_end - (duration/2), y=unit_id, label=worker_id), size=3,color = "black")
	#hist_plot <- hist_plot + scale_x_datetime(breaks = date_breaks("5 min"), minor_breaks = date_breaks("1 min"),labels = date_format("%H:%M"))
	#hist_plot <- hist_plot + xlab("Units amount") + ylab("Workers/Units")

	#hist_plot <- hist_plot + ggtitle(paste("Execution of the task id=",data[1,"job_id"], " on ", data[1,"platform"], sep=""))
	#hist_plot <- hist_plot + scale_fill_discrete(name="Worker ID")
	#hist_plot <- hist_plot + facet_grid(filter ~ variation)
	# hist_plot
	#hist_plot <- hist_plot + theme(text = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"), title = element_text(size=20),strip.text = element_text(size = 20), strip.text.y = element_text(angle = 90),axis.title.y=element_text(vjust=1.5)) + theme(legend.position="top")
	# CONSTRACT A FILENAME
	destfile = paste(GRAPHS_FOLDER,"/plot_",data[1,'task'],".pdf", sep="")

	# save the plot into the file
	# ggsave(hist_plot, file=destfile, width=width, height=height)
	hist_plot
}

plotCumulative <- function(data, width=20, height=8, faceting = F){
	hist_plot <- ggplot(data)
	hist_plot <- hist_plot + geom_step(aes(x=re_execution_relative_start,y=..y..),stat="ecdf", color=rgb(red= 58,green=145,blue= 255, maxColorValue = 255))
	hist_plot <- hist_plot + geom_step(aes(x=re_execution_relative_end,y=..y..),stat="ecdf", color="black")
	hist_plot <- hist_plot + scale_x_datetime(breaks = date_breaks("30 min"), minor_breaks = date_breaks("15 min"),labels = date_format("%H:%M")) # , limit = c(limit_left,limit_right)
	
	hist_plot <- hist_plot + xlab("Time since the task launch, hours:mins") + ylab("Task completeness")
	#hist_plot <- hist_plot + facet_grid(variation ~ condition)
	# font size, theme settings
	# hist_plot <- hist_plot + theme(text = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"), title = element_text(size=20),strip.text = element_text(size = 20), strip.text.y = element_text(angle = 90),axis.title.y=element_text(vjust=1.5)) + theme(legend.position="top")
	hist_plot <- hist_plot + theme_settings
	if (faceting == T){
		hist_plot <- hist_plot + facet_grid(re_condition ~ re_task)
	}
	
	#hist_plot <- hist_plot + theme(text = element_text(size=14, color = "black"), axis.text = element_text(size=14, color = "black"), title = element_text(size=14),strip.text = element_text(size = 14), strip.text.y = element_text(angle = 90),axis.title.y=element_text(vjust=1.5)) + theme(legend.position="top")
	# save the plot into the file
	# ggsave(hist_plot, file=filename, width=width, height=height)
	hist_plot
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}