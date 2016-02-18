source("Libs/collect.R")
source("Libs/graphs.R")
library(RCurl)
require(scales)


ldata <- read.table("Logs/crowdflower.csv", header=T, sep="," ,quote = "\"", comment.char = "")
ldata$dt_start <- as.POSIXct(as.numeric(ldata$dt_start)/1000, origin="1970-01-01",tz="GMT")
ldata$dt_end <- as.POSIXct(as.numeric(ldata$dt_end)/1000, origin="1970-01-01",tz="GMT")
#ldata$unit_number <- ldata$unit_id
#ldata$unit_number <- ldata$unit_number - min(ldata$unit_number)+1
ldata$unit_id <- as.factor(ldata$unit_id)
timeline_data <- ldata

graph_timeline <- ggplot(timeline_data)
graph_timeline <- graph_timeline + geom_rect(size = 0.01, aes(color = status, fill = status, xmin = dt_start, xmax = dt_end, y = unit_id,  ymin = unit_number - 0.475  , ymax = unit_number + 0.475))
#graph_timeline <- graph_timeline + geom_text(data=timeline_data, aes(x= execution_relative_end - (duration_num/2), y=unit_id, label=tl_bar_detail), size=2,color = "black")
graph_timeline <- graph_timeline + scale_x_datetime(breaks = date_breaks("30 min"), minor_breaks = date_breaks("15 min"),labels = date_format("%H:%M"))
#graph_timeline <- graph_timeline + xlab("x") + ylab("y")
#graph_timeline <- graph_timeline + scale_fill_discrete(name=tl_bar_start,guide=FALSE)
destfile <- "test_log.pdf"

# SAVE GRAPH INTO THE FILE
ggsave(graph_timeline, file=destfile, width=20, height=10)
print(paste("Graph created: ",destfile, sep=" "))