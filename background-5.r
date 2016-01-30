source("graphs.R")
source("collect_experiments.R")
options( warn = -1 )

GOOGLE_SPREADSHEET_URL <-"https://docs.google.com/spreadsheets/d/1DR4_TytGVSE-KyaZGiSpRmd8Ep-wMotK1JxPcPkNBXg/edit#gid=0"
experiments <- collectFromGoogleSpreadsheet(GOOGLE_SPREADSHEET_URL)
experiments$evaluation_url <- as.character(experiments$evaluation_url)

all_data <- FALSE
for (i in 0:2){
	title <- experiments[i*4+1,"title"]
	print(paste("####",title, sep=" "))
	gr <- list()
	for (j in 0:3){
		index <- (i * 4) + j + 1
		print(index)

		experiment <- collectFromGoogleSpreadsheet(experiments[index,"evaluation_url"])
		experiment <- reformatExperimentData(experiment,experiments[index,"job_id"],experiments[index,"title"],experiments[index,"condition"])
		experiment <- filterEvaluation(experiment)
		
		if (all_data == FALSE){
			all_data = experiment
		}else{
			all_data = rbind(all_data, experiment)
		}
	}
}

tl <- plotTimeline(all_data, faceting = T,tl_bar_detail_show = F)
ggsave(tl, file="Graphs/background_Timeline.pdf", width=12, height=6)
print(tl)

pt <- plotDotsDurationVSIndexAccuracy(all_data, faceting = T)
ggsave(pt, file="Graphs/background_Duration_Accuracy.pdf", width=12, height=6)
print(pt)

hg <- plotHistogram(all_data, faceting = T)
ggsave(hg, file="Graphs/background_Histogram.pdf", width=12, height=6)
print(hg)

cm <- plotCumulative(all_data, faceting = T)
ggsave(cm, file="Graphs/background_Cumulative.pdf", width=12, height=6)
print(cm)
