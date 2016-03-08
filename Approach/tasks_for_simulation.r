# ----------------------
# RECEIPT TRANSCRIPTION
# ----------------------
TASK_ID <- c()
GOOGLE_SPREADSHEET <- c()

TASK_ID[1] <- 854546
GOOGLE_SPREADSHEET[1] <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=1916343054"
TASK_ID[2] <- 854432
GOOGLE_SPREADSHEET[2] <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=62480003"
TASK_ID[3] <- 854753
GOOGLE_SPREADSHEET[3] <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=94510901"
#TASK_ID[4] <- 855249
#GOOGLE_SPREADSHEET[4] <- "https://docs.google.com/spreadsheets/d/14mzXgEBziGJbzx6HFcb1PkBgCzVQRZOCKb5S4wJRwmc/edit#gid=1784041800"
# ----------------------
# IMAGE LABELING
# ----------------------
#TASK_ID[5] <- 851104
#GOOGLE_SPREADSHEET[5] <- "https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=867177424"
TASK_ID[6] <- 854433
GOOGLE_SPREADSHEET[6] <- "https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=1121372357"
TASK_ID[7] <- 854544
GOOGLE_SPREADSHEET[7] <- "https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=92134759"
TASK_ID[8] <- 854885
GOOGLE_SPREADSHEET[8] <- "https://docs.google.com/spreadsheets/d/1MrjDlHONdLaD-4C29CQ40nt2vY6qLAU8yOBw0PcWTVc/edit#gid=224720013"
# ----------------------
# BUSINESS SEARCH
# ----------------------
#TASK_ID[9] <- 846894
#GOOGLE_SPREADSHEET[9] <- "https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=753616462"
#TASK_ID[10] <- 851103
#GOOGLE_SPREADSHEET[10] <- "https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=1251457815"
TASK_ID[10] <- 854545
GOOGLE_SPREADSHEET[10] <- "https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=457902294"
TASK_ID[11] <- 854883
GOOGLE_SPREADSHEET[11] <- "https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=549204533"
TASK_ID[12] <- 882920
GOOGLE_SPREADSHEET[12] <- "https://docs.google.com/spreadsheets/d/1iB-BqWle_yj0gTj8Kr4Tcgn7OYTwcQylQam8GEtEf_w/edit#gid=1388769002"

# ----------------------

id <- c()
spreadsheet <- c()

for (i in seq(10,12,1)){
	id <- append(id, TASK_ID[i])
	spreadsheet <- append(spreadsheet, GOOGLE_SPREADSHEET[i])
}
SIMULATION_TASKS <- data.frame(id, spreadsheet)