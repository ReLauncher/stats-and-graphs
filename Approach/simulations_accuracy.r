source("Approach/simulations_lib.r")

# ===================================
# DURATION OUTLIERS DETECTION
# ===================================
for(i in seq(1:nrow(SIMULATION_TASKS))){
	simulate_accuracy_outliers(SIMULATION_TASKS[i,"id"],SIMULATION_TASKS[i,"spreadsheet"])
}

# ===================================
# PREDICTION BASED ON PAGE ACTIVITY
# ===================================
for(m in seq(1:nrow(SIMULATION_TASKS))){
	for(k in seq(1:nrow(SIMULATION_TASKS))){
		print(paste(m,k,sep=" "))
		simulate_accuracy_ml(SIMULATION_TASKS[m,"id"],SIMULATION_TASKS[m,"spreadsheet"],SIMULATION_TASKS[k,"id"],SIMULATION_TASKS[k,"spreadsheet"])
	}
}


