source("Approach/simulations_lib.r")

# ===================================
# LINEAR REGRESSIONS
# ===================================
for(i in seq(1:nrow(SIMULATION_TASKS))){
	simulate_speed_linear(SIMULATION_TASKS[i,"id"],SIMULATION_TASKS[i,"spreadsheet"])
}
# ===================================
# CLOSED TABS ANALYSIS
# ===================================
for(i in seq(1:nrow(SIMULATION_TASKS))){
	simulate_speed_closed_tabs(SIMULATION_TASKS[i,"id"],SIMULATION_TASKS[i,"spreadsheet"])
}
# ===================================
# PREDICTION BASED ON PAGE ACTIVITY
# ===================================
for(i in seq(1:nrow(SIMULATION_TASKS))){
	for(j in seq(1:nrow(SIMULATION_TASKS))){
		simulate_speed_ml(SIMULATION_TASKS[i,"id"],SIMULATION_TASKS[i,"spreadsheet"],SIMULATION_TASKS[j,"id"],SIMULATION_TASKS[j,"spreadsheet"])
	}
}

