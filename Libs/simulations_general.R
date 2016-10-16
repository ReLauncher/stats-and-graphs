simulateRuntime <- function(task, method, settings, results = FALSE){
  print("Simulation is started with the following conditions:")
  print(settings)
  
  timeStart <- as.numeric(settings$timeStart)
  timeEnd <- as.numeric(settings$timeEnd)
  timeStep <- as.numeric(settings$timeStep)
  
  relaunched <- data.frame(assignment_id=as.character())
  for (currentTime in seq(from=timeStart,to=timeEnd,by=timeStep)){
    new_to_relaunch <- simulateRuntimeStep(task, currentTime, method, relaunched, settings, results)
    
    relaunched <- data.frame(assignment_id=union(as.character(relaunched$assignment_id),as.character(new_to_relaunch$assignment_id)))
  }
  
  relaunched
}

simulateRuntimeStep <- function(task, currentTime, method, relaunched, settings, results = FALSE){
  new_to_relaunch <- method(task, currentTime, relaunched, settings, results)
  new_to_relaunch
}

getGoldStandard <- function(task){
  # collect results
  results <- prepareUnitResults(task$id,task$type,task$spreadsheet, FALSE)
  # collect behavior logs
  tabActivity <- prepareTabActivityLogs(task$id)
  clickActivity <- prepareClicksLogs(task$id)
  clickGiveUp <- sqldf("select * from clickActivity where element like '%#give-up-modal > DIV:nth-child(3) > A:nth-child(2)%' ") 
  
  assignments_all <- sqldf("select distinct a.assignment_id, case when r.re_unit_id is not null then 0 else 1 end as re_abandoned, r.re_evaluation  
                          from (select unit_id, assignment_id, max(dt_start) as dt_start from tabActivity group by unit_id, assignment_id ) a
                                    left join clickGiveUp cg on cg.assignment_id = a.assignment_id and abs(cg.dt_start - a.dt_start) < 5
                                    left join results r on r.re_unit_id = a.unit_id and abs(r.re_first_execution_start+r.re_execution_relative_end_num - a.dt_start) < 5
                                where cg.assignment_id is null
                                   ")
  assignments_all
}

evaluateSimulationPerformance <- function(gold_standard, relaunched, evaluation_subject = "speed"){
  	validation <- sqldf("
  		select 
          g.*, 
          case when rl.assignment_id is null then 0 else 1 end as re_relaunched
  		from gold_standard g
  		  left join relaunched rl on g.assignment_id = rl.assignment_id
  		")
  	if (evaluation_subject == 'speed'){
  	  validation_summary <- sqldf("\r
    		select \r
    			sum(case when re_relaunched = 1 and re_abandoned = 1 then 1 else 0 end) as tp,\r
    			sum(case when re_relaunched = 0 and re_abandoned = 0 then 1 else 0 end) as tn,\r
    			sum(case when re_relaunched = 1 and re_abandoned = 0 then 1 else 0 end) as fp,\r
    			sum(case when re_relaunched = 0 and re_abandoned = 1 then 1 else 0 end)  as fn\r
    		from validation 
    		")
  	}else{
  	  #VALIDATION FOR ACCURACY TO BE ADDED
  	}
  	validation_summary <- sqldf("\r
  		select \r
  			v.*, 1.0*tp/(tp+fp) as precision, 1.0*tp/(tp+fn) as recall\r
  		from validation_summary v
  		")
  	validation_summary  
}

saveEvaluationPerformance <- function(performance,filename){
	write.table(performance, paste("Approach/Simulations/",filename,".csv",sep = ""),sep=",",row.names = F)
}