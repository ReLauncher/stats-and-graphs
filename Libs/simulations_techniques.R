method_speed_tabs_closed <- function(task, currentTime, relaunched, settings, results = FALSE){
  
  if (typeof(results) != "list"){
    results <- prepareUnitResults(task$id,task$type,task$spreadsheet,FALSE)
  }
  
  tabsActivity <- prepareTabActivityLogs(task$id, currentTime)
  #collect clickActivity logs for this task by the currentTime moment
  clickActivity <- prepareClicksLogs(task$id, currentTime)
  
  #filter out only browser tab closed events which happened before current time - the delay
  #tabsClosed <- tabsActivity[tabsActivity$dt_end == tabsActivity$dt_start+1,]
  tabsClosed <- tabsActivity[tabsActivity$status == " closed",]
  tabsClosed <- tabsClosed[tabsClosed$dt_start < (currentTime - as.numeric(settings$AlgorithmDelay)),]
  
  #filter out actions when workers Give up working on the assignment (the platform does relaunch)
  clickGiveUp <- sqldf("select * from clickActivity where element like '%#give-up-modal > DIV:nth-child(3) > A:nth-child(2)%' ") #'%#give-up-modal > DIV:nth-child(3) > A:nth-child(2)%'
  
  query <- paste("
                select
                tc.assignment_id
                from tabsClosed tc
                  left join relaunched abr on tc.assignment_id = abr.assignment_id
                  left join tabsActivity ta on tc.assignment_id = ta.assignment_id and ta.dt_start > tc.dt_start
                  -- 5 seconds is the delay to map results from CrowdFlower API and our logs. 5 seconds is the maximum delay could be take place
                  left join results r on r.re_unit_id = tc.unit_id and abs(re_first_execution_start+r.re_execution_relative_end_num - tc.dt_start) < 5
                  -- 2 seconds is the delay to map clicks to GiveUp button and browser tab close event
                  left join clickGiveUp cg on cg.assignment_id = tc.assignment_id and abs(cg.dt_start - tc.dt_start) < 5
                where abr.assignment_id is null and r.re_unit_id is null and cg.assignment_id is null and ta.assignment_id is null
                 ")
  
  new_to_relaunch <- sqldf(query)
  new_to_relaunch
}

method_speed_ml <- function(task, currentTime, relaunched, settings, results = FALSE){
  if (typeof(results) != "list"){
    results <- prepareUnitResults(task$id,task$type,task$spreadsheet,FALSE)
  }
  
  # collect current assignment_logs
  features_for_all_assignments <- prepareAssignmentFeatures(task$id, currentTime)
  # remove assignments which already were relaunched
  features_for_not_relaunched <- features_for_not_relaunched[!(features_for_not_relaunched$assignment_id %in% relaunched$assignment_id),]
  
  # retrieve a model
  
  speed_model <- some_model
  
  # predict assignments to be relaunched in the current data using the model
  predictions <- predict(speed_model, features_for_not_relaunched)
  new_to_relaunch <- features_for_all_assignments[predictions == 1,]
  # return new relaunched assignments
  new_to_relaunch
  
}

prepareTrainingSet <- function(task){
  # here we assume the task is completed and all logs are collected 
  #(not partial, so the decisions about abandoned assignments can be made)
  assignment_features <- prepareAssignmentFeatures(task$id)

  results <- prepareUnitResults(task$id, task$type, task$spreadsheet)

  training_set <- sqldf("
                        select 
                          af.*, 
                          case when r.re_unit_id is null then 1 else 0 end as re_abandoned,
                          r.re_evaluation \r
                        from assignment_features af 
                          left join results r on af.unit_id = r.re_unit_id and abs(af.assignment_end - r.re_execution_end) < 10
                          ")
  training_set
}
