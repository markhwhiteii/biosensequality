#' Generate State-Level Summaries
#' 
#' All of the other functions group by facility and then often group by patient visit. This function generates a true statewide
#' (or table-wide, if the table you are calling is not the entire state) summary for counts and percents for each null and invalid check.
#' The same message-level or patient-visit-level rules are used as are described in `get_all_invalids`, `get_opt_nulls` and `get_req_nulls`.
#' 
#' @param data The raw data from BioSense on which you will generate the statewide summary.
#' @param state_req_nulls A summary returned from the `get_req_nulls` function.
#' @param state_opt_nulls A summary returned from the `get_opt_nulls` function.
#' @param state_invalids A summary returned from the `get_all_invalids` function.
#' @return A list of three two-row data frames, each being counts and percentages for required nulls, optional nulls, and invalid entries.
#' @import dplyr
#' @import tidyr
#' @export
statewide <- function(data, state_req_nulls, state_opt_nulls, state_invalids) {
  
  # get n visits, n records, facilities statewide
  n_visits <- n_groups(group_by(data, C_BioSense_ID)) # number of patient visits
  n_records <- nrow(data) # number of records
  
  # get totals to work with
  statewide_reqnull <- state_req_nulls %>% # take the summary
    filter(Measure=="Count") %>% # only get count rows
    summarise_at(vars(-c(1,2)), funs(sum))
  
  statewide_optnull <- state_opt_nulls %>%
    filter(Measure=="Count") %>%
    summarise_at(vars(-c(1,2)), funs(sum))
  
  statewide_invalids <- state_invalids %>% 
    filter(Measure=="Count") %>%
    summarise_at(vars(-c(1,2)), funs(sum))
  
  # return statewide percent for these
  req_all_fields <- c("Treating_Facility_ID", "Trigger_Event", "Message_Control_ID", "Processing_ID", "Version_ID", 
                      "Message_Profile_ID", "Recorded_Date_Time", "Message_Date_Time", "First_Patient_ID", 
                      "Medical_Record_Number") # required fields
  
  # getting percentages to deal with
  statewide_reqnull[2,] <- ifelse(colnames(statewide_reqnull) %in% req_all_fields, # if colname is in req all records
                                  round(statewide_reqnull[1,]/n_records*100,2), # then divide by number of records
                                  round(statewide_reqnull[1,]/n_visits*100,2)) # if not: visits
  
  statewide_optnull[2,] <- round(statewide_optnull[1,]/n_visits*100,2)
  statewide_invalids[2,] <- round(statewide_invalids[1,]/n_visits*100,2)
  
  # putting name and measure at beginning, return in list
  return(
    list(statewide_reqnull=cbind(Location=c("State", "State"), Measure=c("Count", "Percent"), statewide_reqnull),
         statewide_optnull=cbind(Location=c("State", "State"), Measure=c("Count", "Percent"), statewide_optnull),
         statewide_invalids=cbind(Location=c("State", "State"), Measure=c("Count", "Percent"), statewide_invalids))
  )
}
