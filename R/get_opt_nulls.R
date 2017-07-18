#' Getting Nulls Summaries for Optional Fields
#'
#' This will generate percent and counts of null message, by facility. All are calculated on a once-per-visit basis (see `get_req_nulls`).
#' 
#' The distinctions between optional vs. invalid and per visit vs. every message were made based off of our interpretation of the
#' PHIN guide as well as the needs of the Kansas Department of Health and Environment Syndromic Surveillance.
#' 
#' @param data The raw data from BioSense on which you will do the null checks.
#' @return A summary data frame that lists counts and percentages for null fields, summarized at facility-level.
#' @import dplyr
#' @import tidyr
#' @export
get_opt_nulls <- function(data) {
  
  # listing fields to summarize and create future column names from them
  opt_pv_fields <- c("Birth_Date_Time", "Admit_Source", "Initial_Temp", "Initial_Temp_Units", "Problem_List_Code", 
                     "Problem_List_Description", "Medication_List", "Medication_Code", "Medication_Description", 
                     "Travel_History", "Initial_Pulse_Oximetry", "Initial_Pulse_Oximetry_Units",
                     "Systolic_Diastolic_Blood_Pressure", "Systolic_Diastolic_Blood_Pressure_Units") # fields optional once per visit
  opt_pv_pctnames <- unlist(lapply(opt_pv_fields, function(x) paste0(x, ".Percent"))) # creating names for the file
  opt_pv_cntnames <- unlist(lapply(opt_pv_fields, function(x) paste0(x, ".Count"))) # creating names for the file
  
  # percent nulls for optional fields
  pct <- data %>% # take data
    group_by(C_BioSense_ID) %>% # group by patient visit
    summarise_at(opt_pv_fields, # summarise for all fields in the vector above
                 funs(all(is.na(.)))) %>% # returns true if that field is in na in all messages for that patient visit
    ungroup() %>% # explicitly ungroup
    full_join(data[,c("C_BioSense_ID", "C_Biosense_Facility_ID")], by="C_BioSense_ID") %>% # get facility info back
    group_by(C_BioSense_ID) %>% # grouping by patient visit again
    slice(1) %>% # taking just one row per patient visit (they are all the same)
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # regroup by facility
    summarise_at(opt_pv_fields, # summarise at the given fields above
                 funs(round(mean(., na.rm=TRUE)*100,2))) %>% # take mean (i.e., proportion true), round to get a percentage
    magrittr::set_colnames(c("C_Biosense_Facility_ID", opt_pv_pctnames))
  
  # count nulls for optional fields
  cnt <- data %>% # take data
    group_by(C_BioSense_ID) %>% # group by patient visit
    summarise_at(opt_pv_fields, # summarise for all fields in the vector above
                 funs(all(is.na(.)))) %>% # returns true if that field is in na in all messages for that patient visit
    ungroup() %>% # explicitly ungroup
    full_join(data[,c("C_BioSense_ID", "C_Biosense_Facility_ID")], by="C_BioSense_ID") %>% # get facility info back
    group_by(C_BioSense_ID) %>% # grouping by patient visit again
    slice(1) %>% # taking just one row per patient visit (they are all the same)
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # regroup by facility
    summarise_at(opt_pv_fields, # summarise at the given fields above
                 funs(sum(., na.rm=TRUE))) %>% # take mean (i.e., proportion true), round to get a percentage
    magrittr::set_colnames(c("C_Biosense_Facility_ID", opt_pv_cntnames))
  
  return(
    pct %>% # take stuff from above...
      full_join(cnt, by = "C_Biosense_Facility_ID") %>% # more stuff from above
      gather("Check", "Value", 2:ncol(.)) %>% # gather all columns after facility into check and value columns
      separate(Check, c("Field", "Measure"), "[.]") %>% # splitting up check varaible into three columns by the period
      spread(Field, Value) %>% # spread out all of the facility ids into multiple columns
      as.data.frame() # make data frame, not tbl_df, for when we want to write to excel
  )
}
