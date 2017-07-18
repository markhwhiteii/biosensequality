#' Getting Nulls Summaries for Required Fields
#'
#' This will generate percent and counts of null message, by facility. All are calculated on a once-per-visit basis, except for:
#' Treating_Facility_ID, Trigger_Event, Message_Control_ID, Processing_ID, Version_ID, Message_Profile_ID, Event_Type_Code, 
#' Recorded_Date_Time, Message_Date_Time, First_Patient_ID, Medical_Record_Number. These fields are required every single message, so
#' null counts are based on messages. 
#' 
#' The rest are required only once per-visit, so the whole visit is considered null *only* if *every* record for that field from that visit was null.
#' 
#' The distinctions between optional vs. invalid and per visit vs. every message were made based off of our interpretation of the
#' PHIN guide as well as the needs of the Kansas Department of Health and Environment Syndromic Surveillance.
#' 
#' @param data The raw data from BioSense on which you will do the null checks.
#' @return A summary data frame that lists counts and percentages for null fields, summarized at facility-level.
#' @import dplyr
#' @import magrittr
#' @import tidyr
#' @export
get_req_nulls <- function(data) {
  
  # listing fields to summarize and create future column names from them
  req_all_fields <- c("Treating_Facility_ID", "Trigger_Event", "Message_Control_ID", "Processing_ID", "Version_ID", 
                      "Message_Profile_ID", "Recorded_Date_Time", "Message_Date_Time", "First_Patient_ID", 
                      "Medical_Record_Number") # fields required on all messages
  req_all_pctnames <- unlist(lapply(req_all_fields, function(x) paste0(x, ".Percent"))) # creating names for the report
  req_all_cntnames <- unlist(lapply(req_all_fields, function(x) paste0(x, ".Count"))) # creating names for the report
  
  req_pv_fields <- c("Discharge_Date_Time", "Diagnosis_Code", "Diagnosis_Description", "Diagnosis_Type", "Discharge_Disposition", 
                     "Insurance_Company_ID", "Administrative_Sex", "Race_Code", "Race_Description", "Patient_City", "Patient_Zip", 
                     "Patient_State", "C_Patient_County", "Patient_Country", "Ethnicity_Code", "Ethnicity_Description", 
                     "Patient_Class_Code", "Visit_ID", "Admit_Reason_Combo", "Chief_Complaint_Combo",
                     "Facility_Type_Code", "Facility_Type_Description", "Age_Reported", 
                     "Age_Units_Reported", "Triage_Notes", "Clinical_Impression", "Height", "Height_Units", "Weight", 
                     "Weight_Units", "Smoking_Status_Code", "Smoking_Status_Description") # fields required once per visit
  req_pv_pctnames <- unlist(lapply(req_pv_fields, function(x) paste0(x, ".Percent"))) # creating names for the report
  req_pv_cntnames <- unlist(lapply(req_pv_fields, function(x) paste0(x, ".Count"))) # creating names for the report
  
  # getting required on all messages summaries
  pct_nulls_req_all <- data %>% # take data
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise_at(req_all_fields, # summarise at fields required for all messages
                 funs(round(sum(is.na(.))/length(.)*100,2))) %>% # percentage of nulls in a given field
    set_colnames(c("C_Biosense_Facility_ID", req_all_pctnames)) # set column names based on those saved above
  
  count_nulls_req_all <- data %>% # take data
    group_by((C_Biosense_Facility_ID)) %>%  # group by facility
    summarise_at(req_all_fields, # summarise at fields required for all messages
                 funs(sum(is.na(.)))) %>% # sum all na values in a given field
    set_colnames(c("C_Biosense_Facility_ID", req_all_cntnames)) # set column names based on those saved above
  
  # getting required on one message per visit summaries
  pct_nulls_req_pv <- data %>% # take data
    group_by(C_BioSense_ID) %>% # group by patient visit
    summarise_at(req_pv_fields, # summarise at fields required once per visit
                 funs(all(is.na(.)))) %>% # returns true if that field is na in all messages for that patient visit
    ungroup() %>% # joining will ungroup, but I'm explicitly doing it here
    full_join(data[,c("C_BioSense_ID", "C_Biosense_Facility_ID")], by="C_BioSense_ID") %>% # joining with facility information
    group_by(C_BioSense_ID) %>% # grouping by patient visit again
    slice(1) %>% # taking just one ovservation per patient visit (they are all identical)
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # regroup by facility
    summarise_at(req_pv_fields, # summarise at fields required once per patient visit
                 funs(round(mean(., na.rm=TRUE)*100,2))) %>% # take the mean (i.e., proportion true), multiply and round to get a percentage
    set_colnames(c("C_Biosense_Facility_ID", req_pv_pctnames)) # renaming columns
  
  count_nulls_req_pv <- data %>% # take data
    group_by(C_BioSense_ID) %>% # group by patient visit
    summarise_at(req_pv_fields, # summarise at fields required once per visit
                 funs(all(is.na(.)))) %>% # returns true if that field is na in all messages for that patient
    ungroup() %>% # joining will ungroup, but explicitly doing it here
    left_join(data[,c("C_BioSense_ID", "C_Biosense_Facility_ID")], by="C_BioSense_ID") %>% # joining with facility information
    group_by(C_BioSense_ID) %>% # grouping by patient visit again
    slice(1) %>% # taking just one ovservation per patient visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # regroup by facility
    summarise_at(req_pv_fields, funs(sum(., na.rm=TRUE))) %>% # take the number of trues (missing in all) per each group
    set_colnames(c("C_Biosense_Facility_ID", req_pv_cntnames)) # renaming columns
  
  return(
    pct_nulls_req_all %>% # take stuff from above...
      full_join(count_nulls_req_all, by = "C_Biosense_Facility_ID") %>% # more stuff from above...
      full_join(pct_nulls_req_pv, by = "C_Biosense_Facility_ID") %>% # more stuff from above...
      full_join(count_nulls_req_pv, by = "C_Biosense_Facility_ID") %>% # and the last table from above
      gather("Check", "Value", 2:ncol(.)) %>% # gather all columns after facility into check and value columns
      separate(Check, c("Field", "Measure"), "[.]") %>% # splitting up check varaible into three columns by the period
      spread(Field, Value) %>% # spread out all of the facility ids into multiple columns
      as.data.frame() # make data frame, not tbl_df, for when we want to write to excel
  )
}
