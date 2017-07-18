#' Testing if First_Patient_ID is equal to one's Medical_Record_Number
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of cases where a patient's
#' First_Patient_ID does not match their Medical_Record_Number.
#' 
#' @param data The raw data from BioSense on which you will do the mismatch checks.
#' @return A list of two data frames: examples and summary for mismatches from First_Patient_ID (FPID) and Medical_Record_Number (MRN).
#' @import dplyr
#' @export
fpid_mrn_invalid <- function(data) {
  
  # generate examples
  fpidmrn_examples <- data %>% # take data
    select(c(First_Patient_ID, Medical_Record_Number, C_BioSense_ID, C_Biosense_Facility_ID)) %>% # selecting just columns we need
    mutate(First_Patient_ID=as.character(First_Patient_ID), # making first patient ID as character
           Medical_Record_Number=as.character(Medical_Record_Number), # making mrn a character
           FPID_MRN_Mismatch=case_when(
             is.na(First_Patient_ID) & is.na(Medical_Record_Number) ~ NA, # if both are na, then mismatch is na
             First_Patient_ID == Medical_Record_Number ~ FALSE, # if they match, then false
             First_Patient_ID != Medical_Record_Number ~ TRUE # if mismatch, then true
           ))
  
  # generate summary
  fpidmrn_summary <- fpidmrn_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Mismatch=case_when(
      all(is.na(FPID_MRN_Mismatch)) ~ NA, # if all na, then na
      sum(FPID_MRN_Mismatch, na.rm=TRUE) == 0 ~ FALSE, # if all false, then record is false invalid
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # get one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(FPID_MRN_Mismatch.Percent=round(mean(Any_Mismatch, na.rm=TRUE)*100,2), # percent
              FPID_MRN_Mismatch.Count=sum(Any_Mismatch, na.rm=TRUE)) # count
  
  return(
    list(fpidmrn_examples=fpidmrn_examples,
         fpidmrn_summary=fpidmrn_summary)
  )
}
