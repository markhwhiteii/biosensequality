#' Checking if Patient Class is "E" At Least Once in a Visit
#'
#' Since these data are coming from emergency departments, the patient class should be reported as "E." This should be reported as "E"
#' at least once per visit. This function will generate two data frames: first, a frame to be used later to extract invalid examples
#' from (i.e., visits with no "E" present); second, a frame that contains facility-level summaries for counts and percentages of visits
#' without an "E" patient class at least once.
#' 
#' @param data The raw data from BioSense on which you will do the check.
#' @return A list of two data frames: examples and summary for visits with no "E" in patient class.
#' @import dplyr
#' @export
any_e_invalid <- function(data) {
  
  # generate examples
  any_e_examples <- data %>% # take data
    group_by(C_BioSense_ID) %>% # group by patient ID
    select(C_Biosense_Facility_ID, Patient_Class_Code, C_BioSense_ID) %>% # selecting just variables we need
    mutate(Patient_Class_Code=toupper(Patient_Class_Code), # uppercase everything
           Class_Is_E=ifelse(Patient_Class_Code=="E", TRUE, FALSE), # if message is e, then true; else, false
           No_E_In_Visit=ifelse(sum(Class_Is_E, rm.na=TRUE) > 0, FALSE, TRUE)) %>% # is there at least one e in a visit? true if no
    ungroup() # explicitly ungrouping
  
  # generate summary
  any_e_summary <- any_e_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by visit
    slice(1) %>% # get one entry for each visit (all of the relevant variables are the same across all rows of a patient visit)
    ungroup() %>% # explicitly ungrouping
    group_by(C_Biosense_Facility_ID) %>% # regrouping by facility
    summarise(Visits_With_No_E_Pt_Class.Percent=round(mean(No_E_In_Visit, na.rm=TRUE)*100,2), # get percent
              Visits_With_No_E_Pt_Class.Count=sum(No_E_In_Visit, na.rm=TRUE)) # get count
  
  return(
    list(any_e_examples=any_e_examples,
         any_e_summary=any_e_summary)
  )
}
