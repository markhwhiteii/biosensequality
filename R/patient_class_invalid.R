#' Getting Invalid Examples and Summaries for Patient_Class_Code
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Patient_Class_Code.
#' 
#' The valid values were taken from the `PHVS_PatientClass_SyndromicSurveillance_V2.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("patient_class")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid patient class checks.
#' @return A list of two data frames: examples and summary for invalid Patient_Class_Code.
#' @import dplyr
#' @export
patient_class_invalid <- function(data) {
  
  # generate valid values
  data("patient_class", envir=environment())
  
  valid_ptclass_values <- patient_class %>% # bring in data
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>%  # remove the names to leave bare values
    toupper() # upper case everything
  
  # generate examples
  patient_class_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Patient_Class_Code)) %>%  # taking just the variables we need
    mutate(Patient_Class_Code=as.character(Patient_Class_Code), # make class code character
           Invalid_Patient_Class_Code=case_when(
             is.na(Patient_Class_Code) ~ NA, # if field is na, then invalid is na
             Patient_Class_Code %in% valid_ptclass_values ~ FALSE, # if patient class is in valid, then invalid is false
             !Patient_Class_Code %in% valid_ptclass_values ~ TRUE, # if it is not, then invalid is true
           ))
  
  # generate summary
  patient_class_summary <- patient_class_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Patient_Class_Code=case_when(
      all(is.na(Invalid_Patient_Class_Code)) ~ NA, # if all na, keep na
      sum(Invalid_Patient_Class_Code, na.rm=TRUE) == 0 ~ FALSE, # if all false, then visit is false invalid
      TRUE ~ TRUE # otherwise, invalid
    )) %>% 
    slice(1) %>% # take one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Patient_Class_Code.Percent=round(mean(Any_Invalid_Patient_Class_Code, na.rm=TRUE)*100,2), # percent
              Patient_Class_Code.Count=sum(Any_Invalid_Patient_Class_Code, na.rm=TRUE)) # count
  
  return(
    list(patient_class_examples=patient_class_examples,
         patient_class_summary=patient_class_summary)
  )
}
