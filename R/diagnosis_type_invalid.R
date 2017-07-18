#' Getting Invalid Examples and Summaries for Diagnosis_Type
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Diagnosis_Type.
#' 
#' The valid values were taken from the `PHVS_DiagnosisType_HL7_2x_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("diagnosis_type")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid Diagnosis_Type checks.
#' @return A list of two data frames: examples and summary for invalid Diagnosis_Type.
#' @import dplyr
#' @export
diagnosis_type_invalid <- function(data) {
  
  # generate list of valid values
  data("diagnosis_type", envir=environment())
  
  valid_diagtype_values <- diagnosis_type %>% # take data
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    `[[`(1) %>% # make vector 
    as.character() %>% # make character
    c(";") %>% # including the semicolon
    paste0(collapse="") # collapse all parts of string into one string
  
  # generate examples
  diagnosis_type_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Diagnosis_Type)) %>% # taking just the variables we need
    mutate(Diagnosis_Type=toupper(as.character(Diagnosis_Type)), # make character and uppercase
           Invalid_Diagnosis_Type=case_when(
             is.na(Diagnosis_Type) ~ NA, # if it is na, keep it na
             grepl(paste0("^[", valid_diagtype_values, "]*$"), Diagnosis_Type) ~ FALSE, # if it contains only valid values, then false invalid
             !grepl(paste0("^[", valid_diagtype_values, "]*$"), Diagnosis_Type) ~ TRUE # if it does not, then true invalid
           ))
  
  # generate summary
  diagnosis_type_summary <- diagnosis_type_examples %>% # take the examples
    group_by(C_BioSense_ID) %>% 
    mutate(Any_Invalid_Diagnosis_Type=case_when(
      all(is.na(Invalid_Diagnosis_Type)) ~ NA, # if all na, keep na
      sum(Invalid_Diagnosis_Type, na.rm=TRUE) == 0 ~ FALSE, # if all are false, then false invalid
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # get one observation per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>%  # group by facility
    summarise(Diagnosis_Type.Percent=round(mean(Any_Invalid_Diagnosis_Type, na.rm=TRUE)*100,2), # percents
              Diagnosis_Type.Count=sum(Any_Invalid_Diagnosis_Type, na.rm=TRUE)) # counts
  
  return(
    list(diagnosis_type_examples=diagnosis_type_examples,
         diagnosis_type_summary=diagnosis_type_summary)
  )
}
