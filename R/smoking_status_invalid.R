#' Getting Invalid Examples and Summaries for Smoking_Status_Code
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Smoking_Status_Code.
#' 
#' The valid values were taken from the `PHVS_SmokingStatus_MU_V2.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("smoking_status")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid smoking status checks.
#' @return A list of two data frames: examples and summary for invalid Smoking_Status_Code.
#' @import dplyr
#' @export
smoking_status_invalid <- function(data) {
  
  # generating valid values
  data("smoking_status", envir=environment())
  
  valid_smoke_values <- smoking_status %>% # get data
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() # unname stuff
  
  # generating examples
  smoking_status_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Smoking_Status_Code)) %>%  # taking just the variables we need
    mutate(Smoking_Status_Code=as.character(Smoking_Status_Code), # make as character and uppercase
           Invalid_Smoking_Status_Code=case_when(
             is.na(Smoking_Status_Code) ~ NA, # if field is na, then invalid is na
             Smoking_Status_Code %in% valid_smoke_values ~ FALSE, # if valid, then false
             !Smoking_Status_Code %in% valid_smoke_values ~ TRUE # if not in valid values, then true
           ))
  
  # generating summary
  smoking_status_summary <- smoking_status_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Smoking_Status_Code=case_when(
      all(is.na(Invalid_Smoking_Status_Code)) ~ NA, # if all na, then na
      sum(Invalid_Smoking_Status_Code, na.rm=TRUE) == 0 ~ FALSE, # if all false, then invalid false
      TRUE ~ TRUE # otherwise, true invalid
    )) %>% 
    slice(1) %>% # get one row per patient visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Smoking_Status_Code.Percent=round(mean(Any_Invalid_Smoking_Status_Code, na.rm=TRUE)*100,2), # percent
              Smoking_Status_Code.Count=sum(Any_Invalid_Smoking_Status_Code, na.rm=TRUE)) # count
  
  return(
    list(smoking_status_examples=smoking_status_examples,
         smoking_status_summary=smoking_status_summary)
  )
}
