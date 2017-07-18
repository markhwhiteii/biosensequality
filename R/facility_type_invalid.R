#' Getting Invalid Examples and Summary for Facility_Type
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Facility_Type.
#' 
#' The valid values were taken from the `PHVS_FacilityVisitType_SyndromicSurveillance_V3.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("facility_type")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid facility type checks.
#' @return A list of two data frames: examples and summary for invalid Facility_Type.
#' @import dplyr
#' @export
facility_type_invalid <- function(data) {
  
  # generate valid values
  data("facility_type", envir=environment())
  
  valid_factype_values <- facility_type %>% # get file
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname()
  
  # generate examples
  facility_type_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Facility_Type_Code)) %>%  # taking just the variables we need
    mutate(Facility_Type_Code=toupper(as.character(Facility_Type_Code)), # make as character and uppercase
           Invalid_Facility_Type_Code=case_when(
             is.na(Facility_Type_Code) ~ NA, # if field is na, then invalid is na
             Facility_Type_Code %in% valid_factype_values ~ FALSE, # if code is in valid values, false
             !Facility_Type_Code %in% valid_factype_values ~ TRUE # if not, true
             ))
  
  # generate summary
  facility_type_summary <- facility_type_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Facility_Type_Code=case_when(
      all(is.na(Invalid_Facility_Type_Code)) ~ NA, # if all na, keep na
      sum(Invalid_Facility_Type_Code, na.rm=TRUE) == 0 ~ FALSE, # if all false, then false invalid
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # get one row per patient visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Facility_Type_Code.Percent=round(mean(Any_Invalid_Facility_Type_Code, na.rm=TRUE)*100,2), # percent
              Facility_Type_Code.Count=sum(Any_Invalid_Facility_Type_Code, na.rm=TRUE)) # count
  
  return(
    list(facility_type_examples=facility_type_examples,
         facility_type_summary=facility_type_summary)
  )
}
