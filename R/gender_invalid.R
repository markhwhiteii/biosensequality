#' Getting Invalid Examples and Summaries for Administrative_Sex
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Administrative_Sex.
#' 
#' The valid values were taken from the `"PHVS_Gender_SyndromicSurveillance_V1.xls"` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("gender")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid Administrative_Sex checks.
#' @return A list of two data frames: examples and summary for Administrative_Sex.
#' @import dplyr
#' @export
gender_invalid <- function(data) {
  
  # generate valid values
  data("gender", envir=environment())
  
  valid_gender_values <- gender %>% # what we want is on the second sheet
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>%  # remove the names to leave bare values
    toupper() # upper case everything
  
  # generate examples
  gender_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Administrative_Sex)) %>%  # taking just the variables we need
    mutate(Administrative_Sex=toupper(Administrative_Sex), # upper casing administrative sex
           Invalid_Administrative_Sex=case_when(
             is.na(Administrative_Sex) ~ NA, # if na, keep na
             Administrative_Sex %in% valid_gender_values ~ FALSE, # if sex is a valid value, then false
             !Administrative_Sex %in% valid_gender_values ~ TRUE # if it is not, then true
           ))
  
  # generate summary
  gender_summary <- gender_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Administrative_Sex=case_when(
      all(is.na(Invalid_Administrative_Sex)) ~ NA, # if all na, keep na
      sum(Invalid_Administrative_Sex, na.rm=TRUE) == 0 ~ FALSE, # if all false, then whole record is false invalid
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Administrative_Sex.Percent=round(mean(Any_Invalid_Administrative_Sex, na.rm=TRUE)*100,2), # percent
              Administrative_Sex.Count=sum(Any_Invalid_Administrative_Sex, na.rm=TRUE)) # count
  
  return(
    list(gender_examples=gender_examples,
         gender_summary=gender_summary)
  )
}
