#' Getting Invalid Examples and Summaries for Race_Code
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Race_Code.
#' 
#' The valid values were taken from the `PHVS_RaceCategory_CDC_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("race")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid race checks.
#' @return A list of two data frames: examples and summary for invalid Race_Code.
#' @import dplyr
#' @export
race_invalid <- function(data) {
  
  # generate valid values
  data("race", envir=environment())
  
  valid_race_values <- race %>% # get data
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>%  # remove the names to leave bare values
    toupper() # upper case everything
  
  # generate examples
  race_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Race_Code)) %>%  # taking just the variables we need
    mutate(Race_Code=toupper(Race_Code), # upper casing
           Invalid_Race_Code=case_when(
             is.na(Race_Code) ~ NA, # if na keep na
             Race_Code %in% valid_race_values ~ FALSE, # if field includes one of allowed values, false invalid
             !Race_Code %in% valid_race_values ~ TRUE # if it does not, true invalid
           ))
  
  # generate summary
  race_summary <- race_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Race_Code=case_when(
      all(is.na(Invalid_Race_Code)) ~ NA, # if all is na, then keep na
      sum(Invalid_Race_Code, na.rm=TRUE) == 0 ~ FALSE, # if all false, then invalid false
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # take one observation per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Race_Code.Percent=round(mean(Any_Invalid_Race_Code, na.rm=TRUE)*100,2), # percent
              Race_Code.Count=sum(Any_Invalid_Race_Code, na.rm=TRUE)) # count
  
  return(
    list(race_examples=race_examples,
         race_summary=race_summary)
  )
}
