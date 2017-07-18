#' Getting Invalid Examples and Summaries for Ethnicity_Code
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Ethnicity_Code.
#' 
#' The valid values were taken from the `"PHVS_EthnicityGroup_CDC_V1.xls"` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("ethnicity")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid ethnicity checks.
#' @return A list of two data frames: examples and summary for invalid Ethnicity_Code.
#' @import dplyr
#' @export
ethnicity_invalid <- function(data) {
  
  # get valid values
  data("ethnicity", envir=environment())
  
  valid_eth_values <- ethnicity %>% # what we want is on the second sheet
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>%  # remove the names to leave bare values
    toupper() # upper case everything
  
  # get examples
  ethnicity_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Ethnicity_Code)) %>%  # taking just the variables we need
    mutate(Invalid_Ethnicity_Code=case_when(
      is.na(Ethnicity_Code) ~ NA, # if na, keep na
      Ethnicity_Code %in% valid_eth_values ~ FALSE, # if ethnicity code is in valid values, then invalid is false
      !Ethnicity_Code %in% valid_eth_values ~ TRUE # if it is not, then invalid is true
    ))
  
  # get summary
  ethnicity_summary <- ethnicity_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Ethnicity_Code=case_when(
      all(is.na(Invalid_Ethnicity_Code)) ~ NA, # if all na, keep na
      sum(Invalid_Ethnicity_Code, na.rm=TRUE) == 0 ~ FALSE, # if all false, then invalid false
      TRUE ~ TRUE # otherwise, invalid is true
    )) %>%
    slice(1) %>% # get one row per patient visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Ethnicity_Code.Percent=round(mean(Any_Invalid_Ethnicity_Code, na.rm=TRUE)*100,2), # percents
              Ethnicity_Code.Count=sum(Any_Invalid_Ethnicity_Code, na.rm=TRUE)) # counts
  
  return(
    list(ethnicity_examples=ethnicity_examples,
         ethnicity_summary=ethnicity_summary)
  )
}
