#' Getting Invalid Examples and Summaries for Patient_Country
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of cases with invalid Patient_Country.
#' 
#' The valid values were taken from the `PHVS_Country_ISO_3166-1_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("country")`.
#' 
#' @param data The raw data From BioSense on which you will do the invalid Patient_Country check.
#' @return A list of two data frames: examples and summary for Patient_Country.
#' @import dplyr
#' @export
country_invalid <- function(data) {
  
  # get valid values for country
  data("country", envir=environment())
  
  valid_country_values <- country %>% # take data
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # unname stuff
    toupper() # uppercase everything
  
  # generate example file
  country_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Patient_Country)) %>%  # taking just the variables we need
    mutate(Patient_Country=toupper(as.character(Patient_Country)), # make as character and uppercase
           Invalid_Patient_Country=case_when(
             is.na(Patient_Country) ~ NA, # if field is na, then invalid remains na
             Patient_Country %in% valid_country_values ~ FALSE, # if field includes one of the allowed values, then invalid is false
             !Patient_Country %in% valid_country_values ~ TRUE # if field does not include allowed value, then invalid is true
           ))
  
  # generating summary data
  country_summary <- country_examples %>% # take data
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Patient_Country=case_when(
      all(is.na(Invalid_Patient_Country)) ~ NA, # if all checks na, then case is na
      sum(Invalid_Patient_Country, na.rm=TRUE) == 0 ~ FALSE, # if no checks true, then false
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # take one observation per patient visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Patient_Country.Percent=round(mean(Any_Invalid_Patient_Country, na.rm=TRUE)*100,2), # percent
              Patient_Country.Count=sum(Any_Invalid_Patient_Country, na.rm=TRUE)) # count
  
  return(
    list(country_examples=country_examples,
         country_summary=country_summary)
  )  
}
