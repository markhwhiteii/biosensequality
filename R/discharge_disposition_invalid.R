#' Getting Invalid for Examples and Summaries for Discharge_Disposition
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Discharge_Disposition.
#' 
#' The valid values were taken from the `PHVS_DischargeDisposition_HL7_2x_V3.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("discharge_disposition")`. Note that this
#' data frame does not include zeros in front of single-digit numbers 1 to 9; these are also added as valid values
#' in the function.
#' 
#' @param data The raw data from BioSense on which you will do the invalid discharge disposition checks.
#' @return A list of two data frames: examples and summary for invalid Discharge_Disposition.
#' @import dplyr
#' @export
discharge_disposition_invalid <- function(data) {
  
  # generate valid values
  data("discharge_disposition", envir=environment())
  
  valid_dd_values <- discharge_disposition %>% # what we want is on the second sheet
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # remove the names to leave bare values
    c("01","02","03","04","05","06","07","08","09") # including the single-digit numbers with the 0 at the beginning
  
  # generate examples
  discharge_disposition_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Discharge_Disposition)) %>%  # taking just the variables we need
    mutate(Discharge_Disposition=as.character(Discharge_Disposition),
           Invalid_Discharge_Disposition=case_when(
             is.na(Discharge_Disposition) ~ NA, # if field is na, then invalid is na
             Discharge_Disposition %in% valid_dd_values ~ FALSE, # if value is valid, then invalid is false
             !Discharge_Disposition %in% valid_dd_values ~ TRUE, # if it is not, then invalid is true
           ))
             
  # generate summary
  discharge_disposition_summary <- discharge_disposition_examples %>% 
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Discharge_Disposition=case_when(
      all(is.na(Invalid_Discharge_Disposition)) ~ NA, # if all na, keep na
      sum(Invalid_Discharge_Disposition, na.rm=TRUE) == 0 ~ FALSE, # if all false, then false
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # get one per visit
    ungroup() %>% # ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Discharge_Disposition.Percent=round(mean(Any_Invalid_Discharge_Disposition, na.rm=TRUE)*100,2), # percents
              Discharge_Disposition.Count=sum(Any_Invalid_Discharge_Disposition, na.rm=TRUE)) # counts
  
  return(
    list(discharge_disposition_examples=discharge_disposition_examples,
         discharge_disposition_summary=discharge_disposition_summary)
  )
}
