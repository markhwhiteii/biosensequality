#' Getting Invalid Examples and Summaries for Weight and Weight_Units
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Weight and Weight_Units.
#' 
#' The valid values were taken from the `PHVS_WeightUnit_UCUM_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept names that are considered valid by calling `data("weight_units")`.
#' 
#' Weight is considered invalid of Weight_Units are missing; the reverse is also true.
#' 
#' @param data The raw data from BioSense on which you will do the invalid patient weight checks.
#' @return A list of two data frames: examples and summary for Weight and Weight_Units.
#' @import dplyr
#' @export
weight_invalid <- function(data) {
  
  # generate valid weight units values
  data("weight_units", envir=environment())
  
  valid_weight_values <- weight_units %>% # take file
    select(Concept.Name) %>% # the variable we want is called concept name
    filter(!is.na(Concept.Name)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # unname stuff
    toupper() # make everything uppercase
  
  # generate examples
  weight_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Weight_Units, Weight)) %>%  # taking just the variables we need
    mutate(Weight_Units=toupper(as.character(Weight_Units)), # make as character and uppercase
           Invalid_Weight_Units=case_when(
             is.na(Weight_Units) ~ NA, # if field is na, then invalid is na
             Weight_Units %in% valid_weight_values ~ FALSE, # if in valid, false
             !Weight_Units %in% valid_weight_values ~ TRUE # if not, true
           ),
           Missing_Weight_Units_Given_Weight=ifelse(is.na(Weight_Units) & !is.na(Weight), TRUE, FALSE), # need both
           Missing_Weight_Given_Weight_Units=ifelse(!is.na(Weight_Units) & is.na(Weight), TRUE, FALSE)) # need both
  
  # generate summary
  weight_summary <- weight_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Invalid_Weight_Units=case_when(
        all(is.na(Invalid_Weight_Units)) ~ NA, # if all na, keep it na
        sum(Invalid_Weight_Units, na.rm=TRUE) == 0 ~ FALSE, # if all false, then invalid false
        TRUE ~ TRUE # otherwise, true invalid
      ),
      Any_Missing_Weight_Units_Given_Weight=case_when(
        any(is.na(Missing_Weight_Units_Given_Weight)) ~ NA,
        sum(Missing_Weight_Units_Given_Weight, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      Any_Missing_Weight_Given_Weight_Units=case_when(
        all(is.na(Missing_Weight_Given_Weight_Units)) ~ NA,
        sum(Missing_Weight_Given_Weight_Units, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>% 
    slice(1) %>% # take one row
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>%  # group by facility
    summarise(Weight_Units.Percent=round(mean(Any_Invalid_Weight_Units, na.rm=TRUE)*100,2),
              Weight_Units_Missing_Given_Weight.Percent=round(mean(Any_Missing_Weight_Units_Given_Weight, na.rm=TRUE)*100,2),
              Weight_Missing_Given_Weight_Units.Percent=round(mean(Any_Missing_Weight_Given_Weight_Units, na.rm=TRUE)*100,2),
              Weight_Units.Count=sum(Any_Invalid_Weight_Units, na.rm=TRUE),
              Weight_Units_Missing_Given_Weight.Count=sum(Any_Missing_Weight_Units_Given_Weight, na.rm=TRUE),
              Weight_Missing_Given_Weight_Units.Count=sum(Any_Missing_Weight_Given_Weight_Units, na.rm=TRUE))
  
  return(
    list(weight_examples=weight_examples,
         weight_summary=weight_summary)
  )
}
