#' Getting Invalid Examples and Summaries for Initial_Pulse_Oximetry and Initial_Pulse_Oximetry_Units
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of pulse oximetry invalids.
#' 
#' The valid values were taken from the `PHVS_PulseOximetryUnit_UCUM_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept names that are considered valid by calling `data("pulse_oximetry_units")`.
#' 
#' This function will do four invalid checks: Initial_Pulse_Oximetry is considered invalid if it is above 100 or less than 50;
#' Initial_Pulse_Oximetry_Units are considered invalid if they are not in the valid concept names; Initial_Pulse_Oximetry is considered
#' invalid if Units are not reported; lastly, Initial_Pulse_Oximetry_Units are considered invalid if a corresponding Initial_Pulse_Oximetry
#' is not reported.
#' 
#' @param data The raw data from BioSense on which you will do the invalid pulse oximetry checks.
#' @return A list of two data frames: examples and summary for Initial_Pulse_Oximetry and Initial_Pulse_Oximetry_Units.
#' @import dplyr
#' @export
pulseox_invalid <- function(data) {
  
  # generate valid values
  data("pulse_oximetry_units", envir=environment())
  
  valid_pulseox_values <- pulse_oximetry_units %>% # bring in data 
    select(Concept.Name) %>% # the variable we want is called concept name
    filter(!is.na(Concept.Name)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # unname stuff
    toupper() # put as uppercase
  
  # generate examples
  pulseox_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Initial_Pulse_Oximetry, Initial_Pulse_Oximetry_Units)) %>% # select relevant vars
    mutate(Initial_Pulse_Oximetry_Units=toupper(as.character(Initial_Pulse_Oximetry_Units)), # uppercase and character
           Invalid_Pulse_Oximetry_Units=case_when(
             is.na(Initial_Pulse_Oximetry) ~ NA, # if na, keep na
             Initial_Pulse_Oximetry_Units %in% valid_pulseox_values ~ FALSE, # if valid value, false
             !Initial_Pulse_Oximetry_Units %in% valid_pulseox_values ~ TRUE # if not valid value, true
           ),
           Invalid_Pulse_Oximetry=ifelse(Initial_Pulse_Oximetry >= 50 & Initial_Pulse_Oximetry <= 100, FALSE, TRUE), # false if field is in acceptable range
           Missing_Oximetry_Units_Given_Pulse_Ox=ifelse(is.na(Initial_Pulse_Oximetry_Units) & !is.na(Initial_Pulse_Oximetry), TRUE, FALSE), # can't have one without other
           Missing_Pulse_Ox_Given_Oximetry_Units=ifelse(!is.na(Initial_Pulse_Oximetry_Units) & is.na(Initial_Pulse_Oximetry), TRUE, FALSE)) # can't have one without other
  
  # generate summary
  pulseox_summary <- pulseox_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Invalid_Pulse_Oximetry=case_when(
        all(is.na(Invalid_Pulse_Oximetry)) ~ NA, # if na, keep na
        sum(Invalid_Pulse_Oximetry, na.rm=TRUE) == 0 ~ FALSE, # if none invalid, then false invalid
        TRUE ~ TRUE # otherwise, invalid
      ),
      Any_Invalid_Pulse_Oximetry_Units=case_when(
        all(is.na(Invalid_Pulse_Oximetry_Units)) ~ NA, 
        sum(Invalid_Pulse_Oximetry_Units, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      Any_Missing_Pulse_Ox_Given_Oximetry_Units=case_when(
        all(is.na(Missing_Pulse_Ox_Given_Oximetry_Units)) ~ NA,
        sum(Missing_Pulse_Ox_Given_Oximetry_Units, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      Any_Missing_Oximetry_Units_Given_Pulse_Ox=case_when(
        all(is.na(Missing_Oximetry_Units_Given_Pulse_Ox)) ~ NA,
        sum(Missing_Oximetry_Units_Given_Pulse_Ox, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>% 
    slice(1) %>% # take one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by pulse ox
    summarise(Initial_Pulse_Oximetry.Percent=round(mean(Any_Invalid_Pulse_Oximetry, na.rm=TRUE)*100,2),
              Initial_Pulse_Oximetry_Units.Percent=round(mean(Any_Invalid_Pulse_Oximetry_Units, na.rm=TRUE)*100,2),
              Initial_Pulse_Oximetry_Missing_Given_Units.Percent=round(mean(Any_Missing_Pulse_Ox_Given_Oximetry_Units, na.rm=TRUE)*100,2),
              Initial_Pulse_Oximetry_Units_Missing_Given_Pulse.Percent=round(mean(Any_Missing_Oximetry_Units_Given_Pulse_Ox, na.rm=TRUE)*100,2),
              Initial_Pulse_Oximetry.Count=sum(Any_Invalid_Pulse_Oximetry, na.rm=TRUE),
              Initial_Pulse_Oximetry_Units.Count=sum(Any_Invalid_Pulse_Oximetry_Units, na.rm=TRUE),
              Initial_Pulse_Oximetry_Missing_Given_Units.Count=sum(Any_Missing_Pulse_Ox_Given_Oximetry_Units, na.rm=TRUE),
              Initial_Pulse_Oximetry_Units_Missing_Given_Pulse.Count=sum(Any_Missing_Oximetry_Units_Given_Pulse_Ox, na.rm=TRUE))
  
  return(
    list(pulseox_examples=pulseox_examples,
         pulseox_summary=pulseox_summary)
  )
}
