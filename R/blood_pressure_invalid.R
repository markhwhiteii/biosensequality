#' Getting Invalid Examples and Summaries for for Systolic_Diastolic_Blood_Pressure and Units
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of Systolic_Diastolic_Blood_Pressure
#' and Systolic_Diastolic_Blood_Pressure_Units. This function will also mark invalid if Units are given, but Blood_Pressure
#' is not; it will also return invalid if Blood_Pressure is given, but Units are not. Therefore, it does three total checks:
#' Are the units invalid? Are units missing, given a blood pressure? Is blood pressure missing, given units?
#' 
#' The valid values were taken from the `PHVS_BloodPressureUnit_UCUM_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept names that are considered valid by calling `data("blood_pressure_units")`.
#' 
#' @param data The raw data from BioSense on which you will do the blood pressure checks.
#' @return A list of two data frames: examples and summary for blood pressure checks.
#' @import dplyr
#' @export
blood_pressure_invalid <- function(data) {
  
  # getting vector of valid blood pressure units
  data("blood_pressure_units", envir=environment())
  
  valid_bp_values <- blood_pressure_units %>% # take data
    select(Concept.Name) %>% # the variable we want is called concept name
    filter(!is.na(Concept.Name)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # unname stuff
    toupper() # put it all in uppercase
  
  # generate examples data
  blood_pressure_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Systolic_Diastolic_Blood_Pressure, Systolic_Diastolic_Blood_Pressure_Units)) %>% # select relevant vars
    mutate(Systolic_Diastolic_Blood_Pressure_Units=toupper(as.character(Systolic_Diastolic_Blood_Pressure_Units)), # uppercase and character
           Invalid_Blood_Pressure_Units=case_when(
             is.na(Systolic_Diastolic_Blood_Pressure_Units) ~ NA, # na if initial blood pressure is na
             Systolic_Diastolic_Blood_Pressure_Units %in% valid_bp_values ~ FALSE, # if units is in the valid values, then invalid is false
             !Systolic_Diastolic_Blood_Pressure_Units %in% valid_bp_values ~ TRUE # if units is not in valid values, then invalid is true
           ),
           Missing_BP_Units_Given_BP=ifelse(is.na(Systolic_Diastolic_Blood_Pressure_Units) & !is.na(Systolic_Diastolic_Blood_Pressure), TRUE, FALSE), # invalid if we have one without the other
           Missing_BP_Given_BP_Units=ifelse(!is.na(Systolic_Diastolic_Blood_Pressure_Units) & is.na(Systolic_Diastolic_Blood_Pressure), TRUE, FALSE)) # invalid if we have one without the other
  
  # generate summary data
  blood_pressure_summary <- blood_pressure_examples %>% # take examples data frame
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Invalid_Units=case_when(
        all(is.na(Invalid_Blood_Pressure_Units)) ~ NA, # if all na, keep na
        sum(Invalid_Blood_Pressure_Units, na.rm=TRUE) == 0 ~ FALSE, # if zero, all invalid checks false, so visit is good
        TRUE ~ TRUE # if not, invalid is true
      ),
      Any_Missing_Units=case_when(
        all(is.na(Missing_BP_Units_Given_BP)) ~ NA, # if all na, keep na
        sum(Missing_BP_Units_Given_BP, na.rm=TRUE) == 0 ~ FALSE, # if zero, all invalid checks false, so visit is good
        TRUE ~ TRUE # if not, invalid is true
      ),
      Any_Missing_BP=case_when(
        all(is.na(Missing_BP_Given_BP_Units)) ~ NA, # if all na, keep na
        sum(Missing_BP_Given_BP_Units, na.rm=TRUE) == 0 ~ FALSE, # if zero, all invalid checks false, so visit is good
        TRUE ~ TRUE # if not, invalid is true
      )
    ) %>% 
    slice(1) %>% # get one observation per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Systolic_Diastolic_Blood_Pressure_Units.Percent=round(mean(Any_Invalid_Units, na.rm=TRUE)*100,2), # percent invalid blood pressure units
              Systolic_Diastolic_Blood_Pressure_Missing_Given_BP_Units.Percent=round(mean(Any_Missing_BP, na.rm=TRUE)*100,2), # percent missing blood pressure given units
              Systolic_Diastolic_Blood_Pressure_Units_Missing_Given_BP.Percent=round(mean(Any_Missing_Units, na.rm=TRUE)*100,2), # percent missing units given blood pressure
              Systolic_Diastolic_Blood_Pressure_Units.Count=sum(Any_Invalid_Units, na.rm=TRUE), # count invalid units
              Systolic_Diastolic_Blood_Pressure_Missing_Given_BP_Units.Count=sum(Any_Missing_BP, na.rm=TRUE), # count missing pressure given units
              Systolic_Diastolic_Blood_Pressure_Units_Missing_Given_BP.Count=sum(Any_Missing_Units, na.rm=TRUE)) # count missing units given pressure
  
  return(
    list(blood_pressure_examples=blood_pressure_examples,
         blood_pressure_summary=blood_pressure_summary)
  )
}
