#' Checking If Death Date Time, Death Indicator, and Death Discharge Disposition Are Reported Together
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of death invalids.
#' 
#' If Discharge_Disposition says the patient died, they should report a Death_Indicator AND Death_Date_Time. 
#' If they do not, this function counts that as invalid. If Death_Indicator is returned, then Death_Date_Time should be reported. 
#' If they do not, this function counts that as invalid. Death_Indicator should be reported if a Death_Date_Time is reported; 
#' if it is not, this function counts that as invalid.
#' 
#' @param data The raw data from BioSense on which you will do the invalid death checks.
#' @return A list of two data frames: examples and summary for these invalid death checks.
#' @import dplyr
#' @export
death_invalid <- function(data) {
  
  # generate examples
  death_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Death_Date_Time, Patient_Death_Indicator, Discharge_Disposition)) %>% # taking variables we need
    mutate(Missing_Death_Given_Discharge_Disposition=ifelse(Discharge_Disposition %in% c("20", "40", "41", "42") & # if dd includes any of these four and
                                                              (is.na(Death_Date_Time) | is.na(Patient_Death_Indicator)), # either of the death fields are missing
                                                            TRUE, FALSE), # then it is missing, else it is not
           Missing_Death_Date_Time_Given_Indicator=ifelse(!is.na(Patient_Death_Indicator) & is.na(Death_Date_Time), TRUE, FALSE), # indicator present but date time is not
           Missing_Death_Indicator_Given_Date_Time=ifelse(is.na(Patient_Death_Indicator) & !is.na(Death_Date_Time), TRUE, FALSE)) # date time not present, indicator is
  
  # generate summary
  death_summary <- death_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Missing_Death_Given_Discharge_Disposition=case_when(
        all(is.na(Missing_Death_Given_Discharge_Disposition)) ~ NA, # if all na, keep na
        sum(Missing_Death_Given_Discharge_Disposition, na.rm=TRUE) == 0 ~ FALSE, # if all records false, visit false
        TRUE ~ TRUE # otherwise true
      ),
      Any_Missing_Death_Date_Time_Given_Indicator=case_when(
        all(is.na(Missing_Death_Date_Time_Given_Indicator)) ~ NA, # if all na, keep na
        sum(Missing_Death_Date_Time_Given_Indicator, na.rm=TRUE) == 0 ~ FALSE, # if all records false, visit false
        TRUE ~ TRUE # otherwise true
      ),
      Any_Missing_Death_Indicator_Given_Date_Time=case_when(
        all(is.na(Missing_Death_Indicator_Given_Date_Time)) ~ NA, # if all na, keep na
        sum(Missing_Death_Indicator_Given_Date_Time, na.rm=TRUE) == 0 ~ FALSE, # if all records false, visit false
        TRUE ~ TRUE # otherwise true
      )
    ) %>% 
    slice(1) %>% # get one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility id
    summarise(Missing_Death_Given_Discharge_Disposition.Percent=round(mean(Any_Missing_Death_Given_Discharge_Disposition)*100,2),
              Missing_Death_Date_Time_Given_Indicator.Percent=round(mean(Any_Missing_Death_Date_Time_Given_Indicator)*100,2),
              Missing_Death_Indicator_Given_Date_Time.Percent=round(mean(Any_Missing_Death_Indicator_Given_Date_Time)*100,2),
              Missing_Death_Given_Discharge_Disposition.Count=sum(Any_Missing_Death_Given_Discharge_Disposition),
              Missing_Death_Date_Time_Given_Indicator.Count=sum(Any_Missing_Death_Date_Time_Given_Indicator),
              Missing_Death_Indicator_Given_Date_Time.Count=sum(Any_Missing_Death_Indicator_Given_Date_Time))
  
  return(
    list(death_examples=death_examples,
         death_summary=death_summary)
  )
}
