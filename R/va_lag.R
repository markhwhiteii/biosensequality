#' Visit-Arrival Lag
#'
#' @description 
#' Calculates the average time lag between when: a patient arrives and the first record for their visit is received by the NSSP BioSense Platform.
#' A summary is returned at the facility level.
#' 
#' @param data The raw data from BioSense on which you will do these summaries.
#' @param names A table of Facility_Name and C_Biosense_Facility_ID which taken from the MFT using the `pull_data` function.
#' @param offset The number of hours you wish to offset Arrived_Date_Time (which is in UTC). The offset value is how far off your local time zone is from UTC. 
#' For example, the Central Time Zone would set this to 5 or 6, depending on if it is daylight savings or not. This value should be an integer. 
#' @return A data frame with three columns: facility name, facility ID, and average lag between patient visit and first record arrival.
#' @import dplyr
#' @import tidyr
#' @export
va_lag <- function(data, names, offset) {
  return(
    data %>% # take raw data
      select(C_BioSense_ID, C_Biosense_Facility_ID, Arrived_Date_Time, C_Visit_Date_Time) %>% # select only vars we need
      group_by(C_BioSense_ID) %>% # group by patient visit
      slice(which.min(Arrived_Date_Time)) %>% # get the first arrived date time
      slice(1) %>% # get one, just in case there are ties
      mutate(
        Arrived_Date_Time = lubridate::ymd_hms(Arrived_Date_Time) - lubridate::hours(offset), # adjust utc arrived to local time
        lag=as.numeric(difftime(Arrived_Date_Time, C_Visit_Date_Time, units="hours")) # get lag, in hours
      ) %>% 
      ungroup() %>% # ungroup explicitly
      group_by(C_Biosense_Facility_ID) %>% # regroup by facility
      summarise(First_Message=round(mean(lag, na.rm=TRUE),2)) %>% # get the average, round by two digits
      right_join(names, ., by="C_Biosense_Facility_ID") # join with the names data
  )
}
