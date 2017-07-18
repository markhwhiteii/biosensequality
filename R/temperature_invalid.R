#' Getting Invalid Examples and Summaries for Initial_Temp and Initial_Temp_Units
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of patient temperature invalids.
#' 
#' This is a more forgiving function. It counts all of the following as valid units: FAHRENHEIT, DEGREE FAHRENHEIT, CELSIUS, DEGREE CELSIUS,
#' FARENHEIT, DEGREE FARENHEIT, CELCIUS, DEGREE CELCIUS.
#' 
#' If temperature is in C, then it has to be between 23 and 44 inclusive, or else it is invalid  ("OOR" for "out of range")
#' If it is in F, then it has to be between 73 and 110 inclusive, or else it is invalid ("OOR" for "out of range"). Temp is
#' considered invalid of Units are not reported with it; similarly, Units are considered invalid of Temp is not reported with them.
#' 
#' @param data The raw data from BioSense on which you will do the invalid temperature checks.
#' @return A list of two data frames: examples and summary for invalid Initial_Temp and Initial_Temp_Units.
#' @import dplyr
#' @export
temperature_invalid <- function(data) {
  
  # generate examples
  temperature_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Initial_Temp, Initial_Temp_Units)) %>% # get only vars we need
    mutate(Initial_Temp_Units=as.character(toupper(Initial_Temp_Units)), # upper casing everything
           Invalid_Temp_Units=ifelse(Initial_Temp_Units %in% c("FAHRENHEIT", "DEGREE FAHRENHEIT", "CELSIUS", "DEGREE CELSIUS",
                                                               "FARENHEIT", "DEGREE FARENHEIT", "CELCIUS", "DEGREE CELCIUS") & # if units matches one of these
                                       !is.na(Initial_Temp_Units), # and it isn't null
                                     FALSE, # then invalid is false
                                     ifelse(!is.na(Initial_Temp_Units), # otherwise, if it is not null 
                                            TRUE, # then invalid is true
                                            NA)), # otherwise, it is na
           Temp_OOR=ifelse(Invalid_Temp_Units == TRUE | is.na(Invalid_Temp_Units), NA, # if temperature is invalid or na, then out of range is na
                           ifelse(Initial_Temp_Units %in% c("FAHRENHEIT", "DEGREE FAHRENHEIT", "FARENHEIT", "DEGREE FARENHEIT") & # if it matches one of these
                                    Initial_Temp >= 73 & Initial_Temp <= 110, # and it is greater than 73 and less than 110
                                  FALSE, # then oor is false
                                  ifelse(Initial_Temp_Units %in% c("CELSIUS", "DEGREE CELSIUS", "CELCIUS", "DEGREE CELCIUS") & # otherwise, if it matches one of these
                                           Initial_Temp >= 23 & Initial_Temp <= 44, # and meets both of these conditions
                                         FALSE, # then oor is false
                                         TRUE))), # otherwise, oor is true
           Missing_Temp_Given_Units=ifelse(is.na(Initial_Temp) & Invalid_Temp_Units==FALSE, TRUE, FALSE), # if temp is null but temp units is valid, true that we are missing temp given units
           Missing_Units_Given_Temp=ifelse(is.na(Initial_Temp_Units) & Temp_OOR==FALSE, TRUE, FALSE)) # if temp units is null but temp is valid, true that we are missing units given temp
  
  # generate summary
  temperature_summary <- temperature_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Invalid_Temp_Units=case_when(
        all(is.na(Invalid_Temp_Units)) ~ NA, # if all na, keep na
        sum(Invalid_Temp_Units, na.rm=TRUE) == 0 ~ FALSE, # if none invalid, then false invalid
        TRUE ~ TRUE # otherwise, true
      ),
      Any_Temp_OOR=case_when(
        all(is.na(Temp_OOR)) ~ NA,
        sum(Temp_OOR, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      Any_Missing_Temp_Given_Units=case_when(
        all(is.na(Missing_Temp_Given_Units)) ~ NA,
        sum(Missing_Temp_Given_Units, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      Any_Missing_Units_Given_Temp=case_when(
        all(is.na(Missing_Units_Given_Temp)) ~ NA,
        sum(Missing_Units_Given_Temp, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>% 
    slice(1) %>% # one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Initial_Temp_Units.Percent=round(mean(Any_Invalid_Temp_Units, na.rm=TRUE)*100,2),
              Initial_Temp_Out_Of_Range.Percent=round(mean(Any_Temp_OOR, na.rm=TRUE)*100,2),
              Initial_Temp_Missing_Given_Units.Percent=round(mean(Any_Missing_Temp_Given_Units, na.rm=TRUE)*100,2),
              Initial_Temp_Units_Missing_Given_Temp.Percent=round(mean(Any_Missing_Units_Given_Temp, na.rm=TRUE)*100,2),
              Initial_Temp_Units.Count=sum(Any_Invalid_Temp_Units, na.rm=TRUE),
              Initial_Temp_Out_Of_Range.Count=sum(Any_Temp_OOR, na.rm=TRUE),
              Initial_Temp_Missing_Given_Units.Count=sum(Any_Missing_Temp_Given_Units, na.rm=TRUE),
              Initial_Temp_Units_Missing_Given_Temp.Count=sum(Any_Missing_Units_Given_Temp, na.rm=TRUE))
  
  return(
    list(temperature_examples=temperature_examples,
         temperature_summary=temperature_summary)
  )
}
