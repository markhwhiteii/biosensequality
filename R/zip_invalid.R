#' Getting Invalid Examples and Summaries for Patient_Zip
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Patient_Zip.
#' 
#' Patient_Zip is considered invalid if it: first, contains anything that is non-numeric or a hypen; second, is not five,
#' nine, or ten characters (ten is included because of the hyphen); third, if it is all zeros, ones, or nines.
#' 
#' @param data The data on which you will do the invalid patient ZIP checks.
#' @return A list of two data frames: examples and summary for invalid Patient_Zip.
#' @import dplyr
#' @importFrom stringr str_length
#' @export
zip_invalid <- function(data) {
  
  # generate examples
  zip_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Patient_Zip)) %>% # limit dataset to just faciltiy id and patient zip
    mutate(Zip_Invalid=case_when(
      is.na(Patient_Zip) ~ NA, # if na, keep as na
      grepl("[^0-9-]", Patient_Zip) | # if it includes anything non-numeric (except for a -), 
        (str_length(Patient_Zip) != 5 & str_length(Patient_Zip) != 9 & str_length(Patient_Zip) != 10) | # if it is not five, nine, or ten digits long, 
        Patient_Zip=="00000" | Patient_Zip=="000000000" | # if it is all zeros, 
        Patient_Zip=="11111" | Patient_Zip=="111111111" | # if it is all ones, 
        Patient_Zip=="99999" | Patient_Zip=="999999999" ~ TRUE,
      TRUE ~ FALSE # otherwise, it is valid
      ))

  # generate summary
  zip_summary <- zip_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Zip_Invalid=case_when(
      all(is.na(Zip_Invalid)) ~ NA, # if all are na, keep na
      sum(Zip_Invalid, na.rm=TRUE) == 0 ~ FALSE, # if none are invalid, whole visit is false 
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Patient_Zip.Percent=round(mean(Any_Zip_Invalid, na.rm=TRUE)*100,2), # percent
              Patient_Zip.Count=sum(Any_Zip_Invalid, na.rm=TRUE)) # count
  
  return(
    list(zip_examples=zip_examples,
         zip_summary=zip_summary)
  )
}
