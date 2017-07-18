#' Getting Invalid Examples and Summaries for Age_Reported and Age_Units_Reported
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Age_Reported 
#' and Age_Units_Reported.
#' 
#' The valid values were taken from the `PHVS_AgeUnit_SyndromicSurveillance_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets. 
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept names that are considered valid by calling `data("age_units")`.
#' 
#' @param data The raw data from BioSense on which you will do the invalid ages and age units checks.
#' @return A list of two data frames: examples and summary for invalid Age_Reported and Age_Units_Reported.
#' @import dplyr
#' @export
age_invalid <- function(data) {
  
  # import the valid admit values
  data("age_units", envir=environment())
  
  valid_age_units <- age_units %>% # take data
    select(Concept.Name) %>% # the variable we want is called concept name
    filter(!is.na(Concept.Name)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>%  # remove the names to leave bare values
    toupper() # upper case everything
  
  # allow the units to be plural, as well
  temp <- c() # initialize temporary vector
  for (i in 1:length(valid_age_units)) { # for each element in the valid_age_values vector
    if (valid_age_units[i] != "UNKNOWN") { # for ones that do not equal unknown
      temp <- c(temp, paste0(valid_age_units[i], "S")) # adding an s to that and appending it to the empty vector
    }
  }
  valid_age_units <- c(valid_age_units, temp) # combine vector with new plurals to the list from the dataset
  
  # generate examples file
  age_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Age_Reported, Age_Units_Reported)) %>% # get only the variables we need right now
    mutate(Age_Units_Reported=toupper(Age_Units_Reported), # upper casing everything
           Invalid_Age_Units=case_when(
             is.na(Age_Units_Reported) ~ NA, # if it is na, keep it na
             Age_Units_Reported %in% valid_age_units ~ FALSE, # if units reported are valid, then false invalid
             !Age_Units_Reported %in% valid_age_units ~ TRUE # if units are not in there, then true invalid
           ),
           Invalid_Age=case_when(
             is.na(Age_Units_Reported) & !is.na(Age_Reported) ~ TRUE, # invalid if they don't report age units, but report age
             (Age_Units_Reported=="YEAR" | Age_Units_Reported=="YEARS") & Age_Reported >= 2 & Age_Reported <=120 ~ FALSE, # if in years and between 2 and 120, then not invalid
             (Age_Units_Reported=="MONTH" | Age_Units_Reported=="MONTHS") & Age_Reported >= 1 & Age_Reported <=24 ~ FALSE, # if in months and between 1 and 24, then not invalid
             (Age_Units_Reported=="WEEK" | Age_Units_Reported=="WEEKS") & Age_Reported >= 1 & Age_Reported <=20 ~ FALSE, # if in weeks and between 1 and 20, then not invalid
             (Age_Units_Reported=="DAY" | Age_Units_Reported=="DAYS") & Age_Reported >= 1 & Age_Reported <=30 ~ FALSE, # if in days and between 1 and 30, then not invalid
             TRUE ~ TRUE, # otherwise, it is invalid
           ))
  
  age_summary <- age_examples %>% # take these examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Invalid_Age_Units=case_when(
        all(is.na(Invalid_Age_Units)) ~ NA, # if all invalid checks are na, keep na
        sum(Invalid_Age_Units, na.rm=TRUE) == 0 ~ FALSE, # if zero, all invalid checks are false, so visit is good
        TRUE ~ TRUE # if it is not, invalid is true
      ),
      Any_Invalid_Age=case_when(
        all(is.na(Invalid_Age)) ~ NA, # all na, then na
        sum(Invalid_Age, na.rm=TRUE) == 0 ~ FALSE, # if zero, all checks are false, so visit is good
        TRUE ~ TRUE # if it is not, invalid is true
      )
    ) %>% 
    slice(1) %>% # get one observation per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility 
    summarise(Age_Reported.Percent=round(mean(Any_Invalid_Age, na.rm=TRUE)*100,2), # get percent
              Age_Units_Reported.Percent=round(mean(Any_Invalid_Age_Units, na.rm=TRUE)*100,2), # get percent
              Age_Reported.Count=sum(Any_Invalid_Age, na.rm=TRUE), # get count
              Age_Units_Reported.Count=sum(Any_Invalid_Age_Units, na.rm=TRUE))
  
  return(
    list(age_examples=age_examples,
         age_summary=age_summary)
  )
}
