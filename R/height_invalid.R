#' Getting Invalid Examples and Summaries for Height and Height_Units
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of height invalids.
#' 
#' The valid values were taken from the `"PHVS_HeightUnit_UCUM_V1.xls"` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept names that are considered valid by calling `data("height_units")`.
#' 
#' This function will test to see if valid Height_Units are reported. It also, checks however: If Height is reported,
#' are Height_Units also reported? If Height_Units are reported, is Height also reported? Heights without Units are considered
#' invalid; similarly, Units are considered invalid if Height is not reported with the Units.
#' 
#' @param data The raw data from BioSense on which you will do the invalid height checks.
#' @return A list of two data frames: examples and summary for invalid Height and Height_Units.
#' @import dplyr
#' @export
height_invalid <- function(data) {
  
  # generate valid values
  data("height_units", envir=environment())
  
  valid_height_values <- height_units %>% # what we want is on the second sheet
    select(Concept.Name) %>% # the variable we want is called concept name
    filter(!is.na(Concept.Name)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # unname stuff
    toupper() # make everything uppercase
  
  # generate examples
  height_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Height_Units, Height)) %>%  # taking just the variables we need
    mutate(Height_Units=toupper(as.character(Height_Units)), # make as character and uppercase
           Invalid_Height_Units=case_when(
             is.na(Height_Units) ~ NA, # if na, then keep na
             Height_Units %in% valid_height_values ~ FALSE, # if height units is in valid list, then false invalid
             !Height_Units %in% valid_height_values ~ TRUE, # if it isn't in there, then true invalid
           ),
           Missing_Height_Units_Given_Height=ifelse(is.na(Height_Units) & !is.na(Height), TRUE, FALSE),
           Missing_Height_Given_Height_Units=ifelse(!is.na(Height_Units) & is.na(Height), TRUE, FALSE))
  
  # generate summary
  height_summary <- height_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Invalid_Height_Units=case_when(
        all(is.na(Invalid_Height_Units)) ~ NA, # if all na keep na
        sum(Invalid_Height_Units, na.rm=TRUE) == 0 ~ FALSE, # if all false, then visit is false invalid
        TRUE ~ TRUE # otherwise, true
      ),
      Any_Missing_Height_Units_Given_Height=case_when(
        all(is.na(Missing_Height_Units_Given_Height)) ~ NA,
        sum(Missing_Height_Units_Given_Height, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      Any_Missing_Height_Given_Height_Units=case_when(
        all(is.na(Missing_Height_Given_Height_Units)) ~ NA,
        sum(Missing_Height_Given_Height_Units, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>% 
    slice(1) %>% # one row per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by faciltiy
    summarise(Height_Units.Percent=round(mean(Any_Invalid_Height_Units, na.rm=TRUE)*100,2), # percent invalid
              Height_Units_Missing_Given_Height.Percent=round(mean(Any_Missing_Height_Units_Given_Height, na.rm=TRUE)*100,2), # percent missing units
              Height_Missing_Given_Height_Units.Percent=round(mean(Any_Missing_Height_Given_Height_Units, na.rm=TRUE)*100,2), # percent missing height
              Height_Units.Count=sum(Any_Invalid_Height_Units, na.rm=TRUE), # count invalid
              Height_Units_Missing_Given_Height.Count=sum(Any_Missing_Height_Units_Given_Height, na.rm=TRUE), # count missing units
              Height_Missing_Given_Height_Units.Count=sum(Any_Missing_Height_Given_Height_Units, na.rm=TRUE)) # count missing height
  
  return(
    list(height_examples=height_examples,
         height_summary=height_summary)
  )
}
