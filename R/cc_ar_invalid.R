#' Getting Invalid Examples and Summaries for Chief_Complaint_Text and Admit_Reason_Description
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for for counts and percentages of Chief_Complaint and Admit_Reason
#' text/descriptions that are shorter than three characters long.
#'
#' @param data The raw data from BioSense on which you will do the checks.
#' @return A list of two data frames: examples and summary for Chief_Complaint_Text and Admit_Reason_Description that are too short.
#' @import dplyr
#' @importFrom stringr str_length
#' @export
cc_ar_invalid <- function(data) {

  # generate examples
  cc_ar_examples <- data %>% # take data
    select(c(C_BioSense_ID, C_Biosense_Facility_ID, Chief_Complaint_Text, Admit_Reason_Description)) %>% # get vars you need
    mutate(
      Chief_Complaint_Text=toupper(Chief_Complaint_Text), # upper everything
      Admit_Reason_Description=toupper(Admit_Reason_Description), # upper everything
      Chief_Complaint_Text_Short=ifelse(str_length(Chief_Complaint_Text) < 3, TRUE, FALSE), # if less than three, true invalid, else false
      Admit_Reason_Description_Short=ifelse(str_length(Admit_Reason_Description) < 3, TRUE, FALSE) # same as above
    )

  # generate summary
  cc_ar_summary <- cc_ar_examples %>% # take examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(
      Any_Chief_Complaint_Text_Short=case_when(
        all(is.na(Chief_Complaint_Text_Short)) ~ NA,
        sum(Chief_Complaint_Text_Short, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      Any_Admit_Reason_Description_Short=case_when(
        all(is.na(Admit_Reason_Description_Short)) ~ NA,
        sum(Admit_Reason_Description_Short, na.rm=TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    slice(1) %>% # one obv per visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Chief_Complaint_Text_Short.Percent=round(mean(Any_Chief_Complaint_Text_Short, na.rm=TRUE)*100,2),
              Admit_Reason_Description_Short.Percent=round(mean(Any_Admit_Reason_Description_Short, na.rm=TRUE)*100,2),
              Chief_Complaint_Text_Short.Count=sum(Any_Chief_Complaint_Text_Short, na.rm=TRUE),
              Admit_Reason_Description_Short.Count=sum(Any_Admit_Reason_Description_Short, na.rm=TRUE))

  # output
  return(
    list(cc_ar_examples=cc_ar_examples,
         cc_ar_summary=cc_ar_summary)
  )
}
