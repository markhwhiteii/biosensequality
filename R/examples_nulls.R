#' Generate Examples of Null Fields
#'
#' @description
#' This function will return the C_BioSense_ID for visits where required fields are missing, as well as what fields that are missing.
#' As mentioned in `get_req_nulls`, fields were treated as null if they were null in every single record of the visit, *except* for
#' ones that had to be present in every record and thus are done at the record-level: Treating_Facility_ID, Trigger_Event, Message_Control_ID,
#' Processing_ID, Version_ID, Message_Profile_ID, Recorded_Date_Time, Message_Date_Time, First_Patient_ID, and Medical_Record_Number.
#'
#' @param i A facility ID, as a string.
#' @param data The raw data from BioSense on which these examples will be generated.
#' @return A data frame containing the patient-visit IDs and fields for this visit that were null.
#' @import dplyr
#' @export
examples_nulls <- function(i, data) {

  req_all_fields <- c("Treating_Facility_ID", "Trigger_Event", "Message_Control_ID", "Processing_ID", "Version_ID",
                      "Message_Profile_ID", "Recorded_Date_Time", "Message_Date_Time", "First_Patient_ID",
                      "Medical_Record_Number") # fields required on all messages

  req_pv_fields <- c("Discharge_Date_Time", "Diagnosis_Code", "Diagnosis_Description", "Diagnosis_Type", "Discharge_Disposition",
                     "Insurance_Company_ID", "Administrative_Sex", "Race_Code", "Race_Description", "Patient_City", "Patient_Zip",
                     "Patient_State", "C_Patient_County", "Patient_Country", "Ethnicity_Code", "Ethnicity_Description",
                     "Patient_Class_Code", "Visit_ID", "Admit_Reason_Description", "Chief_Complaint_Text",
                     "Facility_Type_Code", "Facility_Type_Description", "Age_Reported",
                     "Age_Units_Reported", "Triage_Notes", "Clinical_Impression", "Height", "Height_Units", "Weight",
                     "Weight_Units", "Smoking_Status_Code", "Smoking_Status_Description") # fields required once per visit

  for (j in 1:length(req_all_fields)) { # for all required fields
    if (j==1) { # for first, will initialize the output data frame
      output <- data %>% # take data
        filter(C_Biosense_Facility_ID==i) %>% # data from just facility of interest
        select(C_BioSense_ID, req_all_fields[j]) %>% # get just patient visit and field
        filter(any(is.na(.))) %>% # filter just nas
        transmute(C_BioSense_ID=C_BioSense_ID, # keep patient visit
                  Null_Field=req_all_fields[j]) # list the null field, dispose of the actual null field
    } else {
      output <- data %>%
        filter(C_Biosense_Facility_ID==i) %>%
        select(C_BioSense_ID, req_all_fields[j]) %>%
        filter(any(is.na(.))) %>%
        transmute(C_BioSense_ID=C_BioSense_ID,
                  Null_Field=req_all_fields[j]) %>%
        bind_rows(output, .)
    }
  }

  for (k in 1:length(req_pv_fields)) {
    output <- data %>% # take data
      filter(C_Biosense_Facility_ID==i) %>% # data from just facility of interest
      select(C_BioSense_ID, req_pv_fields[k]) %>% # get just patient visit and field
      mutate(Null=ifelse(is.na(.[,req_pv_fields[k]]), TRUE, FALSE)) %>% # create variable that says if field for that row is null
      group_by(C_BioSense_ID) %>% # group by patient visit
      mutate(All_Null=ifelse(mean(Null)==1, TRUE, FALSE)) %>% # All null if all of entries are null
      ungroup() %>% # explicitly ungroup
      filter(All_Null==TRUE) %>% # get where all rows for patient visit are null
      transmute(C_BioSense_ID=C_BioSense_ID, # keep biosense id
                Null_Field=req_pv_fields[k]) %>% # rename null field with what is null
      bind_rows(output, .)
  }

  # get rid of dupes
  output <- distinct(output)
}
