#' Generate Examples of Invalid Fields
#' 
#' NOTE: THIS FUNCTION IS NOT MEANT TO BE USED ON ITS OWN. IT IS USED INTERNALLY BY `write_reports`.
#' 
#' This function will return the C_BioSense_ID and field name for patient-visits where the entry was invalid.
#' 
#' @param i A facility ID, as a string.
#' @param invalid_examples A list of data frames that are the examples output from the `_invalid` functions.
#' @return A data frame containing the patient-visit IDs and fields for this visit that were invalid.
#' @import dplyr
#' @export
examples_invalids <- function(i, invalid_examples) {
  
  # too many little different things for every df to do it as a 
  # for loop or lapply/sapply; someone cleverer could figure it out sometime?
  
  ## admit source
  output <- invalid_examples[[1]] %>% # get examples
    filter(C_Biosense_Facility_ID==i & Invalid_Admit_Source==TRUE) %>% # for invalids in this facility
    select(C_BioSense_ID, Admit_Source) %>% # select vars we want
    distinct() # get rid of dupes
  
  ## age
  output <- invalid_examples[[2]] %>%
    filter(C_Biosense_Facility_ID==i & Invalid_Age==TRUE) %>%
    select(C_BioSense_ID, Age_Reported) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## age units
  output <- invalid_examples[[2]] %>%
    filter(C_Biosense_Facility_ID==i & Invalid_Age_Units==TRUE) %>%
    select(C_BioSense_ID, Age_Units_Reported) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## any e in visit
  output <- invalid_examples[[3]] %>% 
    filter(C_Biosense_Facility_ID==i & No_E_In_Visit==TRUE) %>% 
    select(C_BioSense_ID, No_E_In_Visit) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## blood pressure units
  output <- invalid_examples[[4]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Blood_Pressure_Units==TRUE) %>% 
    select(C_BioSense_ID, Systolic_Diastolic_Blood_Pressure_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing bp units
  output <- invalid_examples[[4]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_BP_Units_Given_BP==TRUE) %>% 
    select(C_BioSense_ID, Missing_BP_Units_Given_BP) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing bp
  output <- invalid_examples[[4]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_BP_Given_BP_Units==TRUE) %>% 
    select(C_BioSense_ID, Missing_BP_Given_BP_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## admit reason combo
  output <- invalid_examples[[5]] %>% 
    filter(C_Biosense_Facility_ID==i & Admit_Reason_Combo==TRUE) %>% 
    select(C_BioSense_ID, Admit_Reason_Combo) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## chief complaint combo
  output <- invalid_examples[[5]] %>% 
    filter(C_Biosense_Facility_ID==i & Chief_Complaint_Combo==TRUE) %>% 
    select(C_BioSense_ID, Chief_Complaint_Combo) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## patient country
  output <- invalid_examples[[6]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Patient_Country==TRUE) %>% 
    select(C_BioSense_ID, Patient_Country) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing death given dd
  output <- invalid_examples[[7]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Death_Given_Discharge_Disposition==TRUE) %>% 
    select(C_BioSense_ID, Missing_Death_Given_Discharge_Disposition) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing death date time given death
  output <- invalid_examples[[7]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Death_Date_Time_Given_Indicator==TRUE) %>% 
    select(C_BioSense_ID, Missing_Death_Date_Time_Given_Indicator) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing death given ddt
  output <- invalid_examples[[7]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Death_Indicator_Given_Date_Time==TRUE) %>% 
    select(C_BioSense_ID, Missing_Death_Indicator_Given_Date_Time) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## diagnosis type
  output <- invalid_examples[[8]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Diagnosis_Type==TRUE) %>% 
    select(C_BioSense_ID, Diagnosis_Type) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## discharge disposition
  output <- invalid_examples[[9]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Discharge_Disposition==TRUE) %>% 
    select(C_BioSense_ID, Discharge_Disposition) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## discharge disposition
  output <- invalid_examples[[10]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Ethnicity_Code==TRUE) %>% 
    select(C_BioSense_ID, Ethnicity_Code) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## facility type
  output <- invalid_examples[[11]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Facility_Type_Code==TRUE) %>% 
    select(C_BioSense_ID, Facility_Type_Code) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## mrn, fpid mismatch
  output <- invalid_examples[[12]] %>% 
    filter(C_Biosense_Facility_ID==i & FPID_MRN_Mismatch==TRUE) %>% 
    select(C_BioSense_ID, FPID_MRN_Mismatch) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## sex
  output <- invalid_examples[[13]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Administrative_Sex==TRUE) %>% 
    select(C_BioSense_ID, Administrative_Sex) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## height units
  output <- invalid_examples[[14]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Height_Units==TRUE) %>% 
    select(C_BioSense_ID, Height_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing height units
  output <- invalid_examples[[14]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Height_Units_Given_Height==TRUE) %>% 
    select(C_BioSense_ID, Missing_Height_Units_Given_Height) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing height
  output <- invalid_examples[[14]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Height_Given_Height_Units==TRUE) %>% 
    select(C_BioSense_ID, Missing_Height_Given_Height_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## patient class
  output <- invalid_examples[[15]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Patient_Class_Code==TRUE) %>% 
    select(C_BioSense_ID, Patient_Class_Code) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## invalid pulse ox units
  output <- invalid_examples[[16]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Pulse_Oximetry_Units==TRUE) %>% 
    select(C_BioSense_ID, Initial_Pulse_Oximetry_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## invalid pulse ox
  output <- invalid_examples[[16]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Pulse_Oximetry==TRUE) %>% 
    select(C_BioSense_ID, Initial_Pulse_Oximetry) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing pulse ox units
  output <- invalid_examples[[16]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Oximetry_Units_Given_Pulse_Ox==TRUE) %>% 
    select(C_BioSense_ID, Missing_Oximetry_Units_Given_Pulse_Ox) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing pulse ox
  output <- invalid_examples[[16]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Pulse_Ox_Given_Oximetry_Units==TRUE) %>% 
    select(C_BioSense_ID, Missing_Pulse_Ox_Given_Oximetry_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## race
  output <- invalid_examples[[17]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Race_Code==TRUE) %>% 
    select(C_BioSense_ID, Race_Code) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## smoking status
  output <- invalid_examples[[18]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Smoking_Status_Code==TRUE) %>% 
    select(C_BioSense_ID, Smoking_Status_Code) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## patient state
  output <- invalid_examples[[19]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Patient_State==TRUE) %>% 
    select(C_BioSense_ID, Patient_State) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## invalid temp units
  output <- invalid_examples[[20]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Temp_Units==TRUE) %>% 
    select(C_BioSense_ID, Initial_Temp_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## temperature out of range
  output <- invalid_examples[[20]] %>% 
    filter(C_Biosense_Facility_ID==i & Temp_OOR==TRUE) %>% 
    select(C_BioSense_ID, Initial_Temp) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing temp
  output <- invalid_examples[[20]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Temp_Given_Units==TRUE) %>% 
    select(C_BioSense_ID, Missing_Temp_Given_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing temp units
  output <- invalid_examples[[20]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Units_Given_Temp==TRUE) %>% 
    select(C_BioSense_ID, Missing_Units_Given_Temp) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## invalid weight units
  output <- invalid_examples[[21]] %>% 
    filter(C_Biosense_Facility_ID==i & Invalid_Weight_Units==TRUE) %>% 
    select(C_BioSense_ID, Weight_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing weight units
  output <- invalid_examples[[21]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Weight_Units_Given_Weight==TRUE) %>% 
    select(C_BioSense_ID, Missing_Weight_Units_Given_Weight) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  ## missing weight
  output <- invalid_examples[[21]] %>% 
    filter(C_Biosense_Facility_ID==i & Missing_Weight_Given_Weight_Units==TRUE) %>% 
    select(C_BioSense_ID, Missing_Weight_Given_Weight_Units) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  # patient zip
  output <- invalid_examples[[22]] %>% 
    filter(C_Biosense_Facility_ID==i & Zip_Invalid==TRUE) %>% 
    select(C_BioSense_ID, Patient_Zip) %>% 
    distinct() %>% 
    full_join(output, by="C_BioSense_ID")
  
  return(
    suppressWarnings( # it'll throw that it is converting it all to character; we do not care
      output %>% # take output
        gather(Field, Invalid_Entry, 2:ncol(.), na.rm=TRUE) # gather it all, get rid of NAs
    )
  )
}
