#' Write NSSP BioSense Platform Data Quality Summary Reports for One Facility
#'
#' @description
#' This function is a lightweight version of the `write_reports` function. It will generate summary and example workbooks, but only for
#' one specified facility. The first summary workbook shows percents and counts of nulls and invalids, while the examples workbook
#' generates de tailed information on records and visits that are null or invalid.
#'
#' @param username Your BioSense username, as a string. This is the same username you may use to log into RStudio or Adminer.
#' @param password Your BioSense password, as a string. This is the same password you may use to log into RStudio or Adminer.
#' @param table The table that you want to retrieve the data from, as a string.
#' @param mft The MFT (master facilities table) from where the facility name will be retrieved, as a string.
#' @param start The start date time that you wish to begin pulling data from, as a string.
#' @param end The end data time that you wish to stop pulling data from, as a string.
#' @param facility The C_Biosense_Facility_ID for the facility that you wish to generate and write the report for.
#' @param directory The directory where you would like to write the reports to (i.e., "~/Documents/MyReports"), as a string.
#' @param nexamples An integer number of examples you would like for each type of invalid or null field in the examples workbooks for each facility.
#' This defaults to 0, which will not generate these example workbooks.
#' @import dplyr
#' @import tidyr
#' @import openxlsx
#' @import RODBC
#' @importFrom stringr str_replace_all
#' @export
write_facility <- function(username, password, table, mft, start, end, facility, directory="", nexamples=0) {

  # pull data
  channel <- odbcConnect("BioSense_Platform", paste0("BIOSENSE\\", username), password) # open channel
  data <- sqlQuery(
    channel,
    paste0("SELECT * FROM ", table, " WHERE C_Visit_Date_Time >= '", start, "' AND C_Visit_Date_Time <= '", end, "' AND C_Biosense_Facility_ID = ", facility) # create sql query
  )
  if (nrow(data) == 0) stop("The query yielded no data.")
  name <- as.character(unlist(unname(c(sqlQuery(channel, paste0("SELECT Facility_Name FROM ", mft, " WHERE C_Biosense_Facility_ID = ", facility)))))) # get name from mft
  odbcCloseAll() # close connection

  # get hl7 values
  data("hl7_values", envir=environment())
  hl7_values$Field <- as.character(hl7_values$Field)

  # get facility-level state summary of required nulls
  req_nulls <- get_req_nulls(data) %>%
    select(-c(C_Biosense_Facility_ID)) %>%
    gather(Field, Value, 2:ncol(.)) %>%
    spread(Measure, Value) %>%
    right_join(hl7_values, ., by = "Field")
  # get facility-level state summary of optional nulls
  opt_nulls <- get_opt_nulls(data) %>%
    select(-c(C_Biosense_Facility_ID)) %>%
    gather(Field, Value, 2:ncol(.)) %>%
    spread(Measure, Value) %>%
    right_join(hl7_values, ., by = "Field")
  # get facility-level state summary of invalids
  invalids <- get_all_invalids(data) %>%
    select(-c(C_Biosense_Facility_ID)) %>%
    gather(Field, Value, 2:ncol(.)) %>%
    spread(Measure, Value) %>%
    right_join(hl7_values, ., by = "Field")

  # getting first and last visit date times
  vmin <- min(as.character(data$C_Visit_Date_Time))
  vmax <- max(as.character(data$C_Visit_Date_Time))
  amin <- min(as.character(data$Arrived_Date_Time))
  amax <- max(as.character(data$Arrived_Date_Time))

  # write to xlsx
  # initialize workbook
  wb <- createWorkbook()
  # sheet 1: facility information
  sheet1 <- addWorksheet(wb, "Facility Information")
  writeDataTable(wb, sheet1,
                 suppressWarnings(data %>% # take data
                                    select(c(C_Biosense_Facility_ID, Sending_Facility_ID, Sending_Application,
                                             Treating_Facility_ID, Receiving_Application, Receiving_Facility)) %>% # taking only variables we want
                                    gather(key=Field, value=Value, convert=TRUE) %>% # suppressed warnings because this will tell you it converted all to characters
                                    distinct() %>% # get only distinct entries
                                    bind_rows(data.frame(Field="Facility_Name", Value=name), .) %>% # add name to the top
                                    # bind with date ranges and number of records and visits
                                    bind_rows(data.frame(Field=c("Patient_Visit_Dates", "Message_Arrival_Dates",
                                                                 "Number of Records", "Number of Visits"),
                                                         Value=c(paste("From", vmin, "to", vmax),
                                                                 paste("From", amin, "to", amax),
                                                                 nrow(data),
                                                                 n_groups(group_by(data, C_BioSense_ID))))) %>%
                                    right_join(hl7_values, ., by="Field")), # get hl7 values
                 firstColumn=TRUE, bandedRows=TRUE)
  setColWidths(wb, sheet1, 1:3, "auto")
  # sheet 2: required nulls
  sheet2 <- addWorksheet(wb, "Required Nulls") # initialize sheet
  writeDataTable(wb, sheet2, req_nulls, firstColumn=TRUE, bandedRows=TRUE) # write to table
  setColWidths(wb, sheet2, 1:ncol(req_nulls), "auto") # format sheet
  freezePane(wb, sheet2, firstActiveRow=2) # format sheet
  # sheet 3: optional nulls
  sheet3 <- addWorksheet(wb, "Optional Nulls") # initialize sheet
  writeDataTable(wb, sheet3, opt_nulls, firstColumn=TRUE, bandedRows=TRUE) # write to table
  setColWidths(wb, sheet3, 1:ncol(opt_nulls), "auto") # format sheet
  freezePane(wb, sheet3, firstActiveRow=2) # format sheet
  # sheet 4: invalids
  sheet4 <- addWorksheet(wb, "Invalids") # initialize sheet
  writeDataTable(wb, sheet4, invalids, firstColumn=TRUE, bandedRows=TRUE) # write to table
  setColWidths(wb, sheet4, 1:ncol(invalids), "auto") # format sheet
  freezePane(wb, sheet4, firstActiveRow=2) # format sheet
  # write to file
  filename <- str_replace_all(name, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
    str_replace_all("[\\s]", "_") # replace spaces with underscores
  saveWorkbook(wb, paste0(directory, "/", filename, "_Summary.xlsx"), overwrite=TRUE)

  if (nexamples > 0) {
    # get list of invalid examples data frames
    # DO NOT CHANGE THE ORDER OF THIS LIST
    invalid_examples <- list(admit_source_invalid(data)[[1]], # 1
                             age_invalid(data)[[1]], # 2
                             any_e_invalid(data)[[1]], # 3
                             blood_pressure_invalid(data)[[1]], # 4
                             cc_ar_invalid(data)[[1]], # 5
                             country_invalid(data)[[1]], # 6
                             death_invalid(data)[[1]], # 7
                             diagnosis_type_invalid(data)[[1]], # 8
                             discharge_disposition_invalid(data)[[1]], # 9
                             ethnicity_invalid(data)[[1]], # 10
                             facility_type_invalid(data)[[1]], # 11
                             fpid_mrn_invalid(data)[[1]], # 12
                             gender_invalid(data)[[1]], # 13
                             height_invalid(data)[[1]], # 14
                             patient_class_invalid(data)[[1]], # 15
                             pulseox_invalid(data)[[1]], # 16
                             race_invalid(data)[[1]], # 17
                             smoking_status_invalid(data)[[1]], # 18
                             state_invalid(data)[[1]], # 19
                             temperature_invalid(data)[[1]], # 20
                             weight_invalid(data)[[1]], # 21
                             zip_invalid(data)[[1]]) # 22

      inv_examples <- examples_invalids(facility, invalid_examples) # get examples of invalids from this facility
      null_examples <- examples_nulls(facility, data) # get examples of nulls from this faciltiy
      # join with other relevant fields
      inv_examples <- inv_examples %>% # take examples
        left_join(., select(data, c(C_BioSense_ID, C_Visit_Date, C_Visit_Date_Time, First_Patient_ID,
                                    C_Unique_Patient_ID, Medical_Record_Number, Visit_ID, Admit_Date_Time,
                                    Recorded_Date_Time, Message_Date_Time, Create_Raw_Date_Time,
                                    Message_Type, Trigger_Event, Message_Structure, Message_Control_ID)),
                  by="C_BioSense_ID") %>% # join with all these fields, for every record of that visit
        rename(Invalid_Field=Field) %>% # make it clearer that that field is the one that is invalid
        group_by(Invalid_Field) %>% # group by type of field
        slice(1:nexamples) # get nexamples
      # do the same for nulls
      null_examples <- null_examples %>%
        left_join(., select(data, c(C_BioSense_ID, C_Visit_Date, C_Visit_Date_Time, First_Patient_ID,
                                    C_Unique_Patient_ID, Medical_Record_Number, Visit_ID, Admit_Date_Time,
                                    Recorded_Date_Time, Message_Date_Time, Create_Raw_Date_Time,
                                    Message_Type, Trigger_Event, Message_Structure, Message_Control_ID)),
                  by="C_BioSense_ID") %>% # join with all these fields, for every record of that visit
        group_by(Null_Field) %>% # group by type of field
        slice(1:nexamples) # get nexamples

      # write to excel workbook
      wb <- createWorkbook()
      # sheet 1: invalids
      sheet1 <- addWorksheet(wb, "Invalids")
      writeDataTable(wb, sheet1, inv_examples, firstColumn=TRUE, bandedRows=TRUE)
      setColWidths(wb, sheet1, 1:ncol(inv_examples), "auto")
      freezePane(wb, sheet1, firstActiveRow=2, firstActiveCol=4)
      # sheet2: nulls
      sheet2 <- addWorksheet(wb, "Nulls")
      writeDataTable(wb, sheet2, null_examples, firstColumn=TRUE, bandedRows=TRUE)
      setColWidths(wb, sheet2, 1:ncol(null_examples), "auto")
      freezePane(wb, sheet2, firstActiveRow=2, firstActiveCol=3)
      # write sheet
      filename <- str_replace_all(name, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
        str_replace_all("[\\s]", "_") # replace spaces with underscores
      saveWorkbook(wb, paste0(directory, "/", filename, "_Examples.xlsx"), overwrite=TRUE)
  }
}
