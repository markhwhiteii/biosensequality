#' Write NSSP BioSense Platform Data Quality Summary Reports
#' 
#' @description 
#' This function calls upon all of the other functions in the package to write a large number of Excel workbooks. First,
#' it generates a state summary workbook that shows percents and counts of nulls and invalids at both the facility and statewide level,
#' as well as message delivery lag (i.e., timeliness reports). Second, it generates a summary workbook for every single facility
#' that includes only information for that facility. Third, it generates an example workbook for every single facility, including 
#' detailed information on records and visits that are null or invalid. These example workbooks make the function longer to run,
#' so by default this function does not generate them (see `nexamples` input below).
#' 
#' @param username Your BioSense username, as a string. This is the same username you may use to log into RStudio or Adminer.
#' @param password Your BioSense password, as a string. This is the same password you may use to log into RStudio or Adminer.
#' @param table The table that you want to retrieve the data from, as a string.
#' @param mft The MFT (master facilities table) from where the facility names will be retrieved, as a string.
#' @param start The start date time that you wish to begin pulling data from, as a string.
#' @param end The end data time that you wish to stop pulling data from, as a string.
#' @param directory The directory where you would like to write the reports to (i.e., "~/Documents/MyReports"), as a string.
#' @param nexamples An integer number of examples you would like for each type of invalid or null field in the examples workbooks for each facility.
#' This defaults to 0, which will not generate these example workbooks.
#' @param offset The number of hours you wish to offset Arrived_Date_Time (which is in UTC). The offset value is how far off your local time zone is from UTC. 
#' For example, the Central Time Zone would set this to 5 or 6, depending on if it is daylight savings or not. This value should be an integer. 
#' This is used for timeliness reports using the `va_lag` function.
#' @import dplyr
#' @import tidyr
#' @import openxlsx
#' @importFrom stringr str_replace_all
#' @export
write_reports <- function(username, password, table, mft, start, end, directory="", nexamples=0, offset) {
  
  ## get data and names
  pull <- pull_data(username, password, table, mft, start, end)
  # save data into data
  data <- pull$data
  # save names into names, getting rid of any duplicate names (take first listed)
  fnames <- pull$names %>% group_by(C_Biosense_Facility_ID) %>% slice(1) %>% ungroup()
  
  
  
  
  ## state-wide summary
  # get facility-level state summary of required nulls
  state_req_nulls <- get_req_nulls(data)
  # get facility-level state summary of optional nulls
  state_opt_nulls <- get_opt_nulls(data)
  # get facility-level state summary of invalids
  state_invalids <- get_all_invalids(data)
  # overall , state-level average
  statewides <- statewide(data, state_req_nulls, state_opt_nulls, state_invalids)
  
  # writing xlsx
  wb <- createWorkbook() # create workbook
  # sheet 1: required nulls
  sheet1 <- addWorksheet(wb, "Required Nulls")
  # putting statewide above the filter
  writeData(wb, sheet1, statewides$statewide_reqnull, 
            startCol=2, startRow=1, colNames=FALSE)
  # writing data table below
  writeDataTable(wb, sheet1,
                 state_req_nulls %>% 
                   right_join(fnames, ., by = "C_Biosense_Facility_ID"),
                 startCol=1, startRow=3, bandedRows=TRUE)
  # formatting widths, freeze panes, and color
  setColWidths(wb, sheet1, 1:ncol(right_join(fnames, state_req_nulls, by = "C_Biosense_Facility_ID")), "auto")
  freezePane(wb, sheet1, firstActiveRow=4, firstActiveCol=4)
  addStyle(wb, sheet1, createStyle(fgFill="#4f81bd", fontColour="#ffffff", textDecoration = "bold"),
           rows=1:3, cols=1:ncol(right_join(fnames, state_req_nulls, by = "C_Biosense_Facility_ID")), gridExpand=TRUE)
  # sheet 2: optional nulls
  sheet2 <- addWorksheet(wb, "Optional Nulls")
  # putting statewide above the filter
  writeData(wb, sheet2, statewides$statewide_optnull, 
            startCol=2, startRow=1, colNames=FALSE)
  # writing data table below
  writeDataTable(wb, sheet2,
                 state_opt_nulls %>% 
                   right_join(fnames, ., by = "C_Biosense_Facility_ID"),
                 startCol=1, startRow=3, bandedRows=TRUE)
  # formatting widths, freeze panes, and color
  setColWidths(wb, sheet2, 1:ncol(right_join(fnames, state_opt_nulls, by = "C_Biosense_Facility_ID")), "auto")
  freezePane(wb, sheet2, firstActiveRow=4, firstActiveCol=4)
  addStyle(wb, sheet2, createStyle(fgFill="#4f81bd", fontColour="#ffffff", textDecoration = "bold"),
           rows=1:3, cols=1:ncol(right_join(fnames, state_opt_nulls, by = "C_Biosense_Facility_ID")), gridExpand=TRUE)
  # sheet 3: invalids
  sheet3 <- addWorksheet(wb, "Invalids")
  # putting statewide above the filter
  writeData(wb, sheet3, statewides$statewide_invalids, 
            startCol=2, startRow=1, colNames=FALSE)
  # writing data table below
  writeDataTable(wb, sheet3,
                 state_invalids %>% 
                   right_join(fnames, ., by = "C_Biosense_Facility_ID"),
                 startCol=1, startRow=3, bandedRows=TRUE)
  # formatting widths, freeze panes, and color
  setColWidths(wb, sheet3, 1:ncol(right_join(fnames, state_invalids, by = "C_Biosense_Facility_ID")), "auto")
  freezePane(wb, sheet3, firstActiveRow=4, firstActiveCol=4)
  addStyle(wb, sheet3, createStyle(fgFill="#4f81bd", fontColour="#ffffff", textDecoration = "bold"),
           rows=1:3, cols=1:ncol(right_join(fnames, state_invalids, by = "C_Biosense_Facility_ID")), gridExpand=TRUE)
  # sheet 4: visit-arrival lag
  sheet4 <- addWorksheet(wb, "Visit-Arrival Lag")
  writeDataTable(wb, sheet4,
                 va_lag(data, fnames, offset),
                 startCol=1, startRow=1, bandedRows=TRUE)
  setColWidths(wb, sheet4, 1:3, "auto")
  freezePane(wb, sheet4, firstActiveRow=2)
  # write workbook
  saveWorkbook(wb, paste0(directory, "/State_Summary.xlsx"), overwrite=TRUE)




  ## facility by facility summary
  for (i in data$C_Biosense_Facility_ID[!duplicated(data$C_Biosense_Facility_ID)]) { # for every unique facility id
    # getting first and last visit date times
    vmin <- min(as.character(filter(data, C_Biosense_Facility_ID==i)$C_Visit_Date_Time))
    vmax <- max(as.character(filter(data, C_Biosense_Facility_ID==i)$C_Visit_Date_Time))
    amin <- min(as.character(filter(data, C_Biosense_Facility_ID==i)$Arrived_Date_Time))
    amax <- max(as.character(filter(data, C_Biosense_Facility_ID==i)$Arrived_Date_Time))
    # get hl7 values
    data("hl7_values", envir=environment())
    hl7_values$Field <- as.character(hl7_values$Field)
    # get name of facility
    fname <- as.character(unlist(unname(c(fnames[which(fnames$C_Biosense_Facility_ID==i),1]))))

    # write to xlsx
    # initialize workbook
    wb <- createWorkbook()
    # sheet 1: facility information
    sheet1 <- addWorksheet(wb, "Facility Information")
    writeDataTable(wb, sheet1,
                   suppressWarnings(data %>% # take data
                     select(c(C_Biosense_Facility_ID, Sending_Facility_ID, Sending_Application, 
                              Treating_Facility_ID, Receiving_Application, Receiving_Facility)) %>% # taking only variables we want
                     filter(C_Biosense_Facility_ID==i) %>% # taking only rows with the same facility ID
                     gather(key=Field, value=Value, convert=TRUE) %>% # suppressed warnings because this will tell you it converted all to characters
                     distinct() %>% # get only distinct entries
                     bind_rows(data.frame(Field="Facility_Name", Value=fname), .) %>% # add name to the top
                     # bind with date ranges and number of records and visits
                     bind_rows(data.frame(Field=c("Patient_Visit_Dates", "Message_Arrival_Dates", 
                                                  "Number of Records", "Number of Visits"),
                                          Value=c(paste("From", vmin, "to", vmax),
                                                  paste("From", amin, "to", amax),
                                                  nrow(filter(data, C_Biosense_Facility_ID==i)), 
                                                  n_groups(group_by(filter(data, C_Biosense_Facility_ID==i), C_BioSense_ID))))) %>% 
                     right_join(hl7_values, ., by="Field")), # get hl7 values
                   firstColumn=TRUE, bandedRows=TRUE)
    setColWidths(wb, sheet1, 1:3, "auto")
    # sheet 2: required nulls
    sheet2 <- addWorksheet(wb, "Required Nulls") # initialize sheet
    # making data for it
    facsheet2data <- statewides$statewide_reqnull %>% # take state average
      filter(Measure=="Percent") %>% # only percent
      select(-Location, -Measure) %>% # select vars only needed
      gather(Field, State_Percent, 1:ncol(.)) %>% # put into long format
      left_join(one_facility_summary(state_req_nulls, i), ., by="Field") # join with one facility summary
    writeDataTable(wb, sheet2, facsheet2data, firstColumn=TRUE, bandedRows=TRUE) # write to table
    setColWidths(wb, sheet2, 1:ncol(facsheet2data), "auto") # format sheet
    freezePane(wb, sheet2, firstActiveRow=2) # format sheet
    # sheet 3: optional nulls
    sheet3 <- addWorksheet(wb, "Optional Nulls") # initialize sheet
    # making data for it
    facsheet3data <- statewides$statewide_optnull %>% # take state average
      filter(Measure=="Percent") %>% # only percent
      select(-Location, -Measure) %>% # select vars only needed
      gather(Field, State_Percent, 1:ncol(.)) %>% # put into long format
      left_join(one_facility_summary(state_opt_nulls, i), ., by="Field") # join with one facility summary
    writeDataTable(wb, sheet3, facsheet3data, firstColumn=TRUE, bandedRows=TRUE) # write to table
    setColWidths(wb, sheet3, 1:ncol(facsheet3data), "auto") # format sheet
    freezePane(wb, sheet3, firstActiveRow=2) # format sheet
    # sheet 4: invalids
    sheet4 <- addWorksheet(wb, "Invalids") # initialize sheet
    # making data for it
    facsheet4data <- statewides$statewide_invalids %>% # take state average
      filter(Measure=="Percent") %>% # only percent
      select(-Location, -Measure) %>% # select vars only needed
      gather(Field, State_Percent, 1:ncol(.)) %>% # put into long format
      left_join(one_facility_summary(state_invalids, i), ., by="Field") # join with one facility summary
    writeDataTable(wb, sheet4, facsheet4data, firstColumn=TRUE, bandedRows=TRUE) # write to table
    setColWidths(wb, sheet4, 1:ncol(facsheet4data), "auto") # format sheet
    freezePane(wb, sheet4, firstActiveRow=2) # format sheet
    # write to file
    filename <- str_replace_all(fname, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
      str_replace_all("[\\s]", "_") # replace spaces with underscores
    saveWorkbook(wb, paste0(directory, "/", filename, "_Summary.xlsx"), overwrite=TRUE)
  }
  
  
  
  
  ## facility by facility examples
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
    
    for (i in data$C_Biosense_Facility_ID[!duplicated(data$C_Biosense_Facility_ID)]) { # for every unique facility id
      inv_examples <- examples_invalids(i, invalid_examples) # get examples of invalids from this facility
      null_examples <- examples_nulls(i, data) # get examples of nulls from this faciltiy
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
      fname <- as.character(unlist(unname(c(fnames[which(fnames$C_Biosense_Facility_ID==i),1])))) # get facility name
      filename <- str_replace_all(fname, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
        str_replace_all("[\\s]", "_") # replace spaces with underscores
      saveWorkbook(wb, paste0(directory, "/", filename, "_Examples.xlsx"), overwrite=TRUE)
    }
  }
}
