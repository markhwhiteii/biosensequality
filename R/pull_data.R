#' Pulling Data
#'
#' This will pull down data from NSSP's BioSense Platform. It assumes you have a valid connection to BioSense_Platform log-in.
#' 
#' @param username Your BioSense username, as a string. This is the same username you may use to log into RStudio or Adminer.
#' @param password Your BioSense password, as a string. This is the same password you may use to log into RStudio or Adminer.
#' @param table The table that you want to retrieve the data from, as a string.
#' @param mft The MFT (master facilities table) from where the facility names will be retrieved, as a string.
#' @param start The start date time that you wish to begin pulling data from, as a string.
#' @param end The end data time that you wish to stop pulling data from, as a string.
#' @return A list of two data frames: first, raw data of all columns from the `table` that was called; second, just the Facility_Name
#' and C_Biosense_Facility_ID from the MFT table.
#' @import RODBC
#' @export
pull_data <- function(username, password, table, mft, start, end) {
  
  channel <- odbcConnect("BioSense_Platform", paste0("BIOSENSE\\", username), password) # open channel
  data <- sqlQuery(
    channel,
    paste0("SELECT * FROM ", table, " WHERE C_Visit_Date_Time >= '", start, "' AND C_Visit_Date_Time <= '", end, "'") # create sql query
  )
  
  names <- sqlQuery(channel, paste0("SELECT Facility_Name, C_Biosense_Facility_ID FROM ", mft)) # get mft from channel
    
  odbcCloseAll() # close connection
  
  return(list(data=data, names=names))
}
