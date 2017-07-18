#' Extract Summary for One Facility
#' 
#' This function takes a summary from `get_req_nulls`, `get_opt_nulls`, and `get_all_invalids` and returns the information for only one facility.
#' 
#' @param getsummary A summary from `get_req_nulls`, `get_opt_nulls`, or `get_all_invalids`.
#' @param i A facility ID, as a character string.
#' @return The information for only the given facility from the given table, transposed and formatted for `write_reports` to place it in a 
#' facility summary Excel workbook.
#' @import dplyr
#' @import tidyr
#' @export
one_facility_summary <- function(getsummary, i) {
  
  # get hl7 values
  data("hl7_values", envir=environment())
  hl7_values$Field <- as.character(hl7_values$Field)
  
  # generate, return output
  return(
    getsummary %>% # take data
      gather(Field, Value, 3:ncol(.)) %>% # gather by field
      filter(C_Biosense_Facility_ID==i) %>% # get only facility
      select(-C_Biosense_Facility_ID) %>% # dump the facility id
      spread(Measure, Value) %>% # spread out by measure
      right_join(hl7_values, ., by="Field") # join with hl7 values
  )
}
