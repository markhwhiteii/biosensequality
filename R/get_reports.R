#' @import dplyr
#' @import tidyr
#' @import openxlsx
#' @export
get_reports <- function(username, password, table, start, end, directory) {
  
  # get data
  data <- pull_data(username, password, table, start, end)
  
  ## state-wide summary
  # get state summary of required nulls
  state_req_nulls <- get_req_nulls(data)
  
  # get state summary of optional nulls
  state_opt_nulls <- get_opt_nulls(data)
  
  # get state summary of invalids
  state_invalids <- invalids_state_summary(data)
  
  # overall state average
  
  
  # writing xlsx
  
  
  ## facility by facility summary
  
  
  ## facility by facility examples
  
}
