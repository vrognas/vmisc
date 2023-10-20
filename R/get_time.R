#' get_time function
#'
#' This function allows you to extract the time elements from a date-time.
#' @param datetime Date-time column/variable. Defaults to lubridate::now().
#' @keywords date time
#' @export
#' @examples
#' get_time()

get_time <- function(datetime = lubridate::now()) {

  # Extract hour, minute, and second using lubridate
  hour_val   <- lubridate::hour(datetime)
  minute_val <- lubridate::minute(datetime)
  second_val <- lubridate::second(datetime)

  # Convert to hms format
  time_hms <- hms::hms(hours = hour_val, minutes = minute_val, seconds = second_val)

  return(time)
}
