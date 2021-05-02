#' get_time function
#'
#' This function allows you to extract the time elements from a date-time.
#' @param datetime Date-time column/variable. Defaults to lubridate::now().
#' @keywords date time
#' @export
#' @examples
#' get_time()

get_time <- function(datetime = lubridate::now()){

  datetime_list <- datetime %>% stringr::str_split(" ")

  if (base::length(datetime_list[[1]]) == 2){
    time <- datetime_list %>%
      purrr::map_chr(2) %>%
      hms::as_hms()
  } else {
    time <- hms::as_hms("00:00:00")
  }
  return(time)
}
