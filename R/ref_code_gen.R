#' @title Reference Code Generator
#' @description
#' A unique code generator for invoices, and multi-application business.
#'
#'
#' @param ref_code Unique reference code identifier
#' @param device_code Optional device code for multi-application
#' @importFrom stringi stri_sub
#' @importFrom stringi stri_rand_strings
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate hour
#' @import lubridate
#' @import stringi
#' @return a string
#' @export
#'
#' @examples
#' reference_code()
#' reference_code("SH")
#' reference_code("SH", 1)
reference_code <- function(ref_code = NULL, device_code = NULL) {

  date <- Sys.time()

  tryCatch(
    {
      ref_code <- toupper(ref_code)
      year1 <- stringi::stri_sub(as.character(lubridate::year(date)), -2, -2)
      year2 <- LETTERS[as.integer(stringi::stri_sub(as.character(lubridate::year(date)), -1, -1))]
      month <- LETTERS[lubridate::month(date)]
      day1 <- ifelse(
        lubridate::day(date) <= 9,
        lubridate::day(date),
        LETTERS[lubridate::day(date) - 9]
      )
      hour <- LETTERS[as.integer(lubridate::hour(date) - 1)]
      device <- ifelse(is.null(device_code), 1, device_code)
      random <- toupper(stringi::stri_rand_strings(n = 1, length =  4, pattern = "[A-Za-z0-9]"))

      code <- paste0(ref_code, year1, year2, month, day1, hour, device, random)

      return(code)
    }, error = function(e) {
      print(e$message)
    }
  )
}
