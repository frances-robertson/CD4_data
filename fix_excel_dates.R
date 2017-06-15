fix_excel_dates <- function(date) {
  require(lubridate)
  return(ymd(date) + years(4) + days(1))
}
