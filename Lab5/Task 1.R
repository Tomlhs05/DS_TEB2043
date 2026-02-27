check_leap_year <- function(year) {
  if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
    cat(year, "is a leap year.\n")
  } else {
    cat(year, "is a not leap year.\n")
  }
}

year <- as.integer(readline(prompt = "Input year: "))
check_leap_year(year)