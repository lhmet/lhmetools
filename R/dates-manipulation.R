#' Extract dates from file names
#'
#' @param x charcter
#' @param quietly logical. If TRUE, function evaluates without displaying
#' customary messages.
#'
#' @return a vector of class Date
#' @export
#'
#' @examples
#' dates_from_files("MERGE_CPTEC_20201201.grib2")
#' dates_from_files("brick-ETo-25km-19800101-20170731.nc")
#'
#' @family date manipulation
dates_from_files <- function(x, quietly = TRUE) {
  x %>%
    fs::path_dir() %>%
    stringr::str_extract_all("[0-9]{2,}") %>%
    # gsub("[^0-9.-]+", "", .) %>%
    unlist() %>%
    as.integer() %>%
    lubridate::ymd(., quiet = quietly) %>%
    sort()
}


#' Complete a data frame with missing combination of variables (date and group)
#'
#' @param x data.frame with a column date
#' @param group character vector of group variables
#' @param time_step character scalar (e.g., "hours", "days", "months")
#'
#' @return tibble with a regular and constant time step
#' @export
#' @family date manipulation
#' @examples
#' if(TRUE){
#'  dates_comp <- complete_dates(
#'    x = dates_miss,
#'    group = c("id", "g"),
#'    time_step = "months"
#'  )
#' dates_comp
#' }
complete_dates <- function(x, group = "id", time_step = "days") {

  # x = dates_miss; group = c("id", "g"); time_step = "months"
  checkmate::assert_choice("date", names(x))
  checkmate::assert_subset(group, names(x))

  # TEST DATA
  # x <- import_qnat(complete = FALSE); group = "id"; time_step = "days"
  xDT <- data.table::data.table(x, key = c("date", group))
  rm(x)
  if (anyDuplicated(xDT) > 0) {
    message(
      " There are duplicated dates:",
      " keeping the first data record."
    )
  }

  xDT <- unique(xDT)

  # number of distinct values in the group variable
  #groups_u <- unique(xDT[, ..group])
  groups_u <- unique(xDT[, group, with = FALSE])
  data.table::setkeyv(groups_u[, k := 1], c(data.table::key(groups_u), "k"))

  # time span
  time_span <- range(xDT[["date"]])
  # dates with constant and regular time step
  all_dates <- data.table::data.table(
    date = seq(
      from = time_span[1],
      to = time_span[2],
      by = time_step
    ),
    key = "date"
  )
  data.table::setkeyv(all_dates[, k := 1], c(key(all_dates), "k"))

  xDT_c <- data.table::merge.data.table(
    all_dates,
    groups_u,
    by = "k",
    allow.cartesian = TRUE
  )[, k := NULL]

  xDT_c <- data.table::merge.data.table(
    xDT_c,
    xDT,
    all = TRUE,
    by = names(xDT_c)
  )

  data.table::setorderv(
    x = xDT_c,
    cols = c(group, "date")
  )

  return(tibble::as_tibble(xDT_c))
}
