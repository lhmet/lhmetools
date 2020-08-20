## code to prepare `dates_miss` data set
dates_miss <- data.frame(
  c(1, 1, 1, 2, 3, 3, 3, 4, 4, 4),
  c(rep(30, 2), rep(25, 5), rep(20, 3)),
  as.Date(c(
    "2017-01-01", "2017-02-01", "2017-04-01",
    "2017-02-01", "2017-01-01", "2017-02-01",
    "2017-03-01", "2017-01-01", "2017-02-01",
    "2017-04-01"
  )),
  c(runif(10))
)
colnames(dates_miss) <- c("id", "g", "date", "value")
dates_miss <- data.table::as.data.table(dates_miss)
data.table::setorderv(dates_miss, cols = c("id", "date"))
dates_miss <- tibble::as_tibble(dates_miss)

usethis::use_data(dates_miss, overwrite = TRUE)
