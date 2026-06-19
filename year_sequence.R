year_sequence <- function(years) {
  years <- sort(unique(years))
  
  c(
    seq(min(years), max(years), by = 5),
    max(years)
  ) |> unique()
}
