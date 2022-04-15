## code to prepare `creditcoops` dataset goes here
creditcoops <- readr::read_csv("data-raw/creditcoops_USD_paired.csv") %>%
  dplyr::mutate(paired = rep(1:(n() / 2), each = 2)) %>%
  dplyr::mutate(total_loans_log = log(total_loans)) %>%
  dplyr::mutate(year = factor(year))
usethis::use_data(creditcoops, overwrite = TRUE)
