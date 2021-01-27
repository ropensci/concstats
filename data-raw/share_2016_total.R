## Total market share of each cooperative in the credit market in 2016

library(tidyverse)

coops_2016 <- read_csv("data-raw/sample_2016.csv")

coops_2016_total <- coops_2016[, c(1,15)]

share_2016_total <- coops_2016_total %>%
  mutate(share = total/sum(total)) %>%
  group_by(coop_id)

write.csv(share_2016_total, "data-raw/sample_2016.csv")

usethis::use_data(share_2016_total, overwrite = TRUE)
