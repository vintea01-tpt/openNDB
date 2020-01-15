library(tidyverse)
library(openxlsx)

tidyNDB_xlsx <- function(xlsx){
  data <- openxlsx::read.xlsx(xlsx)[-c(1:2),] %>%
    tidyr::fill(1, 2)
  data_male <- select(data, 1:30) %>%
  rename(!!!setNames(names(select(data, 1:30)), c(raw[1,1:9], raw[2,10:30]))) %>%
  mutate(sex=0)
  data_female <- select(data, 1:9, 31:51) %>%
    rename(!!!setNames(names(select(data, 1:9, 31:51)), c(raw[1,1:9], raw[2,10:30]))) %>%
    mutate(sex=1)
  data_tidy <- bind_rows(data_male, data_female) %>%
    pivot_longer(10:30, names_to = "age", values_to = "price")
  return(data_tidy)
}
