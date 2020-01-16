#' Tidying NDB excel file
#'
#' @param xlsx excel file to tidy
#'
#' @import readxl
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @import forcats
#' @export
tidyNDB_xlsx <- function(xlsx) {
  raw <-
    read_xlsx(xlsx,
              na = c("", "-"))
  col_name_1 <-
    c(
      "typecode",
      "typename",
      "drugcode",
      "drugname",
      "tanni",
      "yakkakijun",
      "price",
      "generic",
      "total"
    )
  col_name_2 <- raw[3, 10:30]
  col_type_list <-
    c(
      "numeric",
      "text",
      "numeric",
      rep("text", 3),
      "numeric",
      "text",
      rep("numeric", 43)
    )
  data <-
    suppressWarnings(
      read_xlsx(
        xlsx,
        skip = 4,
        col_names = FALSE,
        col_type = col_type_list
      )
    ) %>%
    fill(1, 2)
  data_male <- data %>%
    select(1:30) %>%
    set_names(c(col_name_1, col_name_2)) %>%
    mutate(sex = 0)
  data_female <- data %>%
    select(1:9, 31:51) %>%
    set_names(c(col_name_1, col_name_2)) %>%
    mutate(sex = 1)
  data_tidy <- bind_rows(data_male, data_female) %>%
    pivot_longer(10:30, names_to = "age", values_to = "count") %>%
    mutate(
      generic = as_factor(generic),
      sex = as_factor(sex),
      age = as_factor(age)
    )
  return(data_tidy)
}
