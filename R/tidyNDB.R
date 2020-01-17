#' 性年齢別の処方薬数量データをtidy化
#'
#' @param xlsx excel file to tidy
#' @export
tidy_syohouyaku_sex_age <- function(xlsx) {
  raw <-
    readxl::read_xlsx(xlsx,
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
      readxl::read_xlsx(
        xlsx,
        skip = 4,
        col_names = FALSE,
        col_type = col_type_list
      )
    ) %>%
    tidyr::fill(1, 2)
  data_male <- data %>%
    dplyr::select(1:30) %>%
    purrr::set_names(c(col_name_1, col_name_2)) %>%
    dplyr::mutate(sex = 0)
  data_female <- data %>%
    dplyr::select(1:9, 31:51) %>%
    purrr::set_names(c(col_name_1, col_name_2)) %>%
    dplyr::mutate(sex = 1)
  data_tidy <- dplyr::bind_rows(data_male, data_female) %>%
    tidyr::pivot_longer(10:30, names_to = "age", values_to = "count") %>%
    dplyr::mutate(
      generic = as.factor(generic),
      sex = as.factor(sex),
      age = as.factor(age)
    )
  return(data_tidy)
}
