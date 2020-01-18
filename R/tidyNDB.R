#' @title ダウンロード
#' @description NDBオープンデータのすべてのxlsxファイルをダウンロードする
#' @note 参考: https://nigimitama.hatenablog.jp/entry/2018/12/18/173703
#' @param foldername destination folder path with /
#' @export
download_xlsx <- function(foldername) {
  html_list = c(
    "https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000139390.html",
    "https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000177221.html",
    "https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000177221_00002.html",
    "https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000177221_00003.html"
  )
  for (i in 1:length(html_list)) {
    html_data <- xml2::read_html(html)
    title_list <-
      html_data %>% rvest::html_nodes(".li-notesB") %>% rvest::html_nodes("a") %>% rvest::html_text()
    url_list <-
      html_data %>% rvest::html_nodes(".li-notesB") %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
    url_df <- data.frame(title = title_list, url = url_list)
    base_url = "https://www.mhlw.go.jp/"
    download_folder = stringr::str_c(foldername, "第", i, "回/")
    dir.create(download_folder)
    file_name = stringr::str_c(url_df$title)
    url = stringr::str_c(base_url, "/", url_df[["url"]])
    for (i in 1:length(url)) {
      if (stringr::str_detect(url[i], "xlsx")) {
        utils::download.file(
          url = url[i],
          destfile = stringr::str_c(download_folder, file_name[i], ".xlsx"),
          mode = "wb"
        )
        Sys.sleep(0.5)
      }
    }
  }
}


#' @title 処方薬(性年齢別)
#' @description 性年齢別の処方薬数量データをtidy化
#' @param xlsx excel file to tidy
#' @export
tidy_syohouyaku_sex_age <- function(xlsx) {
  raw <-
    readxl::read_xlsx(xlsx,
                      na = c("", "-"))
  if (stringr::str_detect(xlsx, "h27")) {
    col_name_1 <-
      c(
        "typecode",
        "typename",
        "drugcode",
        "drugname",
        "yakkakijun",
        "price",
        "generic",
        "total"
      )
    col_name_2 <- raw[3, 9:27]
    col_type_list <-
      c("numeric",
        "text",
        "numeric",
        rep("text", 2),
        rep("numeric", 41))
    data <-
      suppressWarnings(
        readxl::read_xlsx(
          xlsx,
          skip = 4,
          col_names = FALSE,
          col_type = col_type_list,
          na = c("", "-")
        )
      ) %>%
      tidyr::fill(1, 2)
    data_male <- data %>%
      dplyr::select(1:27) %>%
      purrr::set_names(c(col_name_1, col_name_2)) %>%
      dplyr::mutate(sex = 0)
    data_female <- data %>%
      dplyr::select(1:8, 28:46) %>%
      purrr::set_names(c(col_name_1, col_name_2)) %>%
      dplyr::mutate(sex = 1)
    data_tidy <- dplyr::bind_rows(data_male, data_female) %>%
      tidyr::pivot_longer(9:27, names_to = "age", values_to = "count") %>%
      dplyr::mutate(
        generic = as.factor(generic),
        sex = as.factor(sex),
        age = as.factor(age)
      )
  } else {
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
      c("numeric",
        "text",
        "numeric",
        rep("text", 3),
        rep("numeric", 45))
    data <-
      suppressWarnings(
        readxl::read_xlsx(
          xlsx,
          skip = 4,
          col_names = FALSE,
          col_type = col_type_list,
          na = c("", "-")
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
  }
  return(data_tidy)
}

#' @title 処方薬(都道府県別)
#' @description 都道府県別の処方薬数量データをtidy化
#' @param xlsx excel file to tidy
#' @export
tidy_syohouyaku_prefecture <- function(xlsx) {
  raw <-
    readxl::read_xlsx(xlsx,
                      na = c("", "-"))
  if (stringr::str_detect(xlsx, "h27")) {
    col_name_1 <-
      c(
        "typecode",
        "typename",
        "drugcode",
        "drugname",
        "yakkakijun",
        "price",
        "generic",
        "total"
      )
    col_name_2 <- raw[3, 9:55]
    col_type_list <-
      c("numeric",
        "text",
        "numeric",
        rep("text", 2),
        rep("numeric", 50))
    data <-
      suppressWarnings(
        readxl::read_xlsx(
          xlsx,
          skip = 4,
          col_names = FALSE,
          col_type = col_type_list,
          na = c("", "-")
        )
      ) %>%
      tidyr::fill(1, 2) %>%
      purrr::set_names(c(col_name_1, col_name_2)) %>%
      tidyr::pivot_longer(9:55, names_to = "prefecture", values_to = "count") %>%
      dplyr::mutate(generic = as.factor(generic),
                    prefecture = as.factor(prefecture))
  } else {
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
    col_name_2 <- raw[3, 10:56]
    col_type_list <-
      c("numeric",
        "text",
        "numeric",
        rep("text", 3),
        rep("numeric", 50))
    data <-
      suppressWarnings(
        readxl::read_xlsx(
          xlsx,
          skip = 4,
          col_names = FALSE,
          col_type = col_type_list,
          na = c("", "-")
        )
      ) %>%
      tidyr::fill(1, 2) %>%
      purrr::set_names(c(col_name_1, col_name_2)) %>%
      tidyr::pivot_longer(10:56, names_to = "prefecture", values_to = "count") %>%
      dplyr::mutate(generic = as.factor(generic),
                    prefecture = as.factor(prefecture))
  }
  return(data)
}
