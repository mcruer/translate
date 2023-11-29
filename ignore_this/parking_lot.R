library(tidyverse)


rosetta <- tibble(french = c("le", "une"), english = c("the", "a"), category = "articles")
tibble(words = c("a", "the", "une", "le", "other")) %>%
  translate_from_french(words, rosetta, "propositions")
#' #' Translate Column Values Based on a Key
#' #'
#' #' This function takes a data frame, a column name, and a key data frame to translate the values of the specified column.
#' #' It also allows for conditional translation based on a provided condition.
#' #'
#' #' @param df A data frame containing the column to be translated.
#' #' @param column The name of the column in df to be translated.
#' #' @param key A data frame that serves as the translation key.
#' #' @param condition An optional purrr-style function for conditional translation.
#' #' @param comparison An optional column for comparison in the condition.
#' #' @param from The language to translate from ("english" or other).
#' #'
#' #' @return A data frame with the translated column.
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' df <- data.frame(column1 = c("apple", "banana"), column2 = c("fruit", "fruit"))
#' #' key <- data.frame(english = c("fruit"), french = c("fruité"))
#' #' translate(df, "column1", key)
#' #' }
#' translate <- function(df, column, key, condition = NULL, comparison= NULL, from = "english") {
#'   # Load required packages
#'   requireNamespace("dplyr", quietly = TRUE)
#'   requireNamespace("rlang", quietly = TRUE)
#'   requireNamespace("janitor", quietly = TRUE)
#'
#'   # Determine the language to translate to
#'   if(from == "english") {
#'     to <- "french"
#'   } else {
#'     to <- "english"
#'   }
#'
#'   # Set default condition if NULL
#'   if(is.null(condition)) {condition <- ~TRUE}
#'   condition <- rlang::as_function(condition)
#'
#'   # Prepare the key data frame
#'   column.name <- rlang::ensym(column) %>% as.character()
#'   key <- key %>%
#'     janitor::clean_names() %>%
#'     dplyr::group_by(dplyr::across(dplyr::all_of(from))) %>%
#'     dplyr::slice(1) %>%
#'     dplyr::ungroup() %>%
#'     dplyr::rename({{column.name}} := {{from}})
#'
#'   # Join the key and relocate the translated column
#'   df <- df %>%
#'     dplyr::left_join(key, by = column.name) %>%
#'     dplyr::relocate(dplyr::all_of(to))
#'
#'   # Apply the condition and perform the translation
#'   df %>%
#'     dplyr::mutate(
#'       temp.condition = condition({{comparison}}),
#'       {{ column }} := dplyr::if_else(temp.condition, !!rlang::sym(to), {{column}})
#'     ) %>%
#'     dplyr::select(-temp.condition)
#' }
