utils::globalVariables("category")

#' Translate elements in a character vector
#'
#' This function takes a character vector, a dataset that acts as a translation
#' dictionary, and optional arguments specifying the languages to translate from
#' and to. It returns a modified character vector with elements translated where possible.
#'
#' @param words A character vector containing the words to be translated.
#' @param rosetta_stone A tibble that serves as a translation dictionary, with columns
#'                      named after the languages it supports and a 'category' column.
#' @param from The name of the column in `rosetta_stone` representing the source language.
#' @param to The name of the column in `rosetta_stone` representing the target language.
#' @param category_selection Optional category selection to filter rows in `rosetta_stone`.
#'
#' @details
#' The function filters `rosetta_stone` based on `category_selection`, then uses it to
#' create a named vector. Elements in `words` are replaced by their corresponding values
#' in the named vector, if they exist.
#'
#' @examples
#' \dontrun{
#' rosetta <- tibble(french = c("le", "une"), english = c("the", "a"), category = "articles")
#' translate_vec(c("le", "a", "une", "other"),
#' rosetta,
#' from = "french",
#' to = "english",
#' category_selection = "articles")
#' }
#'
#' @return A character vector with translated elements.
#' @importFrom dplyr select pull
#' @importFrom gplyr filter_in
#' @importFrom magrittr %>%
#' @export
translate_vec <-
  function(words,
           rosetta_stone,
           from = "french",
           to = "english",
           category_selection = ".") {
    create_named_list <- function(df, col1, col2) {
      # Extract the columns and create a named list
      named_list <- df %>%
        dplyr::select({{col1}}, {{col2}}) %>%
        dplyr::pull({{col2}}, name = {{col1}})

      return(named_list)
    }

    replace_named_elements <- function(char_vec, named_vec) {
      # Find indices where elements of char_vec match the names in named_vec
      idx <- match(char_vec, names(named_vec))

      # Replace the matched elements
      char_vec[!is.na(idx)] <- named_vec[idx[!is.na(idx)]]

      return(char_vec)
    }

    if(rosetta_stone %>%
       gplyr::filter_in(category, category_selection) %>%
       nrow() == 0){
      stop("Category selection does not match any containted in Rosetta Stone.")
    }


    # Filter the rosetta_stone based on category selection
    rosetta_stone <- rosetta_stone %>%
      gplyr::filter_in(category, category_selection) %>%
      dplyr::select(-category) %>%
      create_named_list(from, to)

    replace_named_elements(words, rosetta_stone)
  }



#' Translate column values between languages
#'
#' This function takes a tibble, a column to be translated, a dataset that acts
#' as a translation dictionary, and optional arguments specifying the languages
#' to translate from and to. It returns a tibble with the specified column's values
#' translated.
#'
#' @param df A tibble containing the data.
#' @param col The name of the column to be translated.
#' @param rosetta_stone A tibble that serves as a translation dictionary, with columns
#'                      named after the languages it supports and a 'category' column.
#' @param from The name of the column in `rosetta_stone` representing the source language.
#' @param to The name of the column in `rosetta_stone` representing the target language.
#' @param category_selection Optional category selection to filter rows in `rosetta_stone`.
#'
#' @details
#' This is a convenience function to access translate_vec without a mutate step.
#' The function matches the values in the specified column of `df` with the
#' source language column in `rosetta_stone`. It then replaces the matched
#' values with the corresponding values from the target language column.
#' If a match is not found, the original value is retained.
#'
#' @examples
#' \dontrun{
#' rosetta <- tibble(french = c("le", "une"), english = c("the", "a"), category = "articles")
#' tibble(words = c("a", "the", "une", "le", "other")) %>%
#'   translate(words, rosetta, from = "french", to = "english", category_selection = "articles")
#' }
#'
#' @return A tibble with the translated column.
#' @importFrom gplyr quickm
#' @importFrom magrittr %>%
#' @export
translate <- function(df, col, rosetta_stone, from = "french", to = "english",
                      category_selection = ".") {

  df %>%
    gplyr::quickm({{col}}, translate_vec, rosetta_stone,
           from = from, to = to, category_selection = category_selection)
}


