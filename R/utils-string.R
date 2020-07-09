
#' Remove comments
#'
#' @description Remove comments from a string
#'
#' @param str A character string
#'
#' @return The character string stripped from its comments

#' @export
#'
#' @examples
#' strip_comments(c("test","test2 # with a comment", "# just a comment"))
strip_comments <- function(str){
  gsub(pattern = "#[^\\\n]*", replacement = "", x = str)
  # Or code from [www.rosettacode.org](https://www.rosettacode.org/wiki/Strip_comments_from_a_string#R)
  # stingr::str_trim(stingr::str_split_fixed(str, "#", 2)[, 1])
}

#' Remove empty lines
#'
#' @description Remove empty lines from a string, such as "" or "  "
#'
#' @param str A character string
#'
#' @return The character string stripped from empty lines
#' @export
#'
#' @examples
#' strip_empty_lines(c("test","", "  "))
strip_empty_lines = function(str){
  str[!grepl("^[[:blank:]]*$",str)]
}

#' Split string at blank
#'
#' @description Split a string using blank characters (i.e. one or more tab or space)
#'
#' @param str A character string
#'
#' @return A vector of strings
#' @export
#'
#' @examples
#' split_at_blank(c("word1 word2   word3  word4"))
#' # NB: first separation uses one space, second two spaces, third a tabulation
split_at_blank = function(str){
  strsplit(str, "\t+")[[1]]
}

