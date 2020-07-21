
#' Remove comments
#'
#' @description Remove comments from a string
#'
#' @param str A character string
#'
#' @return The character string stripped from its comments

#' @keywords internal
#'
#' @examples
#' \dontrun{
#' strip_comments(c("test","test2 # with a comment", "# just a comment"))
#' }
strip_comments = function(str){
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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' strip_empty_lines(c("test","", "  "))
#' }
strip_empty_lines = function(str){
  str[!grepl("^[[:blank:]]*$",str)]
}

#' Split string at tab
#'
#' @description Split a string using tabulation characters
#'
#' @param str A character string
#'
#' @return A vector of strings
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' split_at_tab(c("word1\tword2"))
#' }
split_at_tab = function(str){
  strsplit(str, "\t")
}

