
#' Read MTG
#'
#' @description Read an MTG file.
#' @param file  The path to the MTG file
#'
#' @note See the documentation for the MTG format from the
#' [OpenAlea webpage](http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/intro.html#mtg-a-plant-architecture-databases)
#'  for further details.
#'
#' @return An MTG tree graph
#'
#' @export
#'
#' @examples
#' \dontrun{
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' read_MTG(filepath)
#' }
read_MTG = function(file) {
  MTG_file = readLines(file)
  MTG_file = strip_comments(MTG_file)
  MTG_file = strip_empty_lines(MTG_file)
  # NB: could use pipes here, but can be ~2x slower

  # Checking that all sections are present and ordered properly:
  check_sections(MTG_file)

  # code = read_MTG_code(MTG_file)
  classes = read_MTG_classes(MTG_file)
}

#' Read MTG classes
#'
#' @description Read the classes section from an MTG
#'
#' @param MTG A pre-formatted MTG
#'
#' @return A data.frame with the classes
#' @keywords internal
#'
read_MTG_classes = function(MTG){
  decomp = c('NONE', 'FREE', 'CONNECTED', 'NOTCONNECTED', 'LINEAR', 'PURELINEAR', '<-LINEAR', '+-LINEAR')
  classes_begin = grep("CLASSES", MTG_file)
  if(length(classes_begin) < 1){
    stop("MTG file doesn't have a class section ('CLASSES'). ",
         "Please see this link for more details on the MTG strucure:\n",
         "http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/intro.html#mtg-a-plant-architecture-databases")
  }
  class_header = strsplit(MTG_file[classes_begin+1], "\t")[[1]]

  classes_names= c('SYMBOL', 'SCALE', 'DECOMPOSITION', 'INDEXATION', 'DEFINITION')
  if(!all(class_header %in% classes_names)){
    stop("The header of the MTG CLASSES section is different than:\n",
         "SYMBOL	SCALE	DECOMPOSITION	INDEXATION	DEFINITION")
  }

  next_section = grep("DESCRIPTION:", MTG_file)
  # NB: The sections are ordered, and the MTG is checked before for compliance
  classes= MTG_file[(classes_begin+2):(next_section-1)]
  classes_df=
    as.data.frame(
      matrix(data = character(0), nrow = length(classes), ncol = length(classes_names)),
      stringsAsFactors= FALSE)
  colnames(classes_df) = classes_names

  classes = strsplit(classes,"\t")

  for(i in seq_len(length(classes))){
    class_i= classes[[i]]
    if(length(class_i) != length(classes_names)){
      stop("The ", i, "th class from the CLASSES section in the MTG file does not have length 5")
    }
    classes_df[i,] = trimws(class_i)
  }

  classes_df
}



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



#' Check sections from MTG
#'
#' @description Test if the sections are all present in the MTG file, and written in
#' the right order.
#'
#' @return An error if a section is missing or not properly ordered, nothing otherwise
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' check_sections(mtg)
#' }
check_sections = function(MTG_file){
  sections= c("CODE:", "CLASSES:", "DESCRIPTION:", "FEATURES:","MTG:")
  MTG_sections_pos = grep("CODE:|CLASSES:|DESCRIPTION:|FEATURES:|MTG:", MTG_file)
  MTG_sections= gsub(pattern = ":[^\\\n]*", replacement = ":", x = MTG_file[MTG_sections_pos])
  is_section_in_MTG= sections %in% MTG_sections

  if(!all(is_section_in_MTG)){
    stop("Section ",sections[!is_section_in_MTG]," not found in the MTG file")
  }

  if(!identical(match(MTG_sections,sections), 1:5)){
    stop("Sections of the MTG file are not in the right order.\n",
         " Expected: ",paste(sections, collapse = " "),"\n Found: ",
         paste(MTG_sections, collapse = " "))
  }
}


