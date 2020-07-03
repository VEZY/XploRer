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

  code = parse_MTG_code(MTG_file)
  classes = parse_MTG_classes(MTG_file)
  description = parse_MTG_description(MTG_file)
  features = parse_MTG_features(MTG_file)

  decomp = c('NONE', 'FREE', 'CONNECTED', 'NOTCONNECTED', 'LINEAR', 'PURELINEAR', '<-LINEAR', '+-LINEAR')

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
    stop("Section ",sections[!is_section_in_MTG]," not found in the MTG file. ",
         "\nPlease see this link for more details on the MTG strucure: ",
         "http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/intro.html#mtg-a-plant-architecture-databases"
    )
  }

  if(!identical(match(MTG_sections,sections), 1:5)){
    stop("Sections of the MTG file are not in the right order.\n",
         " Expected: ",paste(sections, collapse = " "),"\n Found: ",
         paste(MTG_sections, collapse = " "))
  }
}

#' Parse MTG section
#'
#' @description Parse any section from an MTG
#'
#' @param MTG A pre-formatted MTG
#' @param section_name The section name in the file
#' @param header The header of the section
#' @param next_section The name of the next section
#' @param allow_empty Boolean. Allow the section to be empty ?
#'
#' @return A data.frame with the section results, or a tree for the MTG.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG_file = strip_comments(MTG_file)
#' MTG_file = strip_empty_lines(MTG_file)
#'
#' # Checking that all sections are present and ordered properly:
#' check_sections(MTG_file)
#' # parse the classes section from the MTG:
#' parse_MTG_section(MTG_file, "CLASSES:",
#'   c('SYMBOL', 'SCALE', 'DECOMPOSITION', 'INDEXATION', 'DEFINITION'),
#'   "DESCRIPTION:",FALSE)
#' }
parse_MTG_section = function(MTG,section_name,header,next_section,allow_empty){
  section_begin = grep(section_name, MTG)

  section_header = split_at_blank(MTG[section_begin+1])

  if(!all(section_header %in% header)){
    if(allow_empty && all(section_header == next_section)){
      # The section is empty, no constraints:
      return(data.frame())
    }else{
      stop("The header of the MTG ",section_name," section is different than:\n",
           header)
    }
  }

  next_section = grep(next_section, MTG)
  # NB: The sections are ordered, and the MTG is checked before for compliance
  section= MTG[(section_begin+2):(next_section-1)]
  section_df=
    as.data.frame(
      matrix(data = character(0), nrow = length(section), ncol = length(header)),
      stringsAsFactors= FALSE)
  colnames(section_df) = header

  section = strsplit(section,"[[:blank:]]+")

  for(i in seq_len(length(section))){
    class_i= section[[i]]
    if(length(class_i) != length(header)){
      stop("The ", i, "th class from the ",section_name,
           " section in the MTG file does not have length ",length(header))
    }
    section_df[i,] = trimws(class_i)
  }

  section_df
}

#' Parse MTG code
#'
#' @description Parse the code section from an MTG
#'
#' @param MTG A pre-formatted MTG
#'
#' @return The MTG code
#' @keywords internal
#'
parse_MTG_code = function(MTG){
  code_section = grep("CODE:", MTG)
  code = gsub("CODE:","",MTG[code_section])
  code = trimws(code)
  if(code != "FORM-A" && code != "FORM-B"){
    stop("Unknown MTG format. CODE: ",code,". Only FORM-A and FORM-B allowed.")
  }

  code
}


#' Parse MTG classes
#'
#' @description Parse the classes section from an MTG
#'
#' @param MTG A pre-formatted MTG
#'
#' @return A data.frame with the classes
#' @keywords internal
#'
parse_MTG_classes = function(MTG){
  parse_MTG_section(MTG,
                   "CLASSES:",
                   c('SYMBOL', 'SCALE', 'DECOMPOSITION', 'INDEXATION', 'DEFINITION'),
                   "DESCRIPTION:",FALSE)
}

#' Parse MTG description
#'
#' @description Parse the description section from an MTG
#'
#' @param MTG A pre-formatted MTG
#'
#' @return A data.frame with the description
#' @keywords internal
#'
parse_MTG_description = function(MTG){
  description_df= parse_MTG_section(MTG,"DESCRIPTION:",
                                   c("LEFT", "RIGHT", "RELTYPE", "MAX"),
                                   "FEATURES:",TRUE)

  description_df$RIGHT= strsplit(description_df$RIGHT, ",")
  if(!all(description_df$RELTYPE %in% c("+","<"))){
    stop("Unknown relation type(s): ",
         paste(unique(description_df$RELTYPE[!(description_df$RELTYPE %in% c("+","<"))]),
               collapse = " "))
  }
  description_df
}

#' Parse MTG features
#'
#' @description Parse the features section from an MTG
#'
#' @param MTG A pre-formatted MTG
#'
#' @return A data.frame with the features
#' @keywords internal
#'
parse_MTG_features = function(MTG){
  parse_MTG_section(MTG,"FEATURES:",
                    c("NAME", "TYPE"),
                    "MTG:",TRUE)
}



#' Parse MTG
#'
#' @description Parse the MTG section from an MTG file
#'
#' @param MTG A pre-formatted MTG
#' @param features the MTG features, see details
#'
#' @details If the features argument is empty, the function uses `[parse_MTG_features()]`
#' to find it.
#'
#' @return A data.frame with the classes
#' @keywords internal
#'
parse_MTG_MTG = function(MTG,features=NULL){

  if(is.null(features)){
    features = parse_MTG_features(MTG)
  }

  section_begin = grep("MTG:", MTG)
  section_header = split_at_blank(MTG[section_begin+1])

  common_features= section_header[-1] %in% features$NAME
  if(!all(common_features)){
    stop("unknown ENTITY-CODE in MTG: ",section_header[!common_features])
  }

  if(!all(section_header %in% header)){
    if(allow_empty && all(section_header == next_section)){
      # The section is empty, no constraints:
      return(data.frame())
    }else{
      stop("The header of the MTG ",section_name," section is different than:\n",
           header)
    }
  }
}


