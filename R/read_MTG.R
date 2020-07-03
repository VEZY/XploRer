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

  MTG = parse_MTG_MTG(MTG,classes,description,features)

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
#' @param classes The MTG classes
#' @param description The MTG description
#' @param features The MTG features
#'
#'
#' @return A parsed MTG
#' @keywords internal
#'
parse_MTG_MTG = function(MTG,classes,description,features){

  section_begin = grep("MTG:", MTG)
  section_header = split_at_blank(MTG[section_begin+1])

  if(section_header[1] != "ENTITY-CODE" && section_header[1] != "TOPO"){
    stop("Neither ENTITY-CODE or TOPO were found in the MTG header")
  }

  common_features= section_header[-1] %in% features$NAME
  if(!all(common_features)){
    stop("unknown ENTITY-CODE in MTG: ",section_header[!common_features])
  }

  nb_features = length(section_header[-1])
  MTG_code = MTG[(section_begin+2):length(MTG)]
  MTG= parse_MTG_lines(MTG_code,features)
}


#' Parse MTG lines
#'
#' @description Parse the MTG lines (called from [parse_MTG_MTG()])
#'
#' @param MTG A pre-formatted MTG
#' @param classes The MTG classes
#' @param description The MTG description
#' @param features The MTG features
#'
#'
#' @return A parsed MTG
#' @keywords internal
#'
parse_MTG_lines = function(MTG_code,classes,description,features){

  # Splitting columns:
  splitted_MTG= strsplit(MTG_code,"[[:blank:]]")

  # AMAPStudio always adds an unnecessary Scene as the root of the MTG,
  # we ignore it:
  if(grepl("/Scene",splitted_MTG[[1]])){
    splitted_MTG= splitted_MTG[-1]
    if(strtrim(splitted_MTG[[1]][1],1) == "^"){
      splitted_MTG[[1]][1] = stringr::str_sub(splitted_MTG[[1]][1],2)
    }
  }

  # Getting the root node:
  root_node = split_MTG_elements(splitted_MTG[[1]][1])[[1]][1]
  root_element = parse_MTG_node(root_node)

  root = data.tree::Node$new(name = root_element$name)

  for(i in seq_len(length(splitted_MTG))){

    node_data= splitted_MTG[[i]]
    node_attr = parse_MTG_node_attr(node_data,features)

    node= split_MTG_elements(node_data[1])[[1]]
    lapply(node, parse_MTG_node)

    if(node[1] == "^"){
      # Here we have to define the case where the line continues relative to the
      # last line of code given in the same column. Hence, we have to keep track
      # of the last node position to be able to retreive it, and this for all newly
      # defined column in the topology
    }
    # strsplit(x = "+A1/U1<U2+S1", "(?<=.)(?=[</+])",perl = TRUE)
  }



}

#' Split MTG line
#'
#' @description Split the elements (e.g. inter-node, growth unit...) in an MTG line
#'
#' @param MTG_line An MTG line (e.g. "/P1/A1")
#'
#' @return A vector of elements (keeping their link, e.g. + or <)
#'
#' @keywords internal
#'
split_MTG_elements = function(MTG_line){
  strsplit(x = MTG_line, "(?<=.)(?=[</+])",perl = TRUE)
}

#' Parse MTG node attributes
#'
#' @description Parse the attributes names, values and type
#'
#' @param MTG_line An MTG line (e.g. "/P1/A1")
#'
#' @return A list of attributes
#'
#' @keywords internal
#'
parse_MTG_node_attr = function(node_data,features){
  node_attr= vector(length = nrow(features))
  node_attr = as.list(node_data[-1])
  names(node_attr) = features[seq_along(node_data[-1]),1]
  node_attr[stringr::str_length(node_attr) == 0] = NA

  node_type = features[seq_along(node_data[-1]),2]

  if(any(node_type == "INT")){
    node_attr[node_type == "INT"] =
      as.integer(node_attr[node_type == "INT"])
  }

  if(any(node_type == "REAL")){
    node_attr[node_type == "REAL"] =
      as.numeric(node_attr[node_type == "REAL"])
  }
  node_attr
}

#' Parse MTG node
#'
#' @description Parse MTG nodes (called from [parse_MTG_lines()])
#'
#' @param MTG_node An MTG node (e.g. "/Individual0")
#'
#' @return A parsed node in the form of a list of three:
#' - the link
#' - the name
#' - and the index
#'
#' @keywords internal
#'
parse_MTG_node = function(MTG_node){
  node = stringr::str_sub(MTG_node,c(1,2,-1),c(1,-2,-1))
  node = setNames(as.list(node),c("link","name","index"))
  node[3] = as.numeric(node[3])
  node
}

