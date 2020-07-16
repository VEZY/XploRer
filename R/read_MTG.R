#' Read MTG
#'
#' @description Read an MTG file.
#' @param file  The path to the MTG file
#'
#' @note See the documentation for the MTG format from the
#' [OpenAlea webpage](http://openalea.gforge.inria.fr/doc/vplants/newmtg/doc/_build/html/user/intro.html#mtg-a-plant-architecture-databases)
#'  for further details.
#'
#' @return A named list of four sections: the MTG classes, description, features,
#' and MTG. The MTG is a [data.tree] data structure.
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

  MTG = parse_MTG_MTG(MTG_file,classes,description,features)

  return(list(classes = classes, description = description,
              features = features,MTG = MTG))
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

  section_header = strsplit(MTG[section_begin+1], "\t+")[[1]]

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
  if(nrow(description_df) == 0){
    return(description_df)
  }
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

  section_begin = grep("MTG", MTG)
  section_header = split_at_tab(MTG[section_begin+1])[[1]]

  if(section_header[1] != "ENTITY-CODE" && section_header[1] != "TOPO"){
    stop("Neither ENTITY-CODE or TOPO were found in the MTG header")
  }

  common_features= section_header[section_header != ""][-1] %in% features$NAME
  if(!all(common_features)){
    stop("unknown ENTITY-CODE in MTG: ",section_header[!common_features])
  }

  attr_column_start = which(section_header != "")[-1][1]

  MTG_code = MTG[(section_begin+2):length(MTG)]

  # Splitting columns:
  splitted_MTG= strsplit(MTG_code,"\t")

  # AMAPStudio always adds an unnecessary Scene as the root of the MTG,
  # we ignore it:
  if(grepl("/Scene",splitted_MTG[[1]][1])){
    splitted_MTG= splitted_MTG[-1]
    if(strtrim(splitted_MTG[[1]][1],1) == "^"){
      splitted_MTG[[1]][1] = stringr::str_sub(splitted_MTG[[1]][1],2)
    }
  }

  # Getting the root node:
  node_1_node = split_MTG_elements(splitted_MTG[[1]][1])[[1]][1]
  node_1_element = parse_MTG_node(node_1_node)
  node_1_attr= c(parse_MTG_node_attr(node_data = splitted_MTG[[1]],features,
                                     attr_column_start),
                 .link = node_1_element$link,
                 .symbol = node_1_element$symbol,
                 .index = node_1_element$index)

  # Initializing "last_node_column". Will be used to remember the last node
  # built for any given column (help to connect new nodes to the right node)
  max_columns = max(unlist(lapply(splitted_MTG, length)))
  last_node_column = c(1,rep(NA_integer_, max_columns - 1))

  # Create the root node (the first one):
  node_1 = data.tree::Node$new(name = paste0("node_",1))

  # Assign the attributes to the root :
  for(i in names(node_1_attr)){
    node_1[[i]] = node_1_attr[[i]]
  }

  node_id = 2

  for(i in seq_len(length(splitted_MTG))[-1]){
    node_name = paste0("node_",node_id)
    node_data= splitted_MTG[[i]]
    node_column = find_MTG_node_column(node_data)
    node_data = node_data[node_column:length(node_data)]

    node_attr_column_start = attr_column_start - node_column + 1
    node = split_MTG_elements(node_data[1])
    node = expand_node(node)

    # Get node attributes:
    node_attr = parse_MTG_node_attr(node_data,features,node_attr_column_start)

    # Declare a new node as object (because the methods associated to nodes are OO):
    # assign(node_name, data.tree::Node$new(node_name))

    if(node[1] == "^"){
      # The parent node is the last one built on the same column
      parent_column = last_node_column[node_column]
      if(is.na(parent_column)){
        stop("Node defined at line ",i," uses the '<' notation but is the first on its column")
      }
    }else{
      # The parent node is the last one built on column - 1.
      parent_column = last_node_column[node_column - 1]

      if(is.na(parent_column)){
        stop("Can't find the parent of Node defined at line ",i,
             ". It may be defined on a column that is too far right.")
      }
    }

    building_nodes = seq_along(node)[node != "^"]
    for(k in building_nodes){
      node_element = parse_MTG_node(node[k])

      # NB: if several nodes are declared on the same line, the attributes are defined
      # for the last node only, unless "<.<" or "+.+" are used
      if(k == length(node) || k %in% attr(node,"shared")){
        node_k_attr= c(node_attr,
                       .link = node_element$link,
                       .symbol = node_element$symbol,
                       .index = node_element$index)
      }else{
        node_k_attr= c(.link = node_element$link,
                       .symbol = node_element$symbol,
                       .index = node_element$index)
      }
      if(k == min(building_nodes)){
        parent_node = paste0("node_",parent_column)
      }else{
        parent_node = paste0("node_",node_id-1)
      }
      node_name = paste0("node_",node_id)

      # Call the "AddChild" method from the parent to add our new node as its child:
      # eval(parse(text=parent_node))[["AddChild"]](node_name)
      assign(node_name,eval(parse(text=parent_node))[["AddChild"]](node_name))
      # Assign the attributes to the current node :
      for(j in names(node_k_attr)){
        assign(j, node_k_attr[[j]], j, eval(parse(text=node_name)))
      }

      last_node_column[node_column] = node_id
      node_id = node_id + 1
    }
  }

  node_1
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
#' @examples
#' \dontrun{
#' split_MTG_elements("/A1+U85/U86<U87<.<U93<U94<.<U96<U97+.+U100")
#'}
split_MTG_elements = function(MTG_line){
  strsplit(x = MTG_line, "(?<=.)(?=[</+])",perl = TRUE)[[1]]
}

#' Expand MTG line
#'
#' @description Expand the elements denoted by the syntactic sugar "<<", "<.<", "++" or "+.+"
#'
#' @param x A split MTG line (e.g. c("/P1","/A1"))
#'
#' @return A split MTG line with explicitly all nodes
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' x = strsplit(x = "/A1+U85/U86<U87<.<U93<U94<.<U96<U97+.+U100",
#' "(?<=.)(?=[</+])",perl = TRUE)[[1]]
#' expand_node(x)
#' }
#'
expand_node = function(x){
  if(any(x %in% c("<.","+."))){
    shared = TRUE
  }else{
    shared = FALSE
  }
  node_to_expand = which(x %in% c("<","<.","+","+."))

  if(length(node_to_expand) > 0){
    MTG_node_parsed = lapply(x, function(x) parse_MTG_node(x))
    symbol = unlist(lapply(MTG_node_parsed, function(x) x$symbol))
    index = unlist(lapply(MTG_node_parsed, function(x) x$index))
    link = unlist(lapply(MTG_node_parsed, function(x) x$link))

    for(i in seq_along(x[node_to_expand])){
      expanded_index =
        (index[node_to_expand - 1][i]+1):
        (index[node_to_expand + 1][i] - 1)

      expanded_nodes = paste0(gsub("\\.","",link[node_to_expand[i]]),
                              symbol[node_to_expand[i] - 1],
                              expanded_index)

      x[node_to_expand[i]] = list(expanded_nodes)
    }
    if(shared){
      shared = unlist(c(x[node_to_expand[i] - 1], x[node_to_expand[i] + 1]))
    }
    x = unlist(x)
  }

  shared_index = which(x %in% shared)
  if(length(shared_index) > 0){
    attr(x,"shared") = seq(shared_index[1],shared_index[2])
  }else{
    attr(x,"shared") = NULL
  }
  x
}

#' Parse MTG node attributes
#'
#' @description Parse the attributes names, values and type
#'
#' @param MTG_line An MTG line (e.g. "/P1/A1")
#' @param features The features data.frame
#' @param attr_column_start The index of the column of the first attribute
#'
#' @return A list of attributes
#'
#' @keywords internal
#'
parse_MTG_node_attr = function(node_data,features,attr_column_start){
  node_attr= vector(length = nrow(features))
  node_data_attr = node_data[-c(1:(attr_column_start-1))]
  node_attr = as.list(node_data_attr)
  # NB: -c(1,2) because the first one is the node topology, and the second one
  # is used to separate the topology from the attributes
  names(node_attr) = features[seq_along(node_data_attr),1]
  node_attr[stringr::str_length(node_attr) == 0] = NA

  node_type = features[seq_along(node_data_attr),2]

  if(any(node_type == "INT")){
    node_attr[node_type == "INT" & node_attr == ""] =
      NA_integer_
    node_attr[node_type == "INT" & node_attr != ""] =
      as.integer(node_attr[node_type == "INT" & node_attr != ""])
  }

  if(any(node_type == "REAL")){
    node_attr[node_type == "REAL" & node_attr == ""] =
      NA_real_

    node_attr[node_type == "REAL" & node_attr != ""] =
      as.numeric(node_attr[node_type == "REAL" & node_attr != ""])
  }
  node_attr
}

#' Parse MTG node
#'
#' @description Parse MTG nodes (called from [parse_MTG_MTG()])
#'
#' @param MTG_node An MTG node (e.g. "/Individual0")
#'
#' @return A parsed node in the form of a list of three:
#' - the link
#' - the symbol
#' - and the index
#'
#' @keywords internal
#'
parse_MTG_node = function(MTG_node){
  if(MTG_node %in% c("^","<.","+.")){
    return(list(link = MTG_node, symbol = NA, index = NA))
  }
  link = stringr::str_sub(MTG_node,1,1)
  symbol = gsub("[^[:alpha:]]","",MTG_node)
  index = gsub("[^[:digit:]]","",MTG_node)
  list(link = link, symbol = symbol, index = as.numeric(index))
}




#' Find MTG node column
#'
#' @description Find the position of a node in the MTG (which column it belongs to)
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
#' @examples
#' \dontrun{
#' # First column (last column is missing):
#' find_MTG_node_column(c("/P1","/A1",""))
#' # Node starts at column 2:
#' find_MTG_node_column(c("","/U1","<U2"))
#'}
find_MTG_node_column = function(MTG_node){
  which(stringr::str_length(MTG_node) > 0)[1]
}
