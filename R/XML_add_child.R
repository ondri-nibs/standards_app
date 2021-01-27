
# XML_add_child.R
#
# Purpose: Adds a child tag to the specified parent tag. Also adds text and attr.
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-07-20¤
#
# ==========================================================================================

# location takes a string variable. (refers to which parent the child should be added)
# tag take a string variable written like an XML tag (ex. "<tag/>"), refers to the child tag which will be added

XML_add_child <- function(location, tag) {
  my_xml <- loadLog()
  if (is.null(my_xml)){
    my_xml <- read_xml("<ONDRI_STANDARDS><checks></checks></ONDRI_STANDARDS>")
  }
  # Only add the tag if the tag has not already been created.
  clean_tag <- str_remove_all(tag, "[</>]")
  if (length(xml_find_all(my_xml, paste(".//", clean_tag))) != 0 ){
     #do nothing
  }
   else{
    loc <- xml_find_all(my_xml, paste(".//", location))
    x <- read_xml(tag)
    xml_add_child(loc, read_xml(tag))
    saveLog(my_xml)
 }
}

# adds content to parent tag
XML_add_txt <- function(location, txt) {
  my_xml <- loadLog()
  loc <- xml_find_all(my_xml, paste(".//", location))
  xml_text(loc) <- txt
  saveLog(my_xml)
}

# adds status attribute to log element
XML_add_attr <- function(location, status){
  my_xml <- loadLog()
  loc <- xml_find_all(my_xml, paste(".//", location))
  xml_attr(loc, "status") <- status
  saveLog(my_xml)
}

# adds general attributes to element
XML_general_attr <- function(location, attr, id){
  my_xml <- loadLog()
  loc <- xml_find_all(my_xml, paste(".//", location))
  xml_attr(loc, attr) <- id
  saveLog(my_xml)
}
#[END]

