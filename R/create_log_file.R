# create_log_file.R
#
# Purpose: Create a log file of the checks performed.
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-08-10
#=====================================================================================

create_log_file <- function(path){
  first_node <- loadLog()
  if (is.null(first_node) == FALSE){
    log_file_path <- path
    log_file <- write_xml(first_node, log_file_path)
    return(first_node)
  }
}

# [END]