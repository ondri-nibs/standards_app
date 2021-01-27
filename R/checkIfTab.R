# checkIfTab.R
#
# Purpose: To identify if the inputed data is of a tabular format or Non-Tabular
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-06-03
#
# ========================================================================================

#function iterates through the file names in the primary folder
#and looks for a '.' if there's a '.' in the name it is cosidered a file
#else it is a folder => it would be tabular

checkIfTab <- function(folder_path){
  num_items <- length(list.files(folder_path))
  items <- list.files(folder_path)
  
  find_dot <- str_detect(items, "\\.")
  
  num_files <- length(find_dot[find_dot] == TRUE)
  
  #dealing with special case where no folder is chosen so we have 0 == 0
  if (num_files + num_items == 0){
    type = "NONE"
  }
   else if (num_files == num_items){
    type = "tabular"
  }else {
    type = "non-tabular"
  } 
  
  return(type)
}

#[END]