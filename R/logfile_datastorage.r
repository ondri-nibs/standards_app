# logfile_datastorage.R
#
# Purpose: stores xml style data for logs and can be used to load the xml data
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-07-20
#
# ==========================================================================================


# saving xml log file
saveLog <- function(data) {
  logs <<- data
}

# loading xml log file
loadLog <- function() {
  if (exists("logs")) {
    logs
  }
}


#[END]