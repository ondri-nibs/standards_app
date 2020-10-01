# dataRetainer.R
#
# Purpose: Stores and loads data inputted by the user on the select page.
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2020-07-20¤
#
# ==========================================================================================


# Store data for the current session
saveData <- function(data) {
  responses <<- data
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


#[END]