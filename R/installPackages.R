#' @title installPackages
#'
#' @description A function for installing all required packages and dependencies.
#' @author Jedid Ahn
#' 
#' @export
#' 
installPackages <- function(){
  # CRAN packages.
  cranPackages <- c("devtools", "xml2", "shiny", "shinyFiles", "shinyWidgets", 
                    "shinyjs", "shinyalert", "V8", "stringr", "dplyr", 
                    "lubridate", "varhandle", "readr")
  
  
  invisible(sapply(cranPackages, function(p){
    if (p %in% rownames(utils::installed.packages()) == FALSE) {
      utils::install.packages(p)
    }
  }))
  
  
  # Deprecated package.
  if ("rowr" %in% rownames(utils::installed.packages()) == FALSE) {
    utils::install.packages("https://cran.r-project.org/src/contrib/Archive/rowr/rowr_1.1.3.tar.gz",
                            repos = NULL, type = "source")
  }
}

# [END]