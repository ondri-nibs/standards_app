platformSpecificDF <- utils::read.csv("data-raw/platformspecificSiteCodes.csv", 
                                      stringsAsFactors = FALSE, 
                                      colClasses = "character")
usethis::use_data(platformSpecificDF, internal = TRUE, overwrite = TRUE)