# Data-Driven Lab
# Helper functions for package

#' Helper function to check for names column
#'
#' @param dataset
#' @param col
.col_check <- function(dataset, col) {
  if (!any(grepl(col, tolower(names(dataset))))){
    cat(paste0("No \"" , col, "\"",
               "column is detected in the dataset.",
               " Would you like to specify a column to rename?"))
        ans <- readline(prompt = "Rename column? (Y/N): ")
        while (toupper(ans) != "Y" & toupper(ans) != "N"){
          ans <- readline(prompt = "Please input either Y/N:")
        }
        if (toupper(ans) == "Y"){
          ans2 <- readline(prompt = "Please input column name to be renamed:")
          names(dataset)[grepl(tolower(ans2), tolower(names(dataset)))] <- col
        } else if (toupper(ans) == "N"){
          stop()
          on.exit(assign("to.stop", T), add = T)
        }
  }
  # Check for similar versions of "col"
  col.names <- gsub("[[:punct:]]", "", tolower(names(dataset)))
  if (any(grepl(gsub("[[:punct:]]", "", col), col.names))){
    names(dataset)[grepl(gsub("[[:punct:]]", "", col), col.names)] <- col
    assign(paste0(col, "name"),
           names(dataset)[grepl(gsub("[[:punct:]]", "", col), col.names)])
  }
  return(dataset)
}

#' Coerce location names to handle special characters
#'
#' @param locations Column with names of location (city, country, etc.)
#' @return A vector of locations names with the special character replaced with
#' closest equivalent
#' @example coerce_locations_names(df$name)
.coerce_location_names <- function(locations) {
  locations <- gsub("[ÀÁÂÃÄÅÆĀĂĄǍǞǠǢǺǼȀȂẤẦẨẶȦḀẠẢ]", "A", locations)
  locations <- gsub("[àáâãäåæāăąǎǟǡǣǻǽȁȃấầẩặȧḁạảẚ]", "a", locations)
  locations <- gsub("[ḂḄḆɃ]", "B", locations)
  locations <- gsub("[ḃḅḇƀᵬᶀɓ]", "b", locations)
  locations <- gsub("[ÇĆĈĊČ]", "C", locations)
  locations <- gsub("[çćĉċč]", "c", locations)
  locations <- gsub("[ÐĎĐ]", "D", locations)
  locations <- gsub("[ðďđ]", "d", locations)
  locations <- gsub("[ÈÉÊËĒĔĖĘĚȄȆ]", "E", locations)
  locations <- gsub("[èéêëēĕėęěȅȇ]", "e", locations)
  locations <- gsub("[Ƒ]", "F", locations)
  locations <- gsub("[ƒ]", "f", locations)
  locations <- gsub("[ĜĞĠĢǤǦ]", "G", locations)
  locations <- gsub("[ĝğġģǥǧ]", "g", locations)
  locations <- gsub("[ĤĦ]", "H", locations)
  locations <- gsub("[ĥħ]", "h", locations)
  locations <- gsub("[ÌÍÎÏĨĪĬĮİĲǏȈȊ]", "I", locations)
  locations <- gsub("[ìíîïĩīĭįıĳǐȉȋ]", "i", locations)
  locations <- gsub("[ĴɈ]", "J", locations)
  locations <- gsub("[ĵǰɉ]", "j", locations)
  locations <- gsub("[Ķ]", "K", locations)
  locations <- gsub("[ķĸ]", "k", locations)
  locations <- gsub("[ĹĻĽĿŁ]", "L", locations)
  locations <- gsub("[ĺļľŀłƚ]", "l", locations)
  locations <- gsub("[ÑŃŅŇŊǸ]", "N", locations)
  locations <- gsub("[ñńņňŉŋǹ]", "n", locations)
  locations <- gsub("[ÒÓÔÕÖØŌŎŐŒƟƠƢǑǪǬȌȎȪȬȮȰ]", "O", locations)
  locations <- gsub("[òóôõöøōŏőœơƣǒǫǭȍȏȫȭȯȱ]", "o", locations)
  locations <- gsub("[ŔŖŘȐȒɌ]", "R", locations)
  locations <- gsub("[ŕŗřȑȓɍ]", "r", locations)
  locations <- gsub("[ŚŜŞŠȘ]", "S", locations)
  locations <- gsub("[śŝşšș]", "s", locations)
  locations <- gsub("[ŢŤŦƬƮ]", "T", locations)
  locations <- gsub("[ţťŧƫƭ]", "t", locations)
  locations <- gsub("[ÙÚÛÜŨŪŬŮŰŲǓǕǗǙǛȔȖ]", "U", locations)
  locations <- gsub("[ùúûüũūŭůűųǔǖǘǚǜȕȗ]", "u", locations)
  locations <- gsub("[Ŵ]", "W", locations)
  locations <- gsub("[ŵ]", "w", locations)
  locations <- gsub("[ÝŶŸɎ]", "Y", locations)
  locations <- gsub("[ýÿŷɏ]", "y", locations)
  locations <- gsub("[ŹŻŽẒẔẐŹƵ]", "Z", locations)
  locations <- gsub("[źżžẓẕẑʐʑȥƶ]", "z", locations)

  return(locations)
}


#' Helper function to help check and convert the encoding of the column specified
#' @param column Column to check and convert the encoding for
#' @return column with the encoding (hopefully) converted
.check_and_convert <- function(col){
  # Check if there exists a hidden environment to create hidden variables for
  # If not, then create one
  if (!exists(".pkgenv")){
    .pkgenv <- new.env(parent = emptyenv())
  }
  if (!exists("foreign", envir = .pkgenv)){
    assign("foreign", names(which(!unlist(l10n_info()[2:3]))),
           envir = .pkgenv)
    assign("native", names(which(unlist(l10n_info()[2:3]))),
           envir = .pkgenv)
  }
  if (.pkgenv$native == "UTF-8"){
    col <- iconv(col, from = "UTF-8", to = "latin1")
    col <- iconv(col, from = "latin1", to = "UTF-8")
  } else if (.pkgenv$native == "Latin-1"){
    col <- iconv(col, from = "latin1", to = "UTF-8")
    col <- iconv(col, from = "UTF-8", to = "UTF-8")
  }
  return(col)
}
