require(dplyr)
require(tidyr)
require(stringi)

#' @title Remove Common Administrative and Government Descriptors from Entity Names
#' @description This function cleans the entity names in the specified column ('name')
#' of a dataset by iteratively searching for and removing common local government,
#' administrative, and geographic descriptor terms (e.g., "city of", "municipality",
#' "province", "alcaldia de") found at the beginning, end, or inside parentheses
#' of the name string. It attempts to standardize names for better matching in subsequent steps.
#'
#' The cleaning process is case-insensitive and handles multiple languages (English, French, Spanish, Portuguese, etc.).
#'
#' @param dataset A data frame containing the entity names to be cleaned. It must
#'   contain a column named \code{name}.
#'
#' @return The modified \code{dataset} data frame with the \code{name} column
#'   updated by removing the specified descriptive words.
#' @export


remove_extra_updated <- function (dataset)
{
  col <- "name"
  dataset <- .col_check(dataset, col, environment())
  if (exists("to.stop")) {
    stop(paste0("Stopping function. Please create or rename a \"",
                col, "\"", "column."))
  }
  words <- c("city", "city of", "town", "town of", "village", "village of", "City-State of",
             "City and Metropolitan Municipality of", "City District Government",
             "City Government of", "The Local Government of",
             "municipality of", "municipality", "district municipality of", "district municipality",
             "municipal government", "municipal corporation", "district of ", "province", "province of",
             "district", "municipal people's government", "municipal council", "municipal council of",
             "metropolitan municipality", "metropolitan municipality of",
             "local municipality of", "local municipality", "district council", "city council", "city and county",
             "the city of", "Metropolitan Area", "People's Government of", "Capital Municipality of",
             "prefecture of", "prefecture", #English City
             "County Council", "Metropolitan Borough Council", "Borough Council", "National Park Authority",
             "government", "state government", "state", "region",
             "autonomous region", "regency", "province", "voivodeship","autonomous community", "capital region",
             "government of", "state government of", "state of", "region of",
             "autonomous region of", "regency of", "province of", "voivodeship of","autonomous community of", "capital region of",
             "autonomous community of the",#English Region
             "ville", "ville de", "municipalite", "municipalite de", "la mairie de", "commune de", # French
             "municipalidad de", "distrito de", "municipalidad distrital de", "municipalidad provincial de",
             "municipalidad de provincial de", "municipalidad", "ilustre municipalidad de", "region metropolitana de",
             "provincia de", "alcaldia", "alcaldia de", "gobierno municipal de", "gobierno municipal de la",
             "gobierno municipal de la ciudad", "Alcaldia Distrital de", "Concejo Municipal de Distrito de",
             "Gobierno Municipal de la Ciudad de",
             "municipio", "municipio de", "ayuntamiento", "ayuntamiento de", "Presidencia Municipal de",
             "distrito metropolitano de", "Intendencia de",
             "Villa De",'Vila de', "Concello De", "Ajuntament De",#Spanish
             "prefeitura de", "prefeitura do", "prefeitura da", "prefeitura municipal de", "prefeitura do municipio de",
             "prefeitura da cidade de", "prefeitura da cidade do",             # Portuguese
             "Majlis Bandaraya"
  )
  words_rm <- paste0(c(paste("\\s", trimws(tolower(words)), "$", sep = ""),
                       paste("^", trimws(tolower(words)), "\\s", sep = ""),
                       paste("\\s\\(", trimws(tolower(words)), "\\)$", sep = "")
  ),
  collapse = "|")
  dataset$name <- trimws(gsub(words_rm, " ", dataset$name,
                              ignore.case = T))
  if (exists(paste0("namename"))) {
    names(dataset)[grepl("^name$", names(dataset))] <- namename
  }
  return(dataset)
}
