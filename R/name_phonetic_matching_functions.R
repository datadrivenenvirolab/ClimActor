# Data-Driven Lab
# Functions used for cleaning and fuzzy matching of actors' names

#' Cleans the datasets' actors names based on the key dictionary using 
#' exact string matching
#' 
#' @param dataset Dataset to clean the actors' names for
#' @param key.dict Key dictionary to use to clean the actors' names
#' @return Returns a dataset with actors names' cleaned using exact string matching
#' @example clean_name(df, key_dict)
clean_name <- function(dataset, key.dict) {
  
  # creating a vector of indices (in the dataset) of the names that need to be cleaned
  
  # Find all row numbers that match name, iso, and entity type
  # Do this by pasting name, iso and entity type together and then matching on those
  
  all3_matching_rows <- which((paste0(dataset$name, dataset$iso,
                                      dataset$entity.type) %in%
                                 paste0(key.dict$right, key.dict$iso,
                                        key.dict$entity.type)))
  # ## finding all the row numbers in which the dataset matches the key dictionary
  # ## finding the row numbers in which all of the dataset's name, iso, and entity type match
  # ## any row in the key dictionary's name, iso, and entity type
  
  ## creating the vector of indices (in the dataset) of the names that need to be cleaned
  if (length(all3_matching_rows) != 0) {
    indices <- 1:nrow(dataset)
    indices <- indices[-all3_matching_rows]
  } else {
    indices <- 1:nrow(dataset)
  }
  
  # cleaning names by getting rid of extraneous words
  # this list of words can be updated in the future
  words <- c("council|adjuntament|corporation|government|urban|district|mayor|
           the|of|city|autonomous|state|province|provincial|county|municipality|
           municipalidad de|municipalidad|municipio|kommune|municipal|prefecture|
           prefectural|metropolitana|metropolis|m??tropole|metropolitan|metropole|town|
           community|communat|communat??|Ayuntamiento|Gemeente|Comune di|Comune|Kommune|
           Republic")
  dataset$name[indices] <- stringr::str_replace_all(dataset$name[indices],
                                                    stringr::regex(words, ignore_case = T), "")
  
  dataset$name[indices] <- trimws(dataset$name[indices])
  
  return(dataset)
}
