clean_name <- function(dataset, key.dict) {
  
  # creating a vector of indices (in the dataset) of the names that need to be cleaned 
  
  # Find all row numbers that match name, iso, and entity type
  # Do this by pasting name, iso and entity type together and then matching on those
  
  all3_matching_rows <- which((paste0(dataset$name, dataset$iso, 
                                      dataset$entity.type) %in%
                                 paste0(key.dict$right, key.dict$iso,
                                        key.dict$entity.type)))
  # ## finding all the row numbers in which the dataset matches the key dictionary
  # clean_names_rows <- which(!is.na(match(dataset$name, key.dict$right)))
  # matching_iso_rows <- which(!is.na(match(dataset$iso, key.dict$iso)))
  # matching_entity.type_rows <- which(!is.na(match(dataset$entity.type, key.dict$entity.type)))
  # 
  # ## finding the row numbers in which all of the dataset's name, iso, and entity type match 
  # ## any row in the key dictionary's name, iso, and entity type
  # matching_names_iso_rows <- intersect(clean_names_rows, matching_iso_rows)
  # all3_matching_rows <- intersect(matching_names_iso_rows, matching_entity.type_rows)
  
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
  # dataset$name[indices] <- gsub("\\W", "", dataset$name[indices])
  # dataset$name[indices] <- gsub("council", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("adjuntament", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("corporation", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("government", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("urban", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("district", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("mayor", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("the", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("of", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("city", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("autonomous", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("state", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("province", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("provincial", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("county", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("municipality", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("municipalidad de", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("municipalidad", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("municipio", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("kommune", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("municipal", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("prefecture", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("prefectural", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("metropolitana", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("metropolis", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("m??tropole", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("metropolitan", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("metropole", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("town", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("community", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("communat", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("communat??", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("Ayuntamiento", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("Gemeente", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("Comune di", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("Comune", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("Kommune", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("Republic", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("   ", "", dataset$name[indices], ignore.case = TRUE)
  # dataset$name[indices] <- gsub("  ", "", dataset$name[indices], ignore.case = TRUE)
  
  dataset$name[indices] <- trimws(dataset$name[indices])
  
  return(dataset)
}
