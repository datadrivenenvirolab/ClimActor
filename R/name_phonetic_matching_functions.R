# Data-Driven Lab
# Functions used for cleaning and fuzzy matching of actors' names
# Need to import the pipe '%>%' function for use in one of our functions later on

#' @importFrom dplyr %>%
#' @importFrom stats na.omit

#' @export
#' @title Cleans name using exact matches
#' @description Cleans the datasets' actors names based on the key dictionary using
#' exact string matching
#'
#' @param dataset Dataset to clean the actors' names for
#' @param key.dict Key dictionary to use to clean the actors' names
#' @param clean_enc Is the data read in with the correct encoding?
#' If unknown, set as FALSE. Defaults to TRUE.
#' @return Returns a dataset with actors names' cleaned using exact string matching.
#' @return Also creates a vector of indices of names that require cleaning.
#'
#' @example \dontrun{clean_name(df, key_dict, clean_enc = F)}
clean_name <- function(dataset, key.dict, clean_enc = T) {
  if (!is.logical(clean_enc)){
    stop("clean_enc argument requires a logical (True/False) input.")
  }
  # If not sure if data is clean, check and convert to try and repair the encoding
  if (!clean_enc){
    dataset$name <- .check_and_convert(dataset$name)
  }
  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name")
  dataset <- .col_check(dataset, "entity.type")
  dataset <- .col_check(dataset, "iso")
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity.type\", or \"iso\" columns.")
  }
  # First find actors that have the correct names and iso but not the correct entity type
  # Subset data for those that have correct names and iso first
  name_iso_right <- which(paste0(dataset$name, dataset$iso) %in% paste0(key.dict$right, key.dict$iso))
  # Coerce NA entity types to random character
  dataset$entity.type[intersect(which(is.na(dataset$entity.type)), name_iso_right)] <- ","
  # Check for wrong entity types in those that have correct names and iso
  dict_ind <- na.omit(match(paste0(dataset$name[name_iso_right], dataset$iso[name_iso_right]),
                            paste0(key.dict$right, key.dict$iso)))
  ent_ind <- name_iso_right[dataset$entity.type[name_iso_right] != key.dict$entity.type[dict_ind]]
  dict_ent_ind <- dict_ind[dataset$entity.type[name_iso_right] != key.dict$entity.type[dict_ind]]
  # Let users decide which entity type conflicts they want to resolve
  if (length(ent_ind) != 0){
    # Print number of conflicts
    cat(paste0("We found ", length(ent_ind), " number of entries with the same actor name ",
               "and iso but conflicting entity types. Would you like to resolve all",
               " conflicts by accepting the key dictionary's entity type? (Y/N/Skip)"))
    ans <- readline(prompt = "Answer: ")
    # Make sure user enters valid response
    # Allow users to 1) accept all key dict's entity types 2) resolve conflicts 1 by 1
    # or 3) not resolve conflicts
    while (toupper(ans) != "Y" & toupper(ans) != "N" & toupper(ans) != "SKIP"){
      cat("Please enter a valid input (Y/N/Skip)")
      ans <- readline(prompt = "Answer: ")
    }
    if (toupper(ans) == "Y"){
      # Resolve conflicts by taking all key dict's entity types
      dataset$entity.type[ent_ind] <- key.dict$entity.type[dict_ent_ind]
    } else if (toupper(ans) == "N"){
      # Resolve conflicts 1 by 1
      cat("Proceeding to resolve conflict of entity types actor by actor\n")
      for (k in seq_along(ent_ind)){
        # Iterate through conlficts 1 by 1 to let user select which entity type they want
        # to keep
        cat(paste0("For actor ", dataset$name[ent_ind[k]], ", ", dataset$iso[ent_ind[k]],
                   " you had an entity type of ", dataset$entity.type[ent_ind[k]],
                   " while our key dictionary had entity type of ",
                   key.dict$entity.type[dict_ent_ind[k]], ". Which entity type would you like to keep?",
                   "\n\n 1. Dataset \n 2. Key Dictionary \n S. Stop resolving conflicts"))
        ans2 <- readline(prompt = "Please input either 1/2/S: ")
        # Allow users to take the key dict's entity type, keep their own, or skip
        while (ans2 != "1" & ans2 != "2" & toupper(ans2) != "S"){
          cat("Please enter a valid input (1/2/S)")
          ans2 <- readline(prompt = "Answer: ")
        }
        if (ans2 == "1"){
          dataset$entity.type[ent_ind[k]] <- key.dict$entity.type[dict_ent_ind[k]]
        } else if (ans2 == "2"){
          next
        } else if (toupper(ans2) == "S"){
          cat("Stop resolving conflicts in entity types.")
          break
        }
      }} else if (toupper(ans) == "SKIP"){
        cat("Entity types will not be changed for now.")
      }
  }

  # Clean database by doing an exact match with the key dictionary
  # Find indices within key dict where there is a match with the "wrong" column of the key dictionary
  match_keydict <- na.omit(match(paste0(dataset$name, dataset$iso, dataset$entity.type),
                                 paste0(key.dict$wrong, key.dict$iso, key.dict$entity.type)))
  # Find the corresponding indices within the dataset
  match_df <- which(!is.na(match(paste0(dataset$name, dataset$iso, dataset$entity.type),
                                 paste0(key.dict$wrong, key.dict$iso, key.dict$entity.type))))
  # Replace the dataset name with the right name in key dict
  if (length(match_keydict) != 0) {
    dataset$name[match_df] <- key.dict$right[match_keydict]
  }

  # Now find all row numbers that match name, iso, and entity type
  # Do this by pasting name, iso and entity type together and then matching on those
  all3_matching_rows <- which((paste0(dataset$name, dataset$iso,
                                      dataset$entity.type) %in%
                                 paste0(key.dict$right, key.dict$iso,
                                        key.dict$entity.type)))


  # creating the vector of indices (in the dataset) of the names that need to be cleaned
  if (length(all3_matching_rows) != 0) {
    indices <<- 1:nrow(dataset)
    indices <<- indices[-all3_matching_rows]
  } else {
    indices <<- 1:nrow(dataset)
  }
  # Helper function returns output that checks if the name is capitalized
  # Change back to capitalized version if the check is true
  if (exists(paste0("isoname"))){
    names(dataset)[grepl("iso", names(dataset))] <- isoname
  }
  if (exists(paste0("entity.typename"))){
    names(dataset)[grepl("entity.type", names(dataset))] <- entity.typename
  }
  if (exists(paste0("namename"))){
    names(dataset)[grepl("name", names(dataset))] <- namename
  }
  return(dataset)
}

#' @export
#' @title Matches country names based on fuzzy matching
#' @description \code{fuzzify_country} cleans the "country" column in the user's dataset
#' for those names that did not find an exact match in the existing country_dictionary.
#' Given the small size of the country dictionary and the relatively fewer number of
#' countries (as compared to climate actors), a fuzzy string matching algorithm using
#' the Levenshtein distance is used for the fuzzy matching instead of the phonetic
#' algorithms used for matching climate actor names.
#' @param dataset Dataset containing countries by user
#' @param country_keydict Key dictionary to clean actors' countries against
#' @return Cleaned dataset with countries standardized against the country dictionary.
#' @return A few vectors of indices will also be created to store the indices of those
#' countries that needs to be matched. The first is a vector of indices of all actors
#' that require cleaning. \code{unmatched_count} is a vector of indices of countries
#' not cleaned by the function. \code{custom_count} is a vector of indices
#' denoting countries for which custom actor names are given by the user, and will be
#' used to update the country dictionary.
#'
fuzzify_country <- function(dataset, country_keydict){
  # Do the usual checks
  if (!exists("country_ind")){
    stop("Please run the clean_country_iso function first before running this function.")
  }
  if (length(country_ind) == 0){
    stop("The countries within the dataset have already been cleaned")
  }
  dataset <- .col_check(dataset, "country")
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"country\" columns.")
  }
  # We only want to iterate through unique names that are not matched
  # Remove duplicates here
  country_short <- country_ind[!duplicated(dataset$country[country_ind]) & !is.na(dataset$country[country_ind])]
  if (any(is.na(dataset$country[country_ind]))){
    unmatched_count <<- country_ind[is.na(dataset$country[country_ind])]
  }
  .count_updates <<- data.frame(ind = country_short,
                                name = dataset$country[country_short])
  # Iterate through the names that are wrong
  for (i in country_short){
    # Subset for index
    origname <- dataset$country[i]
    # Calculate distance (using Levenstein distance) and find the 15 closest countries
    # that match
    tmp <- country_dict$right[order(adist(dataset$country[i],
                                          country_keydict$wrong))[1:15]]
    # Print out the top 15 matches
    print(paste0("The original country is ", dataset$country[i], " (Actor name: ",
                 dataset$name[i], ")"))
    print(paste0("Here are some possible matches we found: "))
    cat(paste0(1:15, ". ", tmp, "\n"))

    # Let users choose which name they want to keep
    cat("Which name matches? Choose 1-15. (Type N if none match; type S to save your current progress)")
    ans1 <- readline(prompt = "Answer: ")
    ## if one of the listed matches is correct (and not NA), the standardized
    ## version of the matched name will be replaced into the dataset
    # Use grepl to prevent warnings
    if (grepl("^[0-9*]$", ans1)){
      if (!is.na(ans1) &
          (as.numeric(ans1) %in% 1:15) &
          (!is.na(tmp[as.numeric(ans1)]))) {
        correct.name <- tmp[as.numeric(ans1)]
        print(paste0(correct.name, " has been selected and will replace ",
                     dataset$country[i], " in the database."))

        # replacing all instances of the recently matched (raw) name in the dataset
        # with the standardized name
        samecount_inds <- which(dataset$country == origname)

        if (length(samecount_inds) != 0) {
          dataset$country[samecount_inds] <- correct.name
          country_ind <<- country_ind[!(country_ind %in% samecount_inds)]
        }
      }
    } else if (substr(toupper(as.character(ans1)), 1, 1) == "S") {
      cat("Your current progress will be returned.")
      cat("If it's not saved, be sure you saved your results in a variable with an assign function")
      return(dataset)

    } else if (substr(toupper(as.character(ans1)), 1, 1) == "N") {
      cat("Do you want to enter a custom name instead? (Y/N)")
      ans2 <- readline(prompt = "Answer: ")

      ### if the user enters a custom name, the name will be replaced in the dataset,
      ### and the index will be added to the custom_indices vector to make it easier
      ### for the user to look back at the name later and fix, if necessary
      ### if the user wants to add the custom names to the key dictionary,
      ### the update_key_dict function can be used
      if (substr(toupper(as.character(ans2)), 1, 1) == "Y") {
        ans3 <- readline(prompt = "Enter in custom name: ")
        ans3 <- as.character(ans3)
        print(paste0("The name (", ans3, ") will be kept in the dataset.\nThe name has not been added to the key dictionary yet but can be added with the update_country_dict function.\nThe row number of the custom name has been added to a vector called custom_count"))
        if (!exists("custom_count")){
          custom_count <<- i
        } else {
          custom_count <<- c(custom_count, i)
        }

        # replacing all instances in the dataset of the original (raw) name
        # with the new custom name
        samecount_inds <- which(dataset$name == origname)

        if (length(samecount_inds) != 0) {
          dataset$country[samecount_inds] <- ans3
          country_ind <<- country_ind[!(country_ind %in% samecount_inds)]
        }
        ### if the user chooses not to enter a custom name, the original name
        ### will be kept, and the index will be added to the unmatched_indices vector
      } else if (toupper(as.character(ans2)) == "N") {
        print(paste0("The previous name ( ", origname, ") will be kept."))
        if (!(exists("unmatched_count"))){
          unmatched_count <<- i
        } else {
          unmatched_count <<- c(unmatched_count, i)
        }
      }
      ## if the user makes a typo or other invalid answer, the function will continue, and
      ## the index will be added to the unmatched_indices vector for the user to look at later
      ## (or restart the function again later)
    } else {
      print("Sorry, an invalid answer was provided.")
      print(paste0("The previous name (", origname, ") will be kept.",
                   " The index of this entry will be recorded for future inspection."))
      if (!(exists("unmatched_count"))){
        unmatched_count <<- i
      } else {
        unmatched_count <<- c(unmatched_count, i)
      }
    }
  }
  # making these vectors more readable by getting rid of NAs
  if (exists("custom_count")){
    custom_count <<- custom_count[!is.na(custom_count)]
  }
  if (exists("unmatched_count")){
    unmatched_count <<- unmatched_count[!is.na(unmatched_count)]
    unmatched_count <<- sort(unmatched_count)
  }
  # Helper function returns output that checks if the name is capitalized
  # Change back to capitalized version if the check is true
  if (exists(paste0("countryname"))){
    names(dataset)[grepl("country", names(dataset))] <- countryname
  }
  return(dataset)
}

#' @export
#' @title Updates the country dictionary
#' @description Update the country dictionary with the custom countries given by the
#' user and the countries that were not in the country dictionary.
#' @param dataset Dataset by user
#' @param country.dict country dictionary
#' @param custom_count Vector of numbers containing which indices were new custom
#' countries added by the user
#' @return Country dictionary with updated entries
#'
#' @example \dontrun{update_country_dict(df, country_dict, custom_count)}
#'
update_country_dict <- function(dataset, country.dict, custom_count){
  # Do the usual checks for column name and indices
  dataset <- .col_check(dataset, "country")
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"country\" columns.")
  }
  if (!exists(".count_updates")){
    stop(paste("Stopping function. There are either no new updates to the country dictionary",
               "or you have not used the fuzzify_country function yet."))
  }
  # First update those that have custom inputs
  cust_df <- data.frame(wrong = .count_updates$name[.count_updates$ind %in% custom_count],
                        right = NA,
                        code = NA,
                        iso = NA,
                        region = NA,
                        Landarea = NA,
                        iso2 = NA,
                        Population = NA,
                        PopulationGroup = NA)

  # Now remove the names that have custom inputs
  .count_updates <- .count_updates[-which(.count_updates$ind %in% custom_count), ]
  # Remove those that have not been cleaned as well
  .count_updates <- .count_updates[-which(.count_updates$ind %in% unmatched_count), ]
  .count_updates$right <- dataset$country[.count_updates$ind]
  update_dict <- data.frame(wrong = .count_update$name,
                            right = .count_updates$right,
                            code = NA,
                            iso = NA,
                            region = NA,
                            Landarea = NA,
                            iso2 = NA,
                            Population = NA,
                            PopulationGroup = NA)
  update_dict <- merge(update_dict, country.dict, by = "right",
                       all.x = T, all.y = F)
  country.dict <- rbind(country.dict, update_dict, cust_df)
  return(country.dict)

}


#' @export
#' @title Cleans name using phonetic matching
#' @description \code{phonetify_names} searches for actors' names which are not standardized
#' and standardizes them according to the key dictionary. Users get to choose which name
#' is correct based on a selection narrowed down by using phonetic matching. Users can
#' choose to input custom names should the names not be in the key dictionary.
#' @param dataset Dataset containing actors by user
#' @param key.dict Key dictionary to clean actors' names against
#' @return Cleaned dataset with actors names standardized against the key dictionary.
#' @return A few vectors of indices will also be created to store the indices of those names
#' that needs to be matched. The first is a vector of indices of all actors that
#' require cleaning. \code{unmatched_indices} is a vector of indices of names
#' not cleaned by the function. \code{custom_indices} is a vector of indices
#' denoting names for which custom actor names are given by the user, and will be
#' used to update the key dictionary.
phonetify_names <- function(dataset, key.dict) {

  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name")
  dataset <- .col_check(dataset, "entity.type")
  dataset <- .col_check(dataset, "iso")
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity.type\", or \"iso\" columns.")
  }
  # creating a vector of indices (in the dataset) of the names that need to be fuzzy matched

  # dataset.tmp <- dataset[!duplicated(paste0(dataset$name,
  # dataset$iso, dataset$entity.type)), ]

  # all3_short <- which((paste0(dataset.tmp$name, dataset.tmp$iso,
  # dataset.tmp$entity.type) %in%
  # paste0(key.dict$right, key.dict$iso,
  # key.dict$entity.type)))
  # indices_short <-
  ## finding all the row numbers in which the dataset matches the key dictionary
  all3_matching_rows <- which((paste0(dataset$name, dataset$iso,
                                      dataset$entity.type) %in%
                                 paste0(key.dict$right, key.dict$iso,
                                        key.dict$entity.type)))
  ind.short <- which(!duplicated(dataset$name[-all3_matching_rows]))
  if (length(ind.short) == 0){
    stop("It seems the dataset has already been cleaned.")
  }
  ## creating the vector of indices (in the dataset) of the names that need to be fuzzy matched
  if (length(all3_matching_rows) != 0) {
    indices <<- 1:nrow(dataset)
    indices <<- indices[-all3_matching_rows]
  } else {
    indices <<- 1:nrow(dataset)
  }

  # creating a vector that--during the function--tracks which indices aren't
  # fuzzy matched by the end of the whole function
  if (!exists("unmatched_indices")) {
    unmatched_indices <<- rep(NA, length(indices))
  } else if (length(unmatched_indices) < length(indices)) {
    unmatched_indices <<- append(unmatched_indices, rep(NA, (length(indices) - length(unmatched_indices))))
  } else if ((length(unmatched_indices) == length(indices)) &
             (all(unmatched_indices[!is.na(unmatched_indices)] <= nrow(dataset)))) {
    cat("A vector that seems to contain valid indices that have not been matched (named unmatched_indices) already exists. \n Would you like to proceed using this vector? (Y/N)")
    ans_u1 <- readline(prompt = "Answer: ")
    if (substr(toupper(as.character(ans_u1)), 1, 1) == "Y") {
      print("Function will proceed using this vector")
    } else if (substr(toupper(as.character(ans_u1)), 1, 1) == "N") {
      stop("We'll stop the function here to let you decide what to do with the vector.")
    } else {
      cat("An invalid answer was provided, but we'll continue with this vector for now. \n Feel free to stop the function if this is not what you want.")
    }
  } else if ((length(unmatched_indices) == length(indices)) &
             (any(unmatched_indices[!is.na(unmatched_indices)] > nrow(dataset)))) {
    cat("A vector of indices named unmatched_indices already exists and contains invalid indices (e.g. values beyond the row numbers in the dataset or characters). \n Please check the vector to be sure that:
    \n1. It is a vector of indices from the dataset that have not been fuzzy matched and
    \n2. Each element in the vector is a possible row number from the dataset (within the range of the total # of rows)

\nWould you like the function to proceed with this vector anyways (future edits may be difficult)? (Y/N). ")
    ans_u2 <- readline(prompt = "Answer: ")
    if (substr(toupper(as.character(ans_u2)), 1, 1) == "Y") {
      print("Function will proceed using this vector.")
    } else if (substr(toupper(as.character(ans_u2)), 1, 1) == "N") {
      stop("We'll stop the function here to let you decide what to do with the vector.")
    } else {
      print("An invalid answer was provided, but we'll continue with this vector for now.
            Feel free to stop the function if this is not what you want.")
    }
  } else {
    print("A vector of indices named unmatched_indices already exists, but
          there might be something wrong with it. We'll proceed with the function for now,
          but feel free to stop the function if necessary.")
  }

  # creating a vector that--during the function--tracks which indices received custom names
  # to make it easier for the user to look back at the name later and fix, if necessary
  # note to readers: the code below is the exact same as the code to create the unmatched_indices vector,
  # except the name of the vector is different
  if (!exists("custom_indices")) {
    custom_indices <<- rep(NA, length(indices))
  } else if (length(custom_indices) < length(indices)) {
    custom_indices <<- append(custom_indices, rep(NA, (length(indices) - length(custom_indices))))
  } else if ((length(custom_indices) == length(indices)) &
             (all(custom_indices[!is.na(custom_indices)] <= nrow(dataset)))) {
    cat("A vector that seems to contain valid indices that have custom names (named custom_indices) already exists.\nWould you like to proceed using this vector? (Y/N) ")
    ans_c1 <- readline(prompt = "Answer: ")
    if (substr(toupper(as.character(ans_c1)), 1, 1) == "Y") {
      print("Function will proceed using this vector :)")
    } else if (substr(toupper(as.character(ans_c1)), 1, 1) == "N") {
      stop("We'll stop the function here to let you decide what to do with the vector.")
    } else {
      print("An invalid answer was provided, but we'll continue with this vector for now.\nFeel free to stop the function if this is not what you want.")
    }
  } else if ((length(custom_indices) == length(indices)) &
             (any(custom_indices[!is.na(custom_indices)] > nrow(dataset)))) {
    cat("A vector of indices named custom_indices already exists and contains invalid indices (e.g. values beyond the row numbers in the dataset or characters).

Please check the vector to be sure that:
    1. It is a vector of indices from the dataset that have not been fuzzy matched and
    2. Each element in the vector is a possible row number from the dataset (within the range of the total # of rows)

Would you like the function to proceed with this vector anyways (future edits may be difficult)? (Y/N). ")
    ans_c2 <- readline(prompt = "Answer: ")
    if (substr(toupper(as.character(ans_c2)), 1, 1) == "Y") {
      print("Function will proceed using this vector")
    } else if (substr(toupper(as.character(ans_c2)), 1, 1) == "N") {
      stop("We'll stop the function here to let you decide what to do with the vector.")
    } else {
      print("An invalid answer was provided, but we'll continue with this vector for now.
            Feel free to stop the function if this is not what you want.")
    }
  } else {
    print("A vector of indices named custom_indices already exists, but there might be something wrong with it.
    We'll proceed with the function for now, but feel free to stop the function, if necessary.")
  }

  ## creating a dataframe of all phonetic codes for all the names that need to be fuzzy matched
  ds.phon.codes <- phonics::phonics(.coerce_location_names(dataset$name[ind.short]), c("caverphone", "caverphone.modified",
                                                                                       "cologne", "lein", "metaphone",
                                                                                       "nysiis", "nysiis.modified", "onca",
                                                                                       "onca.modified", "onca.refined",
                                                                                       "onca.modified.refined", "phonex",
                                                                                       "rogerroot", "soundex",
                                                                                       "soundex.refined", "statcan"), clean = FALSE)

  ds.phon.codes$rogerroot <- as.character(ds.phon.codes$rogerroot)
  # Export the dataframe to be used for updating the dictionary
  .actor_updates <<- data.frame(ind = ind.short,
                                name_wrong = dataset$name[ind.short])
  # starting the fuzzy matching process:

  for (i in seq_along(ind.short)) {

    # collecting all scores from all phonetic algorithm/edit distance methods
    # for all names in key.dict$wrong

    ind <- ind.short[i]

    ## filtering out only the rows of the key dictionary that have a matching
    ## iso and entity type as the actor that is currently being fuzzy matched
    kd.filtered <- key.dict %>% dplyr::filter(iso == dataset$iso[ind] & entity.type == dataset$entity.type[ind])
    kd.filtered$rogerroot <- as.character(kd.filtered$rogerroot)

    ## calculating scores using the caverphone phonetic algorithm
    ## quantification of scores is based on 9 edit distance methods

    kd.filtered <- kd.filtered %>% dplyr::mutate(metaphone.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$metaphone[i],
                                                                                                        kd.filtered$metaphone, method = "dl"),
                                                                                  stringdist::stringsim(ds.phon.codes$metaphone[i],
                                                                                                        kd.filtered$metaphone, method = "qgram"),
                                                                                  stringdist::stringsim(ds.phon.codes$metaphone[i],
                                                                                                        kd.filtered$metaphone, method = "cosine"),
                                                                                  stringdist::stringsim(ds.phon.codes$metaphone[i],
                                                                                                        kd.filtered$metaphone, method = "jaccard"),
                                                                                  stringdist::stringsim(ds.phon.codes$metaphone[i],
                                                                                                        kd.filtered$metaphone, method = "jw")),
                                                                            na.rm = T),
                                                 nysiis.modified.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                                              kd.filtered$nysiis.modified, method = "dl"),
                                                                                        stringdist::stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                                              kd.filtered$nysiis.modified, method = "qgram"),
                                                                                        stringdist::stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                                              kd.filtered$nysiis.modified, method = "cosine"),
                                                                                        stringdist::stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                                              kd.filtered$nysiis.modified, method = "jaccard"),
                                                                                        stringdist::stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                                              kd.filtered$nysiis.modified, method = "jw")),
                                                                                  na.rm = T),
                                                 onca.modified.refined.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                                    kd.filtered$onca.modified.refined, method = "dl"),
                                                                                              stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                                    kd.filtered$onca.modified.refined, method = "qgram"),
                                                                                              stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                                    kd.filtered$onca.modified.refined, method = "cosine"),
                                                                                              stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                                    kd.filtered$onca.modified.refined, method = "jaccard"),
                                                                                              stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                                    kd.filtered$onca.modified.refined, method = "jw")),
                                                                                        na.rm = T),
                                                 phonex.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$phonex[i],
                                                                                                     kd.filtered$phonex, method = "dl"),
                                                                               stringdist::stringsim(ds.phon.codes$phonex[i],
                                                                                                     kd.filtered$phonex, method = "qgram"),
                                                                               stringdist::stringsim(ds.phon.codes$phonex[i],
                                                                                                     kd.filtered$phonex, method = "cosine"),
                                                                               stringdist::stringsim(ds.phon.codes$phonex[i],
                                                                                                     kd.filtered$phonex, method = "jaccard"),
                                                                               stringdist::stringsim(ds.phon.codes$phonex[i],
                                                                                                     kd.filtered$phonex, method = "jw")),
                                                                         na.rm = T),
                                                 rogerroot.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$rogerroot[i],
                                                                                                        kd.filtered$rogerroot, method = "dl"),
                                                                                  stringdist::stringsim(ds.phon.codes$rogerroot[i],
                                                                                                        kd.filtered$rogerroot, method = "qgram"),
                                                                                  stringdist::stringsim(ds.phon.codes$rogerroot[i],
                                                                                                        kd.filtered$rogerroot, method = "cosine"),
                                                                                  stringdist::stringsim(ds.phon.codes$rogerroot[i],
                                                                                                        kd.filtered$rogerroot, method = "jaccard"),
                                                                                  stringdist::stringsim(ds.phon.codes$rogerroot[i],
                                                                                                        kd.filtered$rogerroot, method = "jw")),
                                                                            na.rm = T))


    # #calculating the total score based on phonetic algorithms

    kd.filtered <- kd.filtered %>% dplyr::mutate(tot.phon.score = #caverphone.score +
                                                   #caverphone.modified.score +
                                                   #cologne.score +
                                                   #lein.score +
                                                   metaphone.score +
                                                   #nysiis.score +
                                                   nysiis.modified.score +
                                                   #onca.score + onca.modified.score + onca.refined.score +
                                                   onca.modified.refined.score + phonex.score + rogerroot.score
                                                 #soundex.score +
                                                 #soundex.refined.score +
                                                 #statcan.score
    )



    # arranging by total score in descending order so we can list the top matches (based on those scores)
    kd.filtered <- kd.filtered %>% dplyr::arrange(dplyr::desc(tot.phon.score))

    # listing the top 15 best-scoring fuzzy-matched names
    print(paste0("The original name is ", dataset$name[ind]))
    print(paste0("Here are some possible matches we found: "))

    for (k in 1:15) {
      if (is.na(kd.filtered$right[k])) {
        next
      } else {
        print(paste0(k, ": ", kd.filtered$right[k]))
      }
    }

    # keeping track of the original name so that all of the same raw names can be changed simultaneously
    origname <- dataset$name[ind]

    # interactive matching where user matches one of the fuzzy matches--
    # the right version of the matched name will replace the dataset's name
    # if there is no right version of the fuzzy matched options, the original name (in the dataset) will be kept
    # two vectors will be updated:
    # one (unmatched_indices) tracks all indices with unchanged/unmatched names and
    # the other vector (custom_indices) tracks all the indices that had custom names

    cat("Which name matches? Choose 1-15. (Type N if none match; type S to save your current progress)")
    ans1 <- readline(prompt = "Answer: ")
    ## if one of the listed matches is correct (and not NA), the standardized
    ## version of the matched name will be replaced into the dataset
    if (!is.na(ans1) &
        (as.numeric(ans1) %in% c(1:15)) &
        (!is.na(kd.filtered$right[ans1]))) {
      correct.name <- kd.filtered$right[ans1]
      print(paste0(correct.name, "has been selected and will replace ", dataset$name[ind],
                   " in the database."))

      # replacing all instances of the recently matched (raw) name in the dataset
      # with the standardized name
      samename_inds <- which(dataset$name == origname)

      if (length(samename_inds) != 0) {
        dataset$name[samename_inds] <- correct.name
        indices <<- indices[!(indices %in% samename_inds)]
      }
      ## if the user response was numeric but not one of the listed matches,
      ## the original name will be kept and that index will be added to the unmatched_indices vector
      ## if none of the listed matches are correct, the user can either enter a
      ## custom name or choose not to do anything for the time being
    } else if (substr(toupper(as.character(ans1)), 1, 1) == "S") {
      cat("Your current progress will be returned.")
      cat("If it's not saved, be sure you saved your results in a variable with an assign function")
      return(dataset)

    } else if (substr(toupper(as.character(ans1)), 1, 1) == "N") {
      cat("Do you want to enter a custom name instead? (Y/N)")
      ans2 <- readline(prompt = "Answer: ")

      ### if the user enters a custom name, the name will be replaced in the dataset,
      ### and the index will be added to the custom_indices vector to make it easier
      ### for the user to look back at the name later and fix, if necessary
      ### if the user wants to add the custom names to the key dictionary,
      ### the update_key_dict function can be used
      if (substr(toupper(as.character(ans2)), 1, 1) == "Y") {
        ans3 <- readline(prompt = "Enter in custom name: ")
        ans3 <- as.character(ans3)
        print(paste0("The name (", ans3, ") will be kept in the dataset.
        The name has not been added to the key dictionary yet but can be added with the update_key_dict function.
        The row number of the custom name has been added to a vector called custom_indices."))
        custom_indices[i] <<- ind

        # replacing all instances in the dataset of the original (raw) name
        # with the new custom name
        samename_inds <- which(dataset$name == origname)

        if (length(samename_inds) != 0) {
          dataset$name[samename_inds] <- ans3
          indices <<- indices[!(indices %in% samename_inds)]
        }
        ### if the user chooses not to enter a custom name, the original name
        ### will be kept, and the index will be added to the unmatched_indices vector
      } else if (substr(toupper(as.character(ans2)), 1, 1) == "N") {
        print(paste0("The previous name (", origname, ") will be kept."))
        unmatched_indices[i] <<- ind
      }
      ## if the user makes a typo or other invalid answer, the function will continue, and
      ## the index will be added to the unmatched_indices vector for the user to look at later
      ## (or restart the function again later)
    } else {
      print("Sorry, an invalid answer was provided.")
      print(paste0("The previous name (", origname, ") will be kept.",
                   " The index of this entry will be recorded for future inspection."))
      unmatched_indices[i] <<- ind
    }
  }
  # making these vectors more readable by getting rid of NAs
  custom_indices <<- custom_indices[!is.na(custom_indices)]
  unmatched_indices <<- unmatched_indices[!is.na(unmatched_indices)]

  if (exists(paste0("isoname"))){
    names(dataset)[grepl("iso", names(dataset))] <- isoname
  }
  if (exists(paste0("entity.typename"))){
    names(dataset)[grepl("entity.type", names(dataset))] <- entity.typename
  }
  if (exists(paste0("namename"))){
    names(dataset)[grepl("name", names(dataset))] <- namename
  }
  # the final (hopefully cleaner) dataset!
  return(dataset)
}

#' @export
#' @title Update key dictionary
#' @description Update the key dictionary with the custom names given by the user and
#' the names that were not in the key dictionary.
#' @param dataset Dataset by user
#' @param key.dict Key dictionary
#' @param custom_indices Vector of numbers containing which indices were new custom names added by the user
#' @return Key dictionary with updated entries
#'
#' @example \dontrun{update_key_dict(df, key_dict, custom_indices)}
update_key_dict <- function(dataset, key.dict, custom_indices) {
  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name")
  dataset <- .col_check(dataset, "entity.type")
  dataset <- .col_check(dataset, "iso")
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity.type\", or \"iso\" columns.")
  }
  # Get the complete dataset of those that needs to be updated first
  .actor_updates <- cbind(.actor_updates, dataset[.actor_updates$ind])
  # Update the custom names first
  custom <- .actor_updates$ind %in% custom_indices
  cust_df <- data.frame(right = NA,
                        wrong = .actor_updates$name[custom],
                        iso = .actor_updates$iso[custom],
                        entity.type = .actor_updates$entity.type[custom],
                        allcaps = toupper(.actor_updates$name[custom]),
                        caverphone = phonics::caverphone(.actor_updates$name[custom], clean = FALSE),
                        caverphone.modified = phonics::caverphone(.actor_updates$name[custom], modified = TRUE, clean = FALSE),
                        cologne = phonics::cologne(.actor_updates$name[custom], clean = FALSE),
                        lein = phonics::lein(.actor_updates$name[custom], clean = FALSE),
                        metaphone = phonics::metaphone(.actor_updates$name[custom], clean = FALSE),
                        mra = phonics::mra_encode(.actor_updates$name[custom], clean = FALSE),
                        nysiis = phonics::nysiis(.actor_updates$name[custom], clean = FALSE),
                        nysiis.modified = phonics::nysiis(.actor_updates$name[custom], modified = TRUE, clean = FALSE),
                        onca = phonics::onca(.actor_updates$name[custom], clean = FALSE),
                        onca.modified = phonics::onca(.actor_updates$name[custom], modified = TRUE, clean = FALSE),
                        onca.refined = phonics::onca(.actor_updates$name[custom], refined = TRUE, clean = FALSE),
                        onca.modified.refined = phonics::onca(.actor_updates$name[custom], modified = TRUE, refined = TRUE, clean = FALSE),
                        phonex = phonics::phonex(.actor_updates$name[custom], clean = FALSE),
                        rogerroot = phonics::rogerroot(.actor_updates$name[custom], clean = FALSE),
                        soundex = phonics::soundex(.actor_updates$name[custom], clean = FALSE),
                        soundex.refined = phonics::refinedSoundex(.actor_updates$name[custom], clean = FALSE),
                        statcan = phonics::statcan(.actor_updates$name[custom], clean = FALSE))
  # Now update the rest
  # First remove those actors that are not yet cleaned and the custom names
  .actor_updates <- .actor_updates[-which(.actor_updates$ind %in% unmatched_indices), ]
  .actor_updates <- .actor_updates[-which(.actor_updates$ind %in% custom_indices), ]
  # Create the rows to be binded to key.dict
  update_df <- data.frame(right = .actor_updates$name,
                          wrong = .actor_updates$name_wrong,
                          iso = .actor_updates$iso,
                          entity.type = .actor_updates$entity.type,
                          allcaps = toupper(.actor_updates$name_wrong),
                          caverphone = phonics::caverphone(.actor_updates$name_wrong, clean = FALSE),
                          caverphone.modified = phonics::caverphone(.actor_updates$name_wrong, modified = TRUE, clean = FALSE),
                          cologne = phonics::cologne(.actor_updates$name_wrong, clean = FALSE),
                          lein = phonics::lein(.actor_updates$name_wrong, clean = FALSE),
                          metaphone = phonics::metaphone(.actor_updates$name_wrong, clean = FALSE),
                          mra = phonics::mra_encode(.actor_updates$name_wrong, clean = FALSE),
                          nysiis = phonics::nysiis(.actor_updates$name_wrong, clean = FALSE),
                          nysiis.modified = phonics::nysiis(.actor_updates$name_wrong, modified = TRUE, clean = FALSE),
                          onca = phonics::onca(.actor_updates$name_wrong, clean = FALSE),
                          onca.modified = phonics::onca(.actor_updates$name_wrong, modified = TRUE, clean = FALSE),
                          onca.refined = phonics::onca(.actor_updates$name_wrong, refined = TRUE, clean = FALSE),
                          onca.modified.refined = phonics::onca(.actor_updates$name_wrong, modified = TRUE, refined = TRUE, clean = FALSE),
                          phonex = phonics::phonex(.actor_updates$name_wrong, clean = FALSE),
                          rogerroot = phonics::rogerroot(.actor_updates$name_wrong, clean = FALSE),
                          soundex = phonics::soundex(.actor_updates$name_wrong, clean = FALSE),
                          soundex.refined = phonics::refinedSoundex(.actor_updates$name_wrong, clean = FALSE),
                          statcan = phonics::statcan(.actor_updates$name_wrong, clean = FALSE))
  # Bind rows to key.dict
  key.dict <- rbind(key.dict, update_df, cust_df)
  return(key.dict)
}

#' @export
#' @title Merge contextual data into original dataset
#' @description Contextual data from the contextuals database obtained from a variety
#' of sources. Contextual information includes region, population, latitude,
#' longitude, area, elevation, and the initiatives committed by the actor. Merging is done
#' based on actors' name, iso, and entity type.
#' @param dataset Original dataset
#' @param contextual_df Contextuals database included in the package
#' @param contextuals Vector consisting of contextuals to be merged into the dataset
#' includes "region", "pop", "lat", "lng", "area", "elevation", "intiatives_committed".
#' Defaults to taking all the contextual information.
#' @return Dataset with contextuals merged
#'
#' @example \dontrun{contextualize_data(df, contextuals_df, c("pop", "region", "lat", "lng"))}
contextualize_data <- function(dataset, contextual_df, contextuals = c("region", "population",
                                                                       "lat", "lng", "area",
                                                                       "elevation",
                                                                       "initiatives_committed")){
  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name")
  dataset <- .col_check(dataset, "entity.type")
  dataset <- .col_check(dataset, "iso")
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity.type\", or \"iso\" columns.")
  }
  # Incorporate check to make sure that the contextuals for merging are in the
  # contextual dataframe
  if (any(!(contextuals %in% names(contextual_df)))){
    stop("The contextuals argument needs to match the column names in the contextual
         dataframe. See ?contextualize_data for a list of applicable column names.")
  }
  contextuals <- c(contextuals, "name", "iso", "entity.type")
  # Merge and keep all of the original dataset's data
  merge_df <- merge(dataset, contextual_df[ , contextuals],
                    by = c("name", "iso", "entity.type"), all.x = T)
  return(merge_df)

}

