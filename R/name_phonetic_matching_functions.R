# Data-Driven EnviroLab
# Functions used for cleaning and fuzzy matching of actors' names
# Need to import the pipe '%>%' function for use in one of our functions later on

#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#' @importFrom utils adist

#' @export
#' @title Clean actor names using exact matches
#' @description Cleans the datasets' actor names based on the key dictionary using
#' exact string matching
#'
#' @param dataset Dataset to clean the actors' names for
#' @param key.dict Key dictionary to use to clean the actors' names
#' @param clean_enc Is the data read in with the correct encoding?
#' If unknown, set as FALSE. Defaults to TRUE.
#' @return Returns a dataset with actors names' cleaned using exact string matching.
#' Also creates a vector of indices of names that require cleaning.
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
  dataset <- .col_check(dataset, "name", environment())
  dataset <- .col_check(dataset, "entity_type", environment())
  dataset <- .col_check(dataset, "iso", environment())
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity_type\", or \"iso\" columns.")
  }

  # Clean database by doing an exact match with the key dictionary
  # Find indices within key dict where there is a match with the "wrong" column of the key dictionary
  match_keydict <- na.omit(match(paste0(trimws(toupper(dataset$name)), trimws(dataset$iso), trimws(dataset$entity_type)),
                                 paste0(trimws(toupper(key.dict$wrong)), key.dict$iso, key.dict$entity_type)))
  # Find the corresponding indices within the dataset
  match_df <- which(!is.na(match(paste0(trimws(toupper(dataset$name)), trimws(dataset$iso), trimws(dataset$entity_type)),
                                 paste0(trimws(toupper(key.dict$wrong)), key.dict$iso, key.dict$entity_type))))
  # Replace the dataset name with the right name in key dict
  if (length(match_keydict) != 0) {
    dataset$name[match_df] <- key.dict$right[match_keydict]
  }

  # Now find all row numbers that match name, iso, and entity type
  # Do this by pasting name, iso and entity type together and then matching on those
  all3_matching_rows <- which((paste0(dataset$name, dataset$iso,
                                      dataset$entity_type) %in%
                                 paste0(key.dict$right, key.dict$iso,
                                        key.dict$entity_type)))


  # creating the vector of indices (in the dataset) of the names that need to be cleaned
  if (length(all3_matching_rows) != 0) {
    name_ind <<- 1:nrow(dataset)
    name_ind <<- name_ind[-all3_matching_rows]
  } else {
    name_ind <<- 1:nrow(dataset)
  }
  # Helper function returns output that checks if the name is capitalized
  # Change back to capitalized version if the check is true
  if (exists(paste0("isoname"))){
    names(dataset)[grepl("^iso$", names(dataset))] <- isoname
  }
  if (exists(paste0("entity_typename"))){
    names(dataset)[grepl("^entity_type$", names(dataset))] <- entity_typename
  }
  if (exists(paste0("namename"))){
    names(dataset)[grepl("^name$", names(dataset))] <- namename
  }
  return(dataset)
}

#' @export
#' @title Matches country names based on fuzzy matching
#' @description \code{fuzzify_country()} cleans the "country" column in the user's dataset
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
  dataset <- .col_check(dataset, "country", environment())
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
    cat(paste0("The original country is ", dataset$country[i], " (Actor name: ",
               dataset$name[i], ")\n"))
    cat(paste0("Here are some possible matches we found: \n"))
    cat(paste0(1:15, ". ", tmp, "\n"))

    # Let users choose which name they want to keep
    cat("Which name matches? Choose 1-15. (Type N if none match; type S to save your current progress)")
    ans1 <- readline(prompt = "Answer: ")
    ## if one of the listed matches is correct (and not NA), the standardized
    ## version of the matched name will be replaced into the dataset
    # Use grepl to prevent warnings
    if (grepl("^[0-9]{1,2}$", ans1)){
      if (!is.na(ans1) &
          (as.numeric(ans1) %in% 1:15) &
          (!is.na(tmp[as.numeric(ans1)]))) {
        correct.name <- tmp[as.numeric(ans1)]
        correct.iso <- unique(country_keydict$iso[country_keydict$right == correct.name])
        cat(paste0(correct.name, " has been selected and will replace ",
                   dataset$country[i], " in the database.\n\n"))

        # Add ISO

        # replacing all instances of the recently matched (raw) name in the dataset
        # with the standardized name and also add iso
        samecount_inds <- which(dataset$country == origname)

        if (length(samecount_inds) != 0) {
          dataset$country[samecount_inds] <- correct.name
          dataset$iso[samecount_inds] <- correct.iso
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
        cat(paste0("The name (", ans3, ") will be kept in the dataset.\n",
                   "The name has not been added to the key dictionary yet but can be added with the update_country_dict function.\n",
                   "The row number of the custom name has been added to the vector custom_count.\n\n"))
        if (!exists("custom_count")){
          custom_count <<- i
        } else {
          custom_count <<- c(custom_count, i)
        }

        # replacing all instances in the dataset of the original (raw) name
        # with the new custom name
        samecount_inds <- which(dataset$country == origname)

        if (length(samecount_inds) != 0) {
          dataset$country[samecount_inds] <- ans3
          country_ind <<- country_ind[!(country_ind %in% samecount_inds)]
        }
        ### if the user chooses not to enter a custom name, the original name
        ### will be kept, and the index will be added to the unmatched_indices vector
      } else if (toupper(as.character(ans2)) == "N") {
        cat(paste0("The previous name (", origname, ") will be kept.\n\n"))
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
      cat("Sorry, an invalid answer was provided.\n")
      cat(paste0("The previous name (", origname, ") will be kept.",
                 " The index of this entry will be recorded for future inspection.\n\n"))
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
    names(dataset)[grepl("^country$", names(dataset))] <- countryname
  }
  return(dataset)
}

#' @export
#' @title Updates the country dictionary
#' @description Update the country dictionary with the custom countries given by the
#' user and the countries that were not in the country dictionary.
#' @param dataset Dataset by user
#' @param country.dict country dictionary
#' @param custom_count Vector of numbers containing indices where new custom
#' countries added by the user
#' @param unmatched_count Vector of numbers containing indices where countries were
#' unmatched with the country dictionary
#' @return Country dictionary with updated entries
#'
#' @example \dontrun{update_country_dict(df, country_dict, custom_count)}
#'
update_country_dict <- function(dataset, country.dict, custom_count, unmatched_count){
  # Do the usual checks for column name and indices
  dataset <- .col_check(dataset, "country", environment())
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"country\" columns.")
  }
  if (!exists(".count_updates")){
    stop(paste("Stopping function. There are either no new updates to the country dictionary",
               "or you have not used the fuzzify_country function yet."))
  }
  # First update those that have custom inputs
  if (!missing(custom_count)){
    cust_df <- data.frame(wrong = .count_updates$name[.count_updates$ind %in% custom_count],
                          right = NA,
                          iso = NA,
                          region = NA,
                          Landarea = NA,
                          iso2 = NA,
                          Population = NA)
    # Now remove the names that have custom inputs
    .count_updates <- .count_updates[-which(.count_updates$ind %in% custom_count), ]
  }
  if (!missing(unmatched_count)){
    # Include and remove those that have not been cleaned as well
    unmatched_df <- data.frame(wrong = .count_updates$name[.count_updates$ind %in% unmatched_count],
                               right = NA,
                               iso = NA,
                               region = NA,
                               Landarea = NA,
                               iso2 = NA,
                               Population = NA)
    .count_updates <- .count_updates[-which(.count_updates$ind %in% unmatched_count), ]
  }
  .count_updates$right <- dataset$country[.count_updates$ind]
  update_dict <- data.frame(wrong = .count_updates$name,
                            right = .count_updates$right,
                            iso = NA,
                            region = NA,
                            Landarea = NA,
                            iso2 = NA,
                            Population = NA)

  if (exists("cust_df") & exists("unmatched_df")){
    country.dict <- rbind(country.dict, update_dict, cust_df, unmatched_df)
  } else if (exists("cust_df")) {
    country.dict <- rbind(country.dict, update_dict, cust_df)
  } else if (exists("unmatched_df")){
    country.dict <- rbind(country.dict, update_dict, unmatched_df)
  } else {
    country.dict <- rbind(country.dict, update_dict)
  }
  country.dict <- country.dict %>%
    dplyr::group_by(right) %>%
    tidyr::fill(iso:Population, .direction = "updown")
  return(as.data.frame(country.dict, stringsAsFactors = F))
  if (exists(paste0("countryname"))){
    names(dataset)[grepl("^country$", names(dataset))] <- countryname
  }
}


#' @export
#' @title Cleans name using phonetic matching
#' @description \code{phonetify_names()} searches for actors' names which are not standardized
#' and standardizes them according to the key dictionary. Users get to choose which name
#' is correct based on a selection narrowed down by using phonetic matching. Users can
#' choose to input custom names should the names not be in the key dictionary.
#'
#' @details A combination of 5 different phonetic representations (Metaphone, Nysiis modified,
#' Onca modified refined, Phonex, Roger Root) is used in tandem with a variety of string
#' distance metrics (Full Damerau-Levenshtein distance, q-gram distance,
#' cosine distance (between q-gram profiles), Jaccard distance between (q-gram profiles),
#' and Jaro-Winker distance) to get an accurate match of the actor's name within the
#' supplied key dictionary.
#'
#' @param dataset Dataset containing actors by user
#' @param key.dict Key dictionary to clean actors' names against
#'
#' @return Cleaned dataset with actors names standardized against the key dictionary.
#' @return A few vectors of indices will also be created to store the indices of those names
#' that needs to be matched. The first is a vector of indices of all actors that
#' require cleaning. \code{unmatched_indices} is a vector of indices of names
#' not cleaned by the function. \code{custom_indices} is a vector of indices
#' denoting names for which custom actor names are given by the user, and will be
#' used to update the key dictionary.
phonetify_names <- function(dataset, key.dict) {

  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name", environment())
  dataset <- .col_check(dataset, "entity_type", environment())
  dataset <- .col_check(dataset, "iso", environment())
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity_type\", or \"iso\" columns.")
  }
  # creating a vector of indices (in the dataset) of the names that need to be fuzzy matched

  # dataset.tmp <- dataset[!duplicated(paste0(dataset$name,
  # dataset$iso, dataset$entity_type)), ]

  # all3_short <- which((paste0(dataset.tmp$name, dataset.tmp$iso,
  # dataset.tmp$entity_type) %in%
  # paste0(key.dict$right, key.dict$iso,
  # key.dict$entity_type)))
  ## finding all the row numbers in which the dataset matches the key dictionary
  all3_matching_rows <- which((paste0(dataset$name, dataset$iso,
                                      dataset$entity_type) %in%
                                 paste0(key.dict$right, key.dict$iso,
                                        key.dict$entity_type)))

  ## creating the vector of indices (in the dataset) of the names that need to be fuzzy matched
  if (!exists("name_ind", where = globalenv())){
    if (length(all3_matching_rows) != 0) {
      name_ind <<- 1:nrow(dataset)
      name_ind <<- name_ind[-all3_matching_rows]
    } else {
      name_ind <<- 1:nrow(dataset)
    }
  }

  ind.short <- name_ind[!duplicated(dataset$name[name_ind])]
  if (length(ind.short) == 0){
    stop(paste0("Please check your dataset and/or the key dictionary to make sure all the",
                "column names are correct (name, iso, entity_type). Otherwise, it seems that" ,
                "all the names in the dataset has already been matched with the key dictionary."))
  }

  # creating a vector that--during the function--tracks which indices aren't
  # fuzzy matched by the end of the whole function
  if (exists("unmatched_indices")) {
    if (all(unmatched_indices[!is.na(unmatched_indices)] <= nrow(dataset))) {
      cat("A vector that seems to contain valid indices that have not been matched (named unmatched_indices) already exists. \n Would you like to proceed using this vector? (Y/N)")
      ans_u1 <- readline(prompt = "Answer: ")
      if (substr(toupper(as.character(ans_u1)), 1, 1) == "Y") {
        cat("Function will proceed using this vector\n")
        ind.short <- ind.short[!(ind.short %in% unmatched_indices)]
      } else if (substr(toupper(as.character(ans_u1)), 1, 1) == "N") {
        cat("Would you like to start with with cleaning the dataset anew (Y/N)? (Your progress might be lost)")
        ans_u11 <- readline(prompt = "Answer: ")
        if (substr(toupper(as.character(ans_u11)), 1, 1) != "Y") {
          stop("We'll stop the function here to let you decide what to do with the vector.\n")
        }
      } else {
        cat("An invalid answer was provided, but we'll continue with this vector for now. \n")
        ind.short <- ind.short[!(ind.short %in% unmatched_indices)]
      }
    } else if (any(unmatched_indices[!is.na(unmatched_indices)] > nrow(dataset))) {
      cat(paste0("A vector of indices named unmatched_indices already exists and contains invalid indices (e.g. values beyond the row numbers in the dataset or characters). \n",
                 "Please check the vector to be sure that:",
                 "\n1. It is a vector of indices from the dataset that have not been fuzzy matched and",
                 "\n2. Each element in the vector is a possible row number from the dataset (within the range of the total # of rows)",
                 "\nWould you like the function to proceed with this vector anyways (future edits may be difficult)? (Y/N). "))
      ans_u2 <- readline(prompt = "Answer: ")
      if (substr(toupper(as.character(ans_u2)), 1, 1) == "Y") {
        cat("Function will proceed using this vector\n")
        ind.short <- ind.short[!(ind.short %in% unmatched_indices)]
      } else if (substr(toupper(as.character(ans_u2)), 1, 1) == "N") {
        cat("Would you like to start with with cleaning the dataset anew (Y/N)? (Your progress might be lost)")
        ans_u21 <- readline(prompt = "Answer: ")
        if (substr(toupper(as.character(ans_u21)), 1, 1) != "Y") {
          stop("We'll stop the function here to let you decide what to do with the vector.\n")
        }
      } else {
        cat("An invalid answer was provided, but we'll continue with this vector for now.\n")
        ind.short <- ind.short[!(ind.short %in% unmatched_indices)]
      }
    } else {
      cat(paste0("A vector of indices named unmatched_indices already exists, but there might ",
                 "be something wrong with it.\n", "The function will continue with it for now.\n"))
      ind.short <- ind.short[!(ind.short %in% unmatched_indices)]
    }
  }


  # creating a vector that--during the function--tracks which indices received custom names
  # to make it easier for the user to look back at the name later and fix, if necessary
  # note to readers: the code below is the exact same as the code to create the unmatched_indices vector,
  # except the name of the vector is different
  if (exists("custom_indices")) {
    if (all(custom_indices[!is.na(custom_indices)] <= nrow(dataset))) {
      cat("A vector that seems to contain valid indices that have custom names (named custom_indices) already exists.\nWould you like to proceed using this vector? (Y/N) \n")
      ans_c1 <- readline(prompt = "Answer: ")
      if (substr(toupper(as.character(ans_c1)), 1, 1) == "Y") {
        cat("Function will proceed using this vector\n")
      } else if (substr(toupper(as.character(ans_c1)), 1, 1) == "N") {
        cat("Would you like to start with with cleaning the dataset anew (Y/N)? (Your progress might be lost)\n")
        ans_c11 <- readline(prompt = "Answer: ")
        if (substr(toupper(as.character(ans_c11)), 1, 1) != "Y") {
          stop("We'll stop the function here to let you decide what to do with the vector.")
        }
      } else {
        cat("An invalid answer was provided, but we'll continue with this vector for now.\n")
      }
    } else if (any(custom_indices[!is.na(custom_indices)] > nrow(dataset))) {
      cat("A vector of indices named custom_indices already exists and contains invalid indices (e.g. values beyond the row numbers in the dataset or characters).\n

Please check the vector to be sure that:
    \n1. It is a vector of indices from the dataset that have not been fuzzy matched and
    \n2. Each element in the vector is a possible row number from the dataset (within the range of the total # of rows)

\nWould you like the function to proceed with this vector anyways (future edits may be difficult)? (Y/N). ")
      ans_c2 <- readline(prompt = "Answer: ")
      if (substr(toupper(as.character(ans_c2)), 1, 1) == "Y") {
        cat("Function will proceed using this vector\n")
        ind.short <- ind.short[!(ind.short %in% custom_indices)]
      } else if (substr(toupper(as.character(ans_c2)), 1, 1) == "N") {
        cat("Would you like to start with with cleaning the dataset anew (Y/N)? (Your progress might be lost)\n")
        ans_c21 <- readline(prompt = "Answer: ")
        if (substr(toupper(as.character(ans_c21)), 1, 1) != "Y") {
          stop("We'll stop the function here to let you decide what to do with the vector.")
        }
      } else {
        cat("An invalid answer was provided, but we'll continue with this vector for now.\n")
        ind.short <- ind.short[!(ind.short %in% custom_indices)]
      }
    } else {
      cat(paste0("A vector of indices named custom_indices already exists, but there might be something wrong with it.\n",
                 "The function will continue with it for now.\n"))
      ind.short <- ind.short[!(ind.short %in% custom_indices)]
    }
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
    kd.filtered <- key.dict %>% dplyr::filter(iso == dataset$iso[ind] & entity_type == dataset$entity_type[ind])
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
    kd.filtered <- kd.filtered[!duplicated(kd.filtered$right), ]

    # keeping track of the original name so that all of the same raw names can be changed simultaneously
    origname <- dataset$name[ind]
    cat(paste0("Currently cleaning actor ", i, " of ", length(ind.short), " (",
               round((i-1)/length(ind.short) * 100, 1), "%) completed.\n"))

    # Check if there were any matches
    if (nrow(kd.filtered) == 0){
      cat(paste0("There seems to be no available matches for ",
                 dataset$name[ind], "; iso = ", dataset$iso[ind],
                 " entity type = ", dataset$entity_type[ind],
                 ".\n", "Would you like to input a custom name (Y/N)?\n"))
      ansn1 <- readline(prompt = "Answer: ")
      if (substr(toupper(as.character(ansn1)), 1, 1) == "Y") {
        ansn2 <- readline(prompt = "Enter in custom name: ")
        ansn2 <- as.character(ansn2)
        cat(paste0("The name (", ansn2, ") will be kept in the dataset.\n",
                   "The name has not been added to the key dictionary yet but can be added with the update_key_dict function.\n",
                   "The row number of the custom name has been added to a vector called custom_indices.\n\n"))
        if (!exists("custom_indices")){
          custom_indices <<- ind
        } else {
          custom_indices <<- c(custom_indices, ind)
        }

        # replacing all instances in the dataset of the original (raw) name
        # with the new custom name
        samename_inds <- which(dataset$name == origname & dataset$iso == dataset$iso[ind] & dataset$entity_type == dataset$entity_type[ind])

        if (length(samename_inds) != 0) {
          dataset$name[samename_inds] <- ansn2
          name_ind <<- name_ind[!(name_ind %in% samename_inds)]
        }
        next
        ### if the user chooses not to enter a custom name, the original name
        ### will be kept, and the index will be added to the unmatched_indices vector
      } else if (substr(toupper(as.character(ansn1)), 1, 1) == "N") {
        cat(paste0("The previous name (", origname, ") will be kept.\n\n"))
        if (!exists("unmatched_indices")){
          unmatched_indices <<- ind
        } else{
          unmatched_indices <<- c(unmatched_indices, ind)
        }
        next
      } else {
        ## if the user makes a typo or other invalid answer, the function will continue, and
        ## the index will be added to the unmatched_indices vector for the user to look at later
        ## (or restart the function again later)
        cat("Sorry, an invalid answer was provided.\n")
        cat(paste0("The previous name (", origname, ") will be kept.",
                   " The index of this entry will be recorded for future inspection.\n\n"))
        if (!exists("unmatched_indices")){
          unmatched_indices <<- ind
        } else {
          unmatched_indices <<- c(unmatched_indices, ind)
        }
        next
      }
    }


    # listing the top 15 best-scoring fuzzy-matched names
    cat(paste0("The original name is ", dataset$name[ind], " with iso code ",
               dataset$iso[ind], " and entity type ", dataset$entity_type[ind], "\n"))
    cat(paste0("Here are some possible matches we found: \n"))
    if (nrow(kd.filtered) < 15){
      for (k in seq_len(nrow(kd.filtered))){
        if (is.na(kd.filtered$right[k])){
          next
        } else if (k < 10){
          cat(paste0(k, ":  ", kd.filtered$right[k], "; iso: ", kd.filtered$iso[k],
                     " and entity type: ", kd.filtered$entity_type[k], "\n"))
        } else {
          cat(paste0(k, ": ", kd.filtered$right[k], "; iso: ", kd.filtered$iso[k],
                     " and entity type: ", kd.filtered$entity_type[k], "\n"))
        }
      }
    } else {
      for (k in 1:15) {
        if (is.na(kd.filtered$right[k])) {
          next
        } else if (k < 10) {
          cat(paste0(k, ":  ", kd.filtered$right[k], "; iso: ", kd.filtered$iso[k],
                     " and entity type: ", kd.filtered$entity_type[k], "\n"))
        } else {
          cat(paste0(k, ": ", kd.filtered$right[k], "; iso: ", kd.filtered$iso[k],
                     " and entity type: ", kd.filtered$entity_type[k], "\n"))
        }
      }
    }


    # interactive matching where user matches one of the fuzzy matches--
    # the right version of the matched name will replace the dataset's name
    # if there is no right version of the fuzzy matched options, the original name (in the dataset) will be kept
    # two vectors will be updated:
    # one (unmatched_indices) tracks all indices with unchanged/unmatched names and
    # the other vector (custom_indices) tracks all the indices that had custom names

    cat("Which name matches? Choose 1-15. (Type N if none match; type S to save your current progress; type C to enter a custom name) \n")
    ans1 <- readline(prompt = "Answer: ")
    ## if one of the listed matches is correct (and not NA), the standardized
    ## version of the matched name will be replaced into the dataset
    if (grepl("^[0-9]{1,2}$", ans1)){
      tmpans <- ans1
      while (as.numeric(tmpans) > 15 | as.numeric(tmpans) > nrow(kd.filtered)) {
        cat("It seems like you have entered a number that is out of bounds. Please enter another number.\n")
        tmpans <- readline(prompt = "Answer: ")
        if (!grepl("^[0-9]{1,2}$", tmpans)){
          cat("Sorry, an invalid answer was provided.\n")
          cat(paste0("The previous name (", origname, ") will be kept.",
                     " The index of this entry will be recorded for future inspection.\n\n"))
          if (!exists("unmatched_indices")){
            unmatched_indices <<- ind
          } else {
            unmatched_indices <<- c(unmatched_indices, ind)
          }
          break
        }
      }
      if (!is.na(ans1) &
          (as.numeric(ans1) %in% (1:15)) &
          (!is.na(kd.filtered$right[as.numeric(ans1)]))) {
        correct.name <- kd.filtered$right[as.numeric(ans1)]
        cat(paste0(correct.name, " has been selected and will replace ", dataset$name[ind],
                   " in the database. \n\n"))

        # replacing all instances of the recently matched (raw) name in the dataset
        # with the standardized name
        samename_inds <- which(dataset$name == origname & dataset$iso == dataset$iso[ind] & dataset$entity_type == dataset$entity_type[ind])

        if (length(samename_inds) != 0) {
          dataset$name[samename_inds] <- correct.name
          name_ind <<- name_ind[!(name_ind %in% samename_inds)]
        }
        ## if the user response was numeric but not one of the listed matches,
        ## the original name will be kept and that index will be added to the unmatched_indices vector
        ## if none of the listed matches are correct, the user can either enter a
        ## custom name or choose not to do anything for the time being
      }
    } else if (substr(toupper(as.character(ans1)), 1, 1) == "S") {
      cat("Your current progress will be returned.\n")
      cat("If it's not saved, be sure you saved your results in a variable with an assign function\n")
      return(dataset)

    } else if (substr(toupper(as.character(ans1)), 1, 1) == "N") {
      ### if the user chooses that none of the names match, the original name
      ### will be kept, and the index will be added to the unmatched_indices vector
      cat(paste0("The previous name (", origname, ") will be kept.\n\n"))
      if (!exists("unmatched_indices")){
        unmatched_indices <<- ind
      } else{
        unmatched_indices <<- c(unmatched_indices, ind)
      }
    } else if (substr(toupper(as.character(ans1)), 1, 1) == "C") {
      ### if the user enters a custom name, the name will be replaced in the dataset,
      ### and the index will be added to the custom_indices vector to make it easier
      ### for the user to look back at the name later and fix, if necessary
      ### if the user wants to add the custom names to the key dictionary,
      ### the update_key_dict function can be used
      ans3 <- readline(prompt = "Enter in custom name: ")
      ans3 <- as.character(ans3)
      cat(paste0("The name (", ans3, ") will be kept in the dataset.\n",
                 "The name has not been added to the key dictionary yet but can be added with the update_key_dict function.\n",
                 "The row number of the custom name has been added to a vector called custom_indices.\n\n"))
      if (!exists("custom_indices")){
        custom_indices <<- ind
      } else {
        custom_indices <<- c(custom_indices, ind)
      }

      # replacing all instances in the dataset of the original (raw) name
      # with the new custom name
      samename_inds <- which(dataset$name == origname & dataset$iso == dataset$iso[ind] & dataset$entity_type == dataset$entity_type[ind])

      if (length(samename_inds) != 0) {
        dataset$name[samename_inds] <- ans3
        name_ind <<- name_ind[!(name_ind %in% samename_inds)]
      }
    } else {
      ## if the user makes a typo or other invalid answer, the function will continue, and
      ## the index will be added to the unmatched_indices vector for the user to look at later
      ## (or restart the function again later)
      cat("Sorry, an invalid answer was provided.\n")
      cat(paste0("The previous name (", origname, ") will be kept.",
                 " The index of this entry will be recorded for future inspection.\n\n"))
      if (!exists("unmatched_indices")){
        unmatched_indices <<- ind
      } else {
        unmatched_indices <<- c(unmatched_indices, ind)
      }
    }
  }

  if (exists(paste0("isoname"))){
    names(dataset)[grepl("^iso$", names(dataset))] <- isoname
  }
  if (exists(paste0("entity_typename"))){
    names(dataset)[grepl("^entity_type$", names(dataset))] <- entity_typename
  }
  if (exists(paste0("namename"))){
    names(dataset)[grepl("^name$", names(dataset))] <- namename
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
#' @param custom_indices Vector of numbers containing which indices where new custom names added by the user
#' @param unmatched_indices Vector of numbers containing indices where names in the dataset were unmatched
#' in the key dictionary
#' @return Key dictionary with updated entries, and their respective phonetic
#' representations
#'
#' @example \dontrun{update_key_dict(df, key_dict, custom_indices)}
update_key_dict <- function(dataset, key.dict, custom_indices, unmatched_indices) {
  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name", environment())
  dataset <- .col_check(dataset, "entity_type", environment())
  dataset <- .col_check(dataset, "iso", environment())
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity_type\", or \"iso\" columns.")
  }
  if (!exists(".actor_updates")){
    stop(paste("Stopping function. There are either no new updates to the key dictionary",
               "or you have not used the phonetify_names function yet."))
  }
  # Get the complete dataset of those that needs to be updated first
  .actor_updates <- cbind(.actor_updates, dataset[.actor_updates$ind, ])
  # Update the custom names first
  if (!missing(custom_indices)){
    custom <- .actor_updates$ind %in% custom_indices
    cust_df <- data.frame(right = NA,
                          wrong = .actor_updates$name[custom],
                          iso = .actor_updates$iso[custom],
                          entity_type = .actor_updates$entity_type[custom],
                          coerced_wrong = .coerce_location_names(.actor_updates$name_wrong[custom]),
                          caverphone = phonics::caverphone(.actor_updates$name[custom], clean = FALSE),
                          caverphone.modified = phonics::caverphone(.actor_updates$name[custom], modified = TRUE, clean = FALSE),
                          cologne = phonics::cologne(.actor_updates$name[custom], clean = FALSE),
                          lein = phonics::lein(.actor_updates$name[custom], clean = FALSE),
                          metaphone = phonics::metaphone(.actor_updates$name[custom], clean = FALSE),
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
    # Remove the custom names
    .actor_updates <- .actor_updates[-which(.actor_updates$ind %in% custom_indices), ]
  }
  # Now update and remove the unmatched ones
  if (!missing(unmatched_indices)){
    unmatched <- .actor_updates$ind %in% unmatched_indices
    unmatched_df <- data.frame(right = NA,
                               wrong = .actor_updates$name[unmatched],
                               iso = .actor_updates$iso[unmatched],
                               entity_type = .actor_updates$entity_type[unmatched],
                               coerced_wrong = .coerce_location_names(.actor_updates$name_wrong[unmatched]),
                               caverphone = phonics::caverphone(.actor_updates$name[unmatched], clean = FALSE),
                               caverphone.modified = phonics::caverphone(.actor_updates$name[unmatched], modified = TRUE, clean = FALSE),
                               cologne = phonics::cologne(.actor_updates$name[unmatched], clean = FALSE),
                               lein = phonics::lein(.actor_updates$name[unmatched], clean = FALSE),
                               metaphone = phonics::metaphone(.actor_updates$name[unmatched], clean = FALSE),
                               nysiis = phonics::nysiis(.actor_updates$name[unmatched], clean = FALSE),
                               nysiis.modified = phonics::nysiis(.actor_updates$name[unmatched], modified = TRUE, clean = FALSE),
                               onca = phonics::onca(.actor_updates$name[unmatched], clean = FALSE),
                               onca.modified = phonics::onca(.actor_updates$name[unmatched], modified = TRUE, clean = FALSE),
                               onca.refined = phonics::onca(.actor_updates$name[unmatched], refined = TRUE, clean = FALSE),
                               onca.modified.refined = phonics::onca(.actor_updates$name[unmatched], modified = TRUE, refined = TRUE, clean = FALSE),
                               phonex = phonics::phonex(.actor_updates$name[unmatched], clean = FALSE),
                               rogerroot = phonics::rogerroot(.actor_updates$name[unmatched], clean = FALSE),
                               soundex = phonics::soundex(.actor_updates$name[unmatched], clean = FALSE),
                               soundex.refined = phonics::refinedSoundex(.actor_updates$name[unmatched], clean = FALSE),
                               statcan = phonics::statcan(.actor_updates$name[unmatched], clean = FALSE))
    # Remove the unmatched names
    .actor_updates <- .actor_updates[-which(.actor_updates$ind %in% unmatched_indices), ]
  }
  # Now update the rest
  # Create the rows to be binded to key.dict
  if (exists("name_ind") & length(name_ind) != 0){
    .actor_updates <- .actor_updates[!.actor_updates$ind %in% name_ind, ]
  }
  if (nrow(.actor_updates) != 0){
    update_df <- data.frame(right = .actor_updates$name,
                            wrong = .actor_updates$name_wrong,
                            iso = .actor_updates$iso,
                            entity_type = .actor_updates$entity_type,
                            coerced_wrong = .coerce_location_names(.actor_updates$name_wrong),
                            caverphone = phonics::caverphone(.actor_updates$name_wrong, clean = FALSE),
                            caverphone.modified = phonics::caverphone(.actor_updates$name_wrong, modified = TRUE, clean = FALSE),
                            cologne = phonics::cologne(.actor_updates$name_wrong, clean = FALSE),
                            lein = phonics::lein(.actor_updates$name_wrong, clean = FALSE),
                            metaphone = phonics::metaphone(.actor_updates$name_wrong, clean = FALSE),
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

  }
  # Bind rows to key.dict
  # Check for whether dataframes exist first or not
  if (exists("cust_df") & exists("unmatched_df") & exists("update_df")){
    key.dict <- rbind(key.dict, update_df, cust_df, unmatched_df)
  } else if (exists("cust_df") & exists("update_df")){
    key.dict <- rbind(key.dict, update_df, cust_df)
  } else if (exists("unmatched_df") & exists("update_df")){
    key.dict <- rbind(key.dict, update_df, unmatched_df)
  } else if (exists("update_df")){
    key.dict <- rbind(key.dict, update_df)
  } else if (exists("unmatched_df") & exists("cust_df")){
    key.dict <- rbind(key.dict, unmatched_df, cust_df)
  } else if (exists("cust_df")){
    key.dict <- rbind(key.dict, cust_df)
  } else if (exists("unmatched_df")){
    key.dict <- rbind(key.dict, unmatched_df)
  }
  if (exists(paste0("isoname"))){
    names(dataset)[grepl("^iso$", names(dataset))] <- isoname
  }
  if (exists(paste0("entity_typename"))){
    names(dataset)[grepl("^entity_type$", names(dataset))] <- entity_typename
  }
  if (exists(paste0("namename"))){
    names(dataset)[grepl("^name$", names(dataset))] <- namename
  }
  return(key.dict)
}

#' @export
#' @title Merge contextual data into original dataset
#' @description Contextual data from the contextuals database obtained from a variety
#' of sources. Contextual information includes region, population, latitude,
#' longitude, area, elevation, and the initiatives committed by the actor. Merging is done
#' based on actors' name, iso, and entity type.
#' @param dataset Original dataset
#' @param contextual_df Contextuals database included in the package. Called
#' using `r contextuals`
#' @param context Vector consisting of contextual column name to be merged
#' into the dataset includes "region", "population", "population_year", "lat",
#' "lng", "area", "area_units", "intiatives_committed", "num_commit", "state".
#' Defaults to taking all the contextual information.
#' @return Dataset with contextuals merged
#'
#' @example \dontrun{contextualize_data(df, contextuals_df, c("population", "region", "lat", "lng"))}
contextualize_data <- function(dataset, contextual_df, context = names(contextual_df)){
  # Check for column naming using helper function
  dataset <- .col_check(dataset, "name", environment())
  dataset <- .col_check(dataset, "entity_type", environment())
  dataset <- .col_check(dataset, "iso", environment())
  if (exists("to.stop")){
    stop("Stopping function. Missing the \"name\", \"entity_type\", or \"iso\" columns.")
  }
  # Incorporate check to make sure that the contextuals for merging are in the
  # contextual dataframe
  if (any(!context %in% names(contextual_df))){
    stop("The context argument needs to match the column names in the contextual
         dataframe. See ?contextualize_data for a list of applicable column names.")
  }
  if (paste0(context, collapse = ";") != paste0(names(contextual_df), collapse = ";")){
    context <- c(context, "name", "iso", "entity_type")
  }
  # Merge and keep all of the original dataset's data
  merge_df <- dplyr::left_join(dataset, contextual_df[ , context],
                    by = c("name", "iso", "entity_type"))
  if (exists(paste0("isoname"))){
    names(merge_df)[grepl("^iso$", names(merge_df))] <- isoname
  }
  if (exists(paste0("entity_typename"))){
    names(merge_df)[grepl("^entity_type$", names(merge_df))] <- entity_typename
  }
  if (exists(paste0("namename"))){
    names(merge_df)[grepl("^name$", names(merge_df))] <- namename
  }
  return(merge_df)
}

