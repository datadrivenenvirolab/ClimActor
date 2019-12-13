# Data-Driven Lab
# Functions used for cleaning and fuzzy matching of actors' names
# Need to import the pipe '%>%' function for use in one of our functions later on

#' @importFrom dplyr %>%

#'
#' @title Cleans name using exact matches
#' @description Cleans the datasets' actors names based on the key dictionary using
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


#' Cleans name using phonetic matching
#' @description \code{fuzzify_names} searches for actors' names which are not standardized
#' and standardizes them according to the key dictionary. Users get to choose which name
#' is correct based on a selection narrowed down by using phonetic matching. Users can
#' choose to input custom names should the names not be in the key dictionary.
#' @param dataset Dataset containing actors by user
#' @param key.dict Key dictionary to clean actors' names against
#' @return Cleaned dataset with actors names standardized against the key dictionary.
#' @return 2 vectors of indices will also be created to store the indices of those names
#' @return that needs to be matched.
fuzzify_names <- function(dataset, key.dict) {

  # creating a vector of indices (in the dataset) of the names that need to be fuzzy matched
  dataset.tmp <- dataset[!duplicated(paste0(dataset$name,
                                            dataset$iso, dataset$entity.type)), ]

  ## finding all the row numbers in which the dataset matches the key dictionary
  all3_matching_rows <- which((paste0(dataset.tmp$name, dataset.tmp$iso,
                                      dataset.tmp$entity.type) %in%
                                 paste0(key.dict$right, key.dict$iso,
                                        key.dict$entity.type)))

  ## creating the vector of indices (in the dataset) of the names that need to be fuzzy matched
  if (length(all3_matching_rows) != 0) {
    indices <<- 1:nrow(dataset.tmp)
    indices <<- indices[-all3_matching_rows]
  } else {
    indices <<- 1:nrow(dataset.tmp)
  }

  # creating a vector that--during the function--tracks which indices aren't
  # fuzzy matched by the end of the whole function
  if (!exists("unmatched_indices")) {
    unmatched_indices <<- rep(NA, length(indices))
  } else if (length(unmatched_indices) != length(indices)) {
    unmatched_indices <<- append(unmatched_indices, rep(NA, (length(indices) - length(unmatched_indices))))
  } else if ((length(unmatched_indices) == length(indices)) &
             (sum(unmatched_indices[!is.na(unmatched_indices)] > nrow(dataset))) == 0) {
    cat("A vector that seems to contain valid indices that have not been matched (named unmatched_indices) already exists.

        Would you like to proceed using this vector? (Y/N)")
    ans_u1 <- readline(prompt = "Answer: ")
    if (substr(toupper(as.character(ans_u1)), 1, 1) == "Y") {
      print("Function will proceed using this vector :)")
    } else if (substr(toupper(as.character(ans_u1)), 1, 1) == "N") {
      stop("We'll stop the function here to let you decide what to do with the vector.")
    } else {
      print("An invalid answer was provided, but we'll continue with this vector for now.
            Feel free to stop the function if this is not what you want.")
    }
  } else if ((length(unmatched_indices) == length(indices)) &
             (sum(unmatched_indices[!is.na(unmatched_indices)] > nrow(dataset))) > 0) {
    cat("A vector of indices named unmatched_indices already exists and contains invalid indices (e.g. values beyond the row numbers in the dataset or characters).

Please check the vector to be sure that:
    1. It is a vector of indices from the dataset that have not been fuzzy matched and
    2. Each element in the vector is a possible row number from the dataset (within the range of the total # of rows)

Would you like the function to proceed with this vector anyways (future edits may be difficult)? (Y/N). ")
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
  } else if (length(custom_indices) != length(indices)) {
    custom_indices <<- append(custom_indices, rep(NA, (length(indices) - length(custom_indices))))
  } else if ((length(custom_indices) == length(indices)) &
             (sum(custom_indices[!is.na(custom_indices)] > nrow(dataset)) == 0)) {
    cat("A vector that seems to contain valid indices that have custom names (named custom_indices) already exists.

        Would you like to proceed using this vector? (Y/N) ")
    ans_c1 <- readline(prompt = "Answer: ")
    if (substr(toupper(as.character(ans_c1)), 1, 1) == "Y") {
      print("Function will proceed using this vector :)")
    } else if (substr(toupper(as.character(ans_c1)), 1, 1) == "N") {
      stop("We'll stop the function here to let you decide what to do with the vector.")
    } else {
      print("An invalid answer was provided, but we'll continue with this vector for now.
            Feel free to stop the function if this is not what you want.")
    }
  } else if ((length(custom_indices) == length(indices)) &
             (sum(custom_indices[!is.na(custom_indices)] > nrow(dataset)) > 0)) {
    cat("A vector of indices named custom_indices already exists and contains invalid indices (e.g. values beyond the row numbers in the dataset or characters).

Please check the vector to be sure that:
    1. It is a vector of indices from the dataset that have not been fuzzy matched and
    2. Each element in the vector is a possible row number from the dataset (within the range of the total # of rows)

Would you like the function to proceed with this vector anyways (future edits may be difficult)? (Y/N). ")
    ans_c2 <- readline(prompt = "Answer: ")
    if (substr(toupper(as.character(ans_c2)), 1, 1) == "Y") {
      print("Function will proceed using this vector :)")
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
  ds.phon.codes <- phonics::phonics(dataset$name[indices], c("caverphone", "caverphone.modified",
                                                             "cologne", "lein", "metaphone",
                                                             "nysiis", "nysiis.modified", "onca",
                                                             "onca.modified", "onca.refined",
                                                             "onca.modified.refined", "phonex",
                                                             "rogerroot", "soundex",
                                                             "soundex.refined", "statcan"), clean = FALSE)

  ds.phon.codes$rogerroot <- as.character(ds.phon.codes$rogerroot)
  # starting the fuzzy matching process:

  for (i in seq_along(indices)) {

    # collecting all scores from all phonetic algorithm/edit distance methods
    # for all names in key.dict$wrong

    ind <- indices[i]

    ## filtering out only the rows of the key dictionary that have a matching
    ## iso and entity type as the actor that is currently being fuzzy matched
    kd.filtered <- key.dict %>% dplyr::filter(iso == dataset$iso[ind] & entity.type == dataset$entity.type[ind])
    kd.filtered$rogerroot <- as.character(kd.filtered$rogerroot)

    ## calculating scores using the caverphone phonetic algorithm
    ## quantification of scores is based on 9 edit distance methods

    kd.filtered <- kd.filtered %>% mutate(metaphone.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$metaphone[i],
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

    # calculating the final total score:


    # arranging by total score in descending order so we can list the top matches (based on those scores)
    kd.filtered <- kd.filtered %>% dplyr::arrange(desc(tot.phon.score))

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
    origname <- dataset.tmp$name[ind]

    # interactive matching where user matches one of the fuzzy matches--
    # the right version of the matched name will replace the dataset's name
    # if there is no right version of the fuzzy matched options, the original name (in the dataset) will be kept
    # two vectors will be updated:
    # one (unmatched_indices) tracks all indices with unchanged/unmatched names and
    # the other vector (custom_indices) tracks all the indices that had custom names

    cat("Which name matches? Choose 1-15. (Type N if none match; type S to save your current progress)")
    ans1 <- readline(prompt = "Answer: ")
    ans1 <- as.numeric(ans1)
    correct.name <- kd.filtered$right[ans1]
    ## if one of the listed matches is correct (and not NA), the standardized
    ## version of the matched name will be replaced into the dataset
    if (!is.na(ans1) &
        (ans1 %in% c(1:15)) &
        (!is.na(kd.filtered$right[ans1]))) {
      print(paste0("The standardized name for ", kd.filtered$wrong[ans1],
                   " is ", correct.name))
      print(paste0(dataset$name[ind], " will be kept in the dataset."))

      # replacing all instances of the recently matched (raw) name in the dataset
      # with the standardized name
      samename_inds <- which(dataset$name == origname)

      if (length(samename_inds) != 0) {
        dataset$name[samename_inds] <- correct.name
      }
      ## if the user response was numeric but not one of the listed matches,
      ## the original name will be kept and that index will be added to the unmatched_indices vector
    } else if (!is.na(ans1)) {
      #& (as.numeric(ans1) > 15 | as.numeric(ans1) < 1)
      print("A number outside of the range of choices has been selected.")
      print(paste0("We'll keep the original name: ", origname, " in the dataset and add this index to unmatched_indices."))
      unmatched_indices[i] <<- ind

      ## if none of the listed matches are correct, the user can either enter a
      ## custom name or choose not to do anything for the time being
    } else if (substr(toupper(as.character(ans1)), 1, 1) == "S") {
      cat("Your current progress will be returned.")
      cat("If it's not saved, be sure you saved your results in a variable with an assign function")
      return(dataset)

    } else if (substr(toupper(as.character(ans1)), 1, 1) == "N") {
      print(paste0("The previous name (", origname, ") will be kept."))

      cat("Do you want to enter a custom name instead? (Y/N)")
      ans2 <- readline(prompt = "Answer: ")

      ### if the user enters a custom name, the name will be replaced in the dataset,
      ### and the index will be added to the custom_indices vector to make it easier
      ### for the user to look back at the name later and fix, if necessary
      ### if the user wants to add the custom names to the key dictionary,
      ### the update_key_dict function can be used :)
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
        }
        ### if the user chooses not to enter a custom name, the original name
        ### will be kept, and the index will be added to the unmatched_indices vector
      } else if (substr(toupper(as.character(ans2)), 1, 1) == "N") {
        print(paste0("We'll leave this name as it is: ", origname))
        unmatched_indices[i] <<- ind
      }
      ## if the user makes a typo or other invalid answer, the function will continue, and
      ## the index will be added to the unmatched_indices vector for the user to look at later
      ## (or restart the function again later)
    } else {
      print("Sorry, an invalid answer was provided.
             Please try the function again after this run-through is over")
      print(paste0("The previous name (", origname, ") will be kept."))
      unmatched_indices[i] <<- ind
    }
  }
  # making these vectors more readable by getting rid of NAs
  custom_indices <<- custom_indices[!is.na(custom_indices)]
  unmatched_indices <<- unmatched_indices[!is.na(unmatched_indices)]

  # the final (hopefully cleaner) dataset!
  return(dataset)
}

#' Update key dictionary
#' @description Update the key dictionary with the new inputs from user.
#' @param dataset Dataset by user
#' @param key.dict Key dictionary
#' @param custom_indices Vector of numbers containing which indices were new custom names added by the user
#' @return Key dictionary with updated entries
#' @example update_key_dict(df, key_dict, custom_indices)
update_key_dict <- function(dataset, key.dict, custom_indices) {
  # Subset for names that were custom added by users
  custom <- dataset[custom_indices, ]
  # Make an indicator for these actors' names, entity types, and iso
  cus_ind <- paste0(custom$name, custom$entity.type, custom$iso)
  # Match the indicator with key dict (we want those that are not in the key dict yet)
  # match_rows give the indices of those that ARE in the key dict, use ! to get the reverse
  match_rows <- which(cus_ind %in% paste0(key.dict$right, key.dict$entity.type,
                                          key.dict$iso) | cus_ind %in% paste0(key.dict$wrong,
                                                                              key.dict$entity.type,
                                                                              key.dict$iso))
  # Create the rows to be binded to key.dict
  newrows <- data.frame(right = custom$name[!match_rows],
                        wrong = custom$name[!match_rows],
                        iso = custom$iso[!match_rows],
                        entity.type = custom$entity.type[!match_rows],
                        allcaps = toupper(custom$name[!match_rows]),
                        caverphone = phonics::caverphone(custom$name[!match_rows], clean = FALSE),
                        caverphone.modified = phonics::caverphone(custom$name[!match_rows], modified = TRUE, clean = FALSE),
                        cologne = phonics::cologne(custom$name[!match_rows], clean = FALSE),
                        lein = phonics::lein(custom$name[!match_rows], clean = FALSE),
                        metaphone = phonics::metaphone(custom$name[!match_rows], clean = FALSE),
                        mra = phonics::mra_encode(custom$name[!match_rows], clean = FALSE),
                        nysiis = phonics::nysiis(custom$name[!match_rows], clean = FALSE),
                        nysiis.modified = phonics::nysiis(custom$name[!match_rows], modified = TRUE, clean = FALSE),
                        onca = phonics::onca(custom$name[!match_rows], clean = FALSE),
                        onca.modified = phonics::onca(custom$name[!match_rows], modified = TRUE, clean = FALSE),
                        onca.refined = phonics::onca(custom$name[!match_rows], refined = TRUE, clean = FALSE),
                        onca.modified.refined = phonics::onca(custom$name[!match_rows], modified = TRUE, refined = TRUE, clean = FALSE),
                        phonex = phonics::phonex(custom$name[!match_rows], clean = FALSE),
                        rogerroot = phonics::rogerroot(custom$name[!match_rows], clean = FALSE),
                        soundex = phonics::soundex(custom$name[!match_rows], clean = FALSE),
                        soundex.refined = phonics::refinedSoundex(custom$name[!match_rows], clean = FALSE),
                        statcan = phonics::statcan(custom$name[!match_rows], clean = FALSE))
# Bind rows to key.dict
key.dict <- rbind(key.dict, newrows)
return(key.dict)
}
