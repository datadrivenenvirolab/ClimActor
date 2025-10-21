require(dplyr)
require(tidyr)
require(stringi)
require(crayon)

#' @title Machine-aided Phonetic matching using and String Similarity
#' @description This function takes a (\code{dataset}) containing entity names and, for each unique entry,
#' attempts to find matches in a provided key dictionary (\code{key.dict}) by using
#' a suite of phonetic algorithms and string distance metrics (e.g., Metaphone, NYSIIS,
#' Phonex, RogerRoot, along with methods like Jaro-Winkler, cosine, and Jaccard).
#' It calculates a composite phonetic score and presents the top matches to the user
#' in the R console for **interactive selection and correction**.
#'
#' **Note:** This function is designed to be run interactively. It uses global
#' assignments (\code{<<-}) for internal state tracking and relies on console
#' input (\code{readline}) from the user. The process requires full completion for
#' all rows in (\code{dataset}) to be saved.
#' It requires the 'dplyr', 'tidyr', ' 'stringi', 'phonics', 'crayon',
#' and 'stringdist' packages (some implicitly via the code).
#' The helper functions \code{.coerce_location_names}, \code{remove_extra_updated}.
#'
#' @param dataset A data frame requiring the name to be cleaned. Must contain columns named
#'   \code{name}, \code{iso}, and \code{entity_type}.
#' @param key.dict A data frame used as the lookup dictionary. Must contain columns
#'   for phonetic codes (e.g., \code{metaphone}, \code{nysiis.modified}, etc., which
#'   must be pre-calculated), \code{iso}, \code{climactor_id}, \code{right} (the correct name),
#'   \code{GDAM_id}, \code{entity_type}, and \code{population} (or another numeric variable
#'   used for display, as the current code uses \code{population}).
#' @param checkvar A character string specifying a variable name in \code{dataset}
#'   (e.g., population) to be displayed during the interactive matching process.
#'
#' @return The modified \code{dataset} data frame. Names that were interactively
#'   selected by the user are updated to the corresponding \code{right} name and
#'   \code{climactor_id} from the \code{key.dict}. If the user enters 'S', the
#'   current progress is returned. Unmatched indices are tracked globally in
#'   \code{unmatched_indices}.
#' @export


phonetify_function_wide <- function (dataset, key.dict, checkvar){

  dataset <-   dataset %>%
    mutate(name_og = name)%>%
    mutate(name = stri_trans_general(name, id = "Latin-ASCII"))%>%
    remove_extra_updated(.)

  name_ind <<- 1:nrow(dataset)
  ind.short <- 1:nrow(dataset)

  ds.phon.codes <- phonics::phonics(.coerce_location_names(dataset$name[ind.short]),
                                    c("caverphone", "caverphone.modified", "cologne", "lein",
                                      "metaphone", "nysiis", "nysiis.modified", "onca",
                                      "onca.modified", "onca.refined", "onca.modified.refined",
                                      "phonex", "rogerroot", "soundex", "soundex.refined",
                                      "statcan"), clean = FALSE)
  ds.phon.codes$rogerroot <- as.character(ds.phon.codes$rogerroot)
  .actor_updates <<- data.frame(ind = ind.short, name_wrong = dataset$name[ind.short])
  for (i in seq_along(ind.short)) {
    ind <- ind.short[i]
    kd.filtered <- key.dict %>% dplyr::filter(iso == dataset$iso[ind])
    kd.filtered$rogerroot <- as.character(kd.filtered$rogerroot)
    kd.filtered <- kd.filtered %>%
      dplyr::mutate(metaphone.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$metaphone[i],kd.filtered$metaphone, method = "dl"),
                                                     stringdist::stringsim(ds.phon.codes$metaphone[i],kd.filtered$metaphone, method = "qgram"),
                                                     stringdist::stringsim(ds.phon.codes$metaphone[i],kd.filtered$metaphone, method = "cosine"),
                                                     stringdist::stringsim(ds.phon.codes$metaphone[i],kd.filtered$metaphone, method = "jaccard"),
                                                     stringdist::stringsim(ds.phon.codes$metaphone[i],kd.filtered$metaphone, method = "jw")), na.rm = T),
                    nysiis.modified.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$nysiis.modified[i],kd.filtered$nysiis.modified, method = "dl"),
                                                           stringdist::stringsim(ds.phon.codes$nysiis.modified[i],kd.filtered$nysiis.modified, method = "qgram"),
                                                           stringdist::stringsim(ds.phon.codes$nysiis.modified[i],kd.filtered$nysiis.modified, method = "cosine"),
                                                           stringdist::stringsim(ds.phon.codes$nysiis.modified[i],kd.filtered$nysiis.modified, method = "jaccard"),
                                                          stringdist::stringsim(ds.phon.codes$nysiis.modified[i],kd.filtered$nysiis.modified, method = "jw")), na.rm = T),
                    onca.modified.refined.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],kd.filtered$onca.modified.refined, method = "dl"),
                                                                 stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],kd.filtered$onca.modified.refined, method = "qgram"),
                                                                 stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],kd.filtered$onca.modified.refined, method = "cosine"),
                                                                 stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],kd.filtered$onca.modified.refined, method = "jaccard"),
                                                                 stringdist::stringsim(ds.phon.codes$onca.modified.refined[i],kd.filtered$onca.modified.refined, method = "jw")),na.rm = T),
                    phonex.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$phonex[i],kd.filtered$phonex, method = "dl"),
                                                  stringdist::stringsim(ds.phon.codes$phonex[i],kd.filtered$phonex, method = "qgram"),
                                                  stringdist::stringsim(ds.phon.codes$phonex[i],kd.filtered$phonex, method = "cosine"),
                                                  stringdist::stringsim(ds.phon.codes$phonex[i],kd.filtered$phonex, method = "jaccard"),
                                                  stringdist::stringsim(ds.phon.codes$phonex[i],kd.filtered$phonex, method = "jw")), na.rm = T),
                    rogerroot.score = rowMeans(cbind(stringdist::stringsim(ds.phon.codes$rogerroot[i],kd.filtered$rogerroot, method = "dl"),
                                                     stringdist::stringsim(ds.phon.codes$rogerroot[i],kd.filtered$rogerroot, method = "qgram"),
                                                     stringdist::stringsim(ds.phon.codes$rogerroot[i],kd.filtered$rogerroot, method = "cosine"),
                                                     stringdist::stringsim(ds.phon.codes$rogerroot[i],kd.filtered$rogerroot, method = "jaccard"),
                                                     stringdist::stringsim(ds.phon.codes$rogerroot[i],kd.filtered$rogerroot, method = "jw")), na.rm = T))
    kd.filtered <- kd.filtered %>% dplyr::mutate(tot.phon.score = metaphone.score + nysiis.modified.score +
                                                                  onca.modified.refined.score + phonex.score +
                                                                  rogerroot.score)
    kd.filtered <- kd.filtered %>% dplyr::arrange(dplyr::desc(tot.phon.score))
    # kd.filtered <- kd.filtered[!duplicated(kd.filtered$right), ]
    kd.filtered <- kd.filtered %>% distinct(climactor_id, .keep_all = TRUE)
    origname <- dataset$name[ind]
    cat(paste0("Currently cleaning actor ", i, " of ", length(ind.short),
               " (", round((i - 1)/length(ind.short) * 100, 1),
               "%) completed.\n"))
    if (nrow(kd.filtered) == 0) {
      cat(paste0("There seems to be no available matches for ",
                 dataset$name_og[ind], "; iso = ", dataset$iso[ind],
                 " entity type = ", dataset$entity_type[ind],
                 ".\n", "Proceed to manual check and add new entity using the bugsheet"))
    }
    cat(paste0("The original name is ", crayon::yellow(dataset$name_og[ind]),
               " with iso code ", crayon::green(dataset$iso[ind]), " and entity type ",
               crayon::blue(dataset$entity_type[ind])," with ", checkvar, ": ", crayon::red(f_comma(dataset[checkvar][[1]][ind]), "\n")))
    cat(paste0("Here are some possible matches we found: \n"))
    if (nrow(kd.filtered) < 20) {
      for (k in seq_len(nrow(kd.filtered))) {
        if (is.na(kd.filtered$right[k])) {
          next
        }
        else if (k < 20) {
          cat(paste0(k, ":  ", crayon::yellow(kd.filtered$right[k]), " and GDAM_id: ", crayon::cyan(kd.filtered$GDAM_id[k]),
                     "; iso: ", crayon::green(kd.filtered$iso[k]), " and entity type: ",
                     crayon::blue(kd.filtered$entity_type[k]), " with ", checkvar, ": ", crayon::red(f_comma(round(kd.filtered$population[k],0)) ,"\n")))
        }
        else {
          cat(paste0(k, ": ", crayon::yellow(kd.filtered$right[k]), " and GDAM_id: ", crayon::cyan(kd.filtered$GDAM_id[k]),
                     "; iso: ", crayon::green(kd.filtered$iso[k]), " and entity type: ",
                     crayon::blue(kd.filtered$entity_type[k]), " with ", checkvar, ": ", crayon::red(f_comma(round(kd.filtered$population[k],0)) ,"\n")))
        }
      }
    }
    else {
      for (k in 1:20) {
        if (is.na(kd.filtered$right[k])) {
          next
        }
        else if (k < 20) {
          cat(paste0(k, ":  ", crayon::yellow(kd.filtered$right[k]), " and GDAM_id: ", crayon::cyan(kd.filtered$GDAM_id[k]),
                     "; iso: ", crayon::green(kd.filtered$iso[k]), " and entity type: ",
                     crayon::blue(kd.filtered$entity_type[k]), " with ", checkvar, ": ", crayon::red(f_comma(round(kd.filtered$population[k],0)) ,"\n")))
        }
        else {
          cat(paste0(k, ": ", crayon::yellow(kd.filtered$right[k]), " and GDAM_id: ", crayon::cyan(kd.filtered$GDAM_id[k]),
                     "; iso: ", crayon::green(kd.filtered$iso[k]), " and entity type: ",
                     crayon::blue(kd.filtered$entity_type[k]), " with ", checkvar, ": ", crayon::red(f_comma(round(kd.filtered$population[k],0)) ,"\n")))
        }
      }
    }
    cat("Which name matches? Choose 1-20. (Type N if none match \n")
    ans1 <- readline(prompt = "Answer: ")
    if (grepl("^[0-9]{1,2}$", ans1)) {
      tmpans <- ans1
      while (as.numeric(tmpans) > 20 | as.numeric(tmpans) >
             nrow(kd.filtered)) {
        cat("It seems like you have entered a number that is out of bounds. Please enter another number.\n")
        tmpans <- readline(prompt = "Answer: ")
        if (!grepl("^[0-9]{1,2}$", tmpans)) {
          cat("Sorry, an invalid answer was provided.\n")
          cat(paste0("The previous name (", origname,
                     ") will be kept.", " The index of this entry will be recorded for future inspection.\n\n"))
          if (!exists("unmatched_indices")) {
            unmatched_indices <<- ind
          }
          else {
            unmatched_indices <<- c(unmatched_indices,
                                    ind)
          }
          break
        }
      }
      if (!is.na(ans1) & (as.numeric(ans1) %in% (1:20)) &
          (!is.na(kd.filtered$right[as.numeric(ans1)]))) {
        correct.name <- kd.filtered$right[as.numeric(ans1)]
        correct.entity <- kd.filtered$entity_type[as.numeric(ans1)]
        correct.climactor_id<- kd.filtered$climactor_id[as.numeric(ans1)]
        # cat(paste0(correct.name," - ",correct.entity," has been selected and will replace ",
        #            dataset$name[ind], " and ", dataset$entity_type, " in the database. \n\n"))
        samename_inds <- which(dataset$name == origname &
                                 dataset$iso == dataset$iso[ind] &
                                 dataset$entity_type ==
                                 dataset$entity_type[ind])
        if (length(samename_inds) != 0) {
          dataset$name[samename_inds] <- correct.name
          dataset$entity_type[samename_inds] <- correct.entity
          dataset$climactor_id[samename_inds] <- correct.climactor_id
          name_ind <<- name_ind[!(name_ind %in% samename_inds)]
        }
      }
    }
    else if (substr(toupper(as.character(ans1)), 1, 1) ==
             "S") {
      cat("Your current progress will be returned.\n")
      cat("If it's not saved, be sure you saved your results in a variable with an assign function\n")
      return(dataset)
    }
    else if (substr(toupper(as.character(ans1)), 1, 1) ==
             "N") {
      cat(paste0("The previous name (", origname, ") will be kept.\n\n"))
      if (!exists("unmatched_indices")) {
        unmatched_indices <<- ind
      }
      else {
        unmatched_indices <<- c(unmatched_indices, ind)
      }
    }
    else {
      cat("Sorry, an invalid answer was provided.\n")
      cat(paste0("The previous name (", origname, ") will be kept.",
                 " The index of this entry will be recorded for future inspection.\n\n"))
      if (!exists("unmatched_indices")) {
        unmatched_indices <<- ind
      }
      else {
        unmatched_indices <<- c(unmatched_indices, ind)
      }
    }
  }
  if (exists(paste0("isoname"))) {
    names(dataset)[grepl("^iso$", names(dataset))] <- isoname
  }
  if (exists(paste0("entity_typename"))) {
    names(dataset)[grepl("^entity_type$", names(dataset))] <- entity_typename
  }
  if (exists(paste0("namename"))) {
    names(dataset)[grepl("^name$", names(dataset))] <- namename
  }
  return(dataset)
}
