library(stringdist)
library(dplyr)
library(phonics)

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
  ds.phon.codes <- phonics(dataset$name[indices], c("caverphone", "caverphone.modified", 
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
    kd.filtered <- key.dict %>% filter(iso == dataset$iso[ind] & entity.type == dataset$entity.type[ind])
    kd.filtered$rogerroot <- as.character(kd.filtered$rogerroot)
    
    ## calculating scores using the caverphone phonetic algorithm
    ## quantification of scores is based on 9 edit distance methods
    
    kd.filtered <- kd.filtered %>% mutate(metaphone.score = rowMeans(cbind(stringsim(ds.phon.codes$metaphone[i], 
                                                                                     kd.filtered$metaphone, method = "dl"),
                                                                           stringsim(ds.phon.codes$metaphone[i], 
                                                                                     kd.filtered$metaphone, method = "qgram"),
                                                                           stringsim(ds.phon.codes$metaphone[i], 
                                                                                     kd.filtered$metaphone, method = "cosine"),
                                                                           stringsim(ds.phon.codes$metaphone[i], 
                                                                                     kd.filtered$metaphone, method = "jaccard"),
                                                                           stringsim(ds.phon.codes$metaphone[i], 
                                                                                     kd.filtered$metaphone, method = "jw")), 
                                                                     na.rm = T),
                                          nysiis.modified.score = rowMeans(cbind(stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                           kd.filtered$nysiis.modified, method = "dl"),
                                                                                 stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                           kd.filtered$nysiis.modified, method = "qgram"),
                                                                                 stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                           kd.filtered$nysiis.modified, method = "cosine"),
                                                                                 stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                           kd.filtered$nysiis.modified, method = "jaccard"),
                                                                                 stringsim(ds.phon.codes$nysiis.modified[i],
                                                                                           kd.filtered$nysiis.modified, method = "jw")),
                                                                           na.rm = T),
                                          onca.modified.refined.score = rowMeans(cbind(stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                 kd.filtered$onca.modified.refined, method = "dl"),
                                                                                       stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                 kd.filtered$onca.modified.refined, method = "qgram"),
                                                                                       stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                 kd.filtered$onca.modified.refined, method = "cosine"),
                                                                                       stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                 kd.filtered$onca.modified.refined, method = "jaccard"),
                                                                                       stringsim(ds.phon.codes$onca.modified.refined[i],
                                                                                                 kd.filtered$onca.modified.refined, method = "jw")),
                                                                                 na.rm = T),
                                          phonex.score = rowMeans(cbind(stringsim(ds.phon.codes$phonex[i],
                                                                                  kd.filtered$phonex, method = "dl"),
                                                                        stringsim(ds.phon.codes$phonex[i],
                                                                                  kd.filtered$phonex, method = "qgram"),
                                                                        stringsim(ds.phon.codes$phonex[i],
                                                                                  kd.filtered$phonex, method = "cosine"),
                                                                        stringsim(ds.phon.codes$phonex[i],
                                                                                  kd.filtered$phonex, method = "jaccard"),
                                                                        stringsim(ds.phon.codes$phonex[i],
                                                                                  kd.filtered$phonex, method = "jw")),
                                                                  na.rm = T),
                                          rogerroot.score = rowMeans(cbind(stringsim(ds.phon.codes$rogerroot[i],
                                                                                     kd.filtered$rogerroot, method = "dl"),
                                                                           stringsim(ds.phon.codes$rogerroot[i],
                                                                                     kd.filtered$rogerroot, method = "qgram"),
                                                                           stringsim(ds.phon.codes$rogerroot[i],
                                                                                     kd.filtered$rogerroot, method = "cosine"),
                                                                           stringsim(ds.phon.codes$rogerroot[i],
                                                                                     kd.filtered$rogerroot, method = "jaccard"),
                                                                           stringsim(ds.phon.codes$rogerroot[i],
                                                                                     kd.filtered$rogerroot, method = "jw")),
                                                                     na.rm = T))
    
    # # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          #kd.filtered$caverphone, method = "osa"),
    # #                                       #score2 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          #kd.filtered$caverphone, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          kd.filtered$caverphone, method = "dl"),
    # #                                       #score4 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          #kd.filtered$caverphone, method = "lcs"),
    # #                                       #score5 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          #kd.filtered$caverphone, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          kd.filtered$caverphone, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          kd.filtered$caverphone, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          kd.filtered$caverphone, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$caverphone[i],
    # #                                                          kd.filtered$caverphone, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(caverphone.score = (#score1 + score2 +
    # #                                                             score3 +
    # #                                                             #score4 + score5 +
    # #                                                             score6 +
    # #                                                             score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the modified caverphone phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          #kd.filtered$caverphone.modified, method = "osa"),
    # #                                       #score2 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          #kd.filtered$caverphone.modified, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          kd.filtered$caverphone.modified, method = "dl"),
    # #                                       #score4 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          #kd.filtered$caverphone.modified, method = "lcs"),
    # #                                       #score5 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          #kd.filtered$caverphone.modified, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          kd.filtered$caverphone.modified, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          kd.filtered$caverphone.modified, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          kd.filtered$caverphone.modified, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$caverphone.modified[i],
    # #                                                          kd.filtered$caverphone.modified, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(caverphone.modified.score = (#score1 + score2 +
    # #                                                                      score3 +
    # #                                                                      #score4 + score5 +
    # #                                                                      score6 +
    # #                                                                      score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the cologne phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                       #                   as.character(kd.filtered$cologne), method = "osa"),
    # #                                       # score2 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                       #                    as.character(kd.filtered$cologne), method = "lv"),
    # #                                       score3 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                                          as.character(kd.filtered$cologne, method = "dl")),
    # #                                       # score4 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                       #                    as.character(kd.filtered$cologne, method = "lcs")),
    # #                                       # score5 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                       #                    as.character(kd.filtered$cologne, method = "hamming")),
    # #                                       score6 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                                          as.character(kd.filtered$cologne, method = "qgram")),
    # #                                       score7 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                                          as.character(kd.filtered$cologne, method = "cosine")),
    # #                                       score8 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                                          as.character(kd.filtered$cologne, method = "jaccard")),
    # #                                       score9 = stringsim(as.character(ds.phon.codes$cologne[i]),
    # #                                                          as.character(kd.filtered$cologne, method = "jw")))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(cologne.score = (#score1 + score2 +
    # #                                                          score3 +
    # #                                                         # score4 + score5 +
    # #                                                          score6 +
    # #                                                          score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the lein phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$lein[i],
    # #                                       #                    kd.filtered$lein, method = "osa"),
    # #                                       # score2 = stringsim(ds.phon.codes$lein[i],
    # #                                       #                    kd.filtered$lein, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$lein[i],
    # #                                                          kd.filtered$lein, method = "dl"),
    # #                                       # score4 = stringsim(ds.phon.codes$lein[i],
    # #                                       #                    kd.filtered$lein, method = "lcs"),
    # #                                       # score5 = stringsim(ds.phon.codes$lein[i],
    # #                                       #                    kd.filtered$lein, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$lein[i],
    # #                                                          kd.filtered$lein, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$lein[i],
    # #                                                          kd.filtered$lein, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$lein[i],
    # #                                                          kd.filtered$lein, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$lein[i],
    # #                                                          kd.filtered$lein, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(lein.score = (#score1 + score2 +
    # #                                                       score3 +
    # #                                                       #score4 + score5 +
    # #                                                       score6 +
    # #                                                       score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the metaphone phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$metaphone[i],
    #   #                    kd.filtered$metaphone, method = "osa"),
    #   # score2 = stringsim(ds.phon.codes$metaphone[i],
    #   #                    kd.filtered$metaphone, method = "lv"),
    #   score3 = stringsim(ds.phon.codes$metaphone[i],
    #                      kd.filtered$metaphone, method = "dl"),
    #   # score4 = stringsim(ds.phon.codes$metaphone[i],
    #   #                    kd.filtered$metaphone, method = "lcs"),
    #   # score5 = stringsim(ds.phon.codes$metaphone[i],
    #   #                    kd.filtered$metaphone, method = "hamming"),
    #   score6 = stringsim(ds.phon.codes$metaphone[i],
    #                      kd.filtered$metaphone, method = "qgram"),
    #   score7 = stringsim(ds.phon.codes$metaphone[i],
    #                      kd.filtered$metaphone, method = "cosine"),
    #   score8 = stringsim(ds.phon.codes$metaphone[i],
    #                      kd.filtered$metaphone, method = "jaccard"),
    #   score9 = stringsim(ds.phon.codes$metaphone[i],
    #                      kd.filtered$metaphone, method = "jw"))
    # 
    # kd.filtered <- kd.filtered %>% mutate(metaphone.score = (#score1 + score2 +
    #   score3 +
    #     #score4 + score5 +
    #     score6 +
    #     score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the New York State Identification and Intelligence System (NYSIIS) phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$nysiis[i],
    # #                                       #                    kd.filtered$nysiis, method = "osa"),
    # #                                       # score2 = stringsim(ds.phon.codes$nysiis[i],
    # #                                       #                    kd.filtered$nysiis, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$nysiis[i],
    # #                                                          kd.filtered$nysiis, method = "dl"),
    # #                                       # score4 = stringsim(ds.phon.codes$nysiis[i],
    # #                                       #                    kd.filtered$nysiis, method = "lcs"),
    # #                                       # score5 = stringsim(ds.phon.codes$nysiis[i],
    # #                                       #                    kd.filtered$nysiis, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$nysiis[i],
    # #                                                          kd.filtered$nysiis, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$nysiis[i],
    # #                                                          kd.filtered$nysiis, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$nysiis[i],
    # #                                                          kd.filtered$nysiis, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$nysiis[i],
    # #                                                          kd.filtered$nysiis, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(nysiis.score = (#score1 + score2 +
    # #                                                         score3 +
    # #                                                       #  score4 + score5 +
    # #                                                         score6 +
    # #                                                         score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the modified NYSIIS phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$nysiis.modified[i],
    #   #                    kd.filtered$nysiis.modified, method = "osa"),
    #   # score2 = stringsim(ds.phon.codes$nysiis.modified[i],
    #   #                    kd.filtered$nysiis.modified, method = "lv"),
    #   score3 = stringsim(ds.phon.codes$nysiis.modified[i],
    #                      kd.filtered$nysiis.modified, method = "dl"),
    #   # score4 = stringsim(ds.phon.codes$nysiis.modified[i],
    #   #                    kd.filtered$nysiis.modified, method = "lcs"),
    #   # score5 = stringsim(ds.phon.codes$nysiis.modified[i],
    #   #                    kd.filtered$nysiis.modified, method = "hamming"),
    #   score6 = stringsim(ds.phon.codes$nysiis.modified[i],
    #                      kd.filtered$nysiis.modified, method = "qgram"),
    #   score7 = stringsim(ds.phon.codes$nysiis.modified[i],
    #                      kd.filtered$nysiis.modified, method = "cosine"),
    #   score8 = stringsim(ds.phon.codes$nysiis.modified[i],
    #                      kd.filtered$nysiis.modified, method = "jaccard"),
    #   score9 = stringsim(ds.phon.codes$nysiis.modified[i],
    #                      kd.filtered$nysiis.modified, method = "jw"))
    # 
    # kd.filtered <- kd.filtered %>% mutate(nysiis.modified.score = (#score1 + score2 +
    #   score3 +
    #     #score4 + score5 +
    #     score6 +
    #     score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the Oxford Name Compression Algorithm (ONCA)
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "osa"),
    # #                                       score2 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "dl"),
    # #                                       score4 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "lcs"),
    # #                                       score5 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$onca[i],
    # #                                                          kd.filtered$onca, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(onca.score = (score1 + score2 + score3 +
    # #                                                       score4 + score5 + score6 +
    # #                                                       score7 + score8 + score9)/9)
    # 
    # ## calculating scores using the modified ONCA
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "osa"),
    # #                                       score2 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "dl"),
    # #                                       score4 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "lcs"),
    # #                                       score5 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$onca.modified[i],
    # #                                                          kd.filtered$onca.modified, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(onca.modified.score = (score1 + score2 + score3 +
    # #                                                                score4 + score5 + score6 +
    # #                                                                score7 + score8 + score9)/9)
    # 
    # ## calculating scores using the refined ONCA
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "osa"),
    # #                                       score2 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "dl"),
    # #                                       score4 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "lcs"),
    # #                                       score5 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$onca.refined[i],
    # #                                                          kd.filtered$onca.refined, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(onca.refined.score = (score1 + score2 + score3 +
    # #                                                               score4 + score5 + score6 +
    # #                                                               score7 + score8 + score9)/9)
    # 
    # ## calculating scores using the modified refined ONCA
    # ## quantification of scores is based on 9 edit distance methods
    # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #   #                    kd.filtered$onca.modified.refined, method = "osa"),
    #   # score2 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #   #                    kd.filtered$onca.modified.refined, method = "lv"),
    #   score3 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #                      kd.filtered$onca.modified.refined, method = "dl"),
    #   # score4 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #   #                    kd.filtered$onca.modified.refined, method = "lcs"),
    #   # score5 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #   #                    kd.filtered$onca.modified.refined, method = "hamming"),
    #   score6 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #                      kd.filtered$onca.modified.refined, method = "qgram"),
    #   score7 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #                      kd.filtered$onca.modified.refined, method = "cosine"),
    #   score8 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #                      kd.filtered$onca.modified.refined, method = "jaccard"),
    #   score9 = stringsim(ds.phon.codes$onca.modified.refined[i],
    #                      kd.filtered$onca.modified.refined, method = "jw"))
    # 
    # kd.filtered <- kd.filtered %>% mutate(onca.modified.refined.score = (#score1 + score2 +
    #   score3 +
    #     #score4 + score5 +
    #     score6 +
    #     score7 + score8 + score9)/5)
    # 
    # ## calculating scores using phonex
    # ## quantification of scores is based on 9 edit distance methods
    # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$phonex[i],
    #   #                    kd.filtered$phonex, method = "osa"),
    #   # score2 = stringsim(ds.phon.codes$phonex[i],
    #   #                    kd.filtered$phonex, method = "lv"),
    #   score3 = stringsim(ds.phon.codes$phonex[i],
    #                      kd.filtered$phonex, method = "dl"),
    #   # score4 = stringsim(ds.phon.codes$phonex[i],
    #   #                    kd.filtered$phonex, method = "lcs"),
    #   # score5 = stringsim(ds.phon.codes$phonex[i],
    #   #                    kd.filtered$phonex, method = "hamming"),
    #   score6 = stringsim(ds.phon.codes$phonex[i],
    #                      kd.filtered$phonex, method = "qgram"),
    #   score7 = stringsim(ds.phon.codes$phonex[i],
    #                      kd.filtered$phonex, method = "cosine"),
    #   score8 = stringsim(ds.phon.codes$phonex[i],
    #                      kd.filtered$phonex, method = "jaccard"),
    #   score9 = stringsim(ds.phon.codes$phonex[i],
    #                      kd.filtered$phonex, method = "jw"))
    # 
    # kd.filtered <- kd.filtered %>% mutate(phonex.score = (#score1 + score2 +
    #   score3 +
    #     #score4 + score5 +
    #     score6 +
    #     score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the roger root phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #   #                   as.character(kd.filtered$rogerroot), method = "osa"),
    #   # score2 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #   #                    as.character(kd.filtered$rogerroot, method = "lv")),
    #   score3 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #                      as.character(kd.filtered$rogerroot, method = "dl")),
    #   # score4 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #   #                    as.character(kd.filtered$rogerroot, method = "lcs")),
    #   # score5 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #   #                    as.character(kd.filtered$rogerroot, method = "hamming")),
    #   score6 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #                      as.character(kd.filtered$rogerroot, method = "qgram")),
    #   score7 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #                      as.character(kd.filtered$rogerroot, method = "cosine")),
    #   score8 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #                      as.character(kd.filtered$rogerroot, method = "jaccard")),
    #   score9 = stringsim(as.character(ds.phon.codes$rogerroot[i]),
    #                      as.character(kd.filtered$rogerroot, method = "jw")))
    # 
    # kd.filtered <- kd.filtered %>% mutate(rogerroot.score = (#score1 + score2 +
    #   score3 +
    #     #score4 + score5 +
    #     score6 +
    #     score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the soundex phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(score1 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "osa"),
    # #                                       score2 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "dl"),
    # #                                       score4 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "lcs"),
    # #                                       score5 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$soundex[i],
    # #                                                          kd.filtered$soundex, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(soundex.score = (score1 + score2 + score3 +
    # #                                                          score4 + score5 + score6 +
    # #                                                          score7 + score8 + score9)/9)
    # 
    # ## calculating scores using the refined soundex phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                       #                    kd.filtered$soundex.refined, method = "osa"),
    # #                                       # score2 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                       #                    kd.filtered$soundex.refined, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                                          kd.filtered$soundex.refined, method = "dl"),
    # #                                       # score4 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                       #                    kd.filtered$soundex.refined, method = "lcs"),
    # #                                       # score5 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                       #                    kd.filtered$soundex.refined, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                                          kd.filtered$soundex.refined, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                                          kd.filtered$soundex.refined, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                                          kd.filtered$soundex.refined, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$soundex.refined[i],
    # #                                                          kd.filtered$soundex.refined, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(soundex.refined.score = (#score1 + score2 +
    # #                                                                  score3 +
    # #                                                                  #score4 + score5 +
    # #                                                                  score6 +
    # #                                                                  score7 + score8 + score9)/5)
    # 
    # ## calculating scores using the Statistics Canada Name Coding (statcan) phonetic algorithm
    # ## quantification of scores is based on 9 edit distance methods
    # # kd.filtered <- kd.filtered %>% mutate(#score1 = stringsim(ds.phon.codes$statcan[i],
    # #                                       #                    kd.filtered$statcan, method = "osa"),
    # #                                       # score2 = stringsim(ds.phon.codes$statcan[i],
    # #                                       #                    kd.filtered$statcan, method = "lv"),
    # #                                       score3 = stringsim(ds.phon.codes$statcan[i],
    # #                                                          kd.filtered$statcan, method = "dl"),
    # #                                       # score4 = stringsim(ds.phon.codes$statcan[i],
    # #                                       #                    kd.filtered$statcan, method = "lcs"),
    # #                                       # score5 = stringsim(ds.phon.codes$statcan[i],
    # #                                       #                    kd.filtered$statcan, method = "hamming"),
    # #                                       score6 = stringsim(ds.phon.codes$statcan[i],
    # #                                                          kd.filtered$statcan, method = "qgram"),
    # #                                       score7 = stringsim(ds.phon.codes$statcan[i],
    # #                                                          kd.filtered$statcan, method = "cosine"),
    # #                                       score8 = stringsim(ds.phon.codes$statcan[i],
    # #                                                          kd.filtered$statcan, method = "jaccard"),
    # #                                       score9 = stringsim(ds.phon.codes$statcan[i],
    # #                                                          kd.filtered$statcan, method = "jw"))
    # #
    # # kd.filtered <- kd.filtered %>% mutate(statcan.score = (#score1 + score2 +
    # #                                                          score3 +
    # #                                                         #score4 + score5 +
    # #                                                          score6 +
    # #                                                          score7 + score8 + score9)/5)
    # 
    # ## calculating scores using no phonetic algorithms--only the edit distance methods
    # ## quantification of scores is based on the edit distances
    # ## these scores were found to be counterproductive to finding the best matches and consequently commented out
    # # kd.filtered <- kd.filtered %>% mutate(osa.score = stringsim(dataset$name[ind],
    # #                                                       kd.filtered$wrong, method = "osa"),
    # #                     lv.score = stringsim(dataset$name[ind],
    # #                                          kd.filtered$wrong, method = "lv"),
    # #                     dl.score = stringsim(dataset$name[ind],
    # #                                          kd.filtered$wrong, method = "dl"),
    # #                     lcs.score =  stringsim(dataset$name[ind],
    # #                                            kd.filtered$wrong, method = "lcs"),
    # #                     hamming.score = stringsim(dataset$name[ind],
    # #                                               kd.filtered$wrong, method = "hamming"),
    # #                     qgram.score = stringsim(dataset$name[ind],
    # #                                             kd.filtered$wrong, method = "qgram"),
    # #                     cosine.score = stringsim(dataset$name[ind],
    # #                                              kd.filtered$wrong, method = "cosine"),
    # #                     jaccard.score = stringsim(dataset$name[ind],
    # #                                               kd.filtered$wrong, method = "jaccard"),
    # #                     jw.score = stringsim(dataset$name[ind],
    # #                                          kd.filtered$wrong, method = "jw"))
    # 
    # # calculating the total score based on individual edit distance scores
    # #
    # # kd.filtered <- kd.filtered %>% mutate(tot.ed.score = osa.score + lv.score +
    # #                                   dl.score + lcs.score + hamming.score +
    # #                                   qgram.score + cosine.score +
    # #                                   jaccard.score + jw.score)
    # 
    # #calculating the total score based on phonetic algorithms

    kd.filtered <- kd.filtered %>% mutate(tot.phon.score = #caverphone.score + 
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
    
    # kd.filtered <- kd.filtered %>% mutate(total.score = tot.ed.score + tot.phon.score) # useful only when we had edit distance scores too
    #kd.filtered <- kd.filtered %>% mutate(total.score = tot.phon.score)
    
    # arranging by total score in descending order so we can list the top matches (based on those scores)
    kd.filtered <- kd.filtered %>% arrange(desc(tot.phon.score))
    
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
