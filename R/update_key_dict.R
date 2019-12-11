update_key_dict <- function(dataset, key.dict) {
  for (i in 1:length(custom_indices)) {
    ind <- custom_indices[i]
    
    # ensuring duplicate entries aren't added to the key dictionary 
    # (e.g. not re-adding names that have already been added)
    # if the entry about to be added in the key dictionary has the same 
    # right name, wrong name, iso, and entity type as an existing entry, it will not be added
    
    ## finding all the row numbers in which the dataset matches the key dictionary--
    ## on right name, wrong name, iso, and entity type
    
    match_wrongname_rows <- which(!is.na(match(key.dict$wrong, dataset$name[i])))
    match_rightname_rows <- which(!is.na(match(key.dict$right, daataset$name[i]))) # 6700, 8175
    match_iso_rows <- which(!is.na(match(key.dict$iso, dataset$iso[i])))
    match_entitytype_rows <- which(!is.na(match(key.dict$entity.type, dataset$iso[i])))
    
    ## finding the row numbers in which all of the dataset's name, iso, and entity type
    ## any row in the key dictionary's wrong name, right name, iso, and entity type
    match_names_rows <- intersect(match_wrongname_rows, match_rightname_rows)
    match_names_iso_rows <- intersect(match_names_rows, match_iso_rows)
    all4_matching_rows <- intersect(match_names_iso_rows, match_entitytype_rows)
    
    # only add the entry to the key dictionary if it doesn't already exist in the key dictionary
    if (length(all4_matching_rows) == 0) {
      newrow <- data.frame(right = as.character(dataset$name[ind]), 
                           wrong = as.character(dataset$name[ind]), 
                           iso = dataset$iso[ind], 
                           entity.type = dataset$entity.type[ind],
                           allcaps = toupper(dataset$name[ind]),
                           caverphone = caverphone(as.character(dataset$name[ind]), clean = FALSE),
                           caverphone.modified = caverphone(as.character(dataset$name[ind]), modified = TRUE, clean = FALSE),
                           cologne = cologne(as.character(dataset$name[ind]), clean = FALSE),
                           lein = lein(as.character(dataset$name[ind]), clean = FALSE),
                           metaphone = metaphone(as.character(dataset$name[ind]), clean = FALSE),
                           mra = mra_encode(as.character(dataset$name[ind]), clean = FALSE),
                           nysiis = nysiis(as.character(dataset$name[ind]), clean = FALSE),
                           nysiis.modified = nysiis(as.character(dataset$name[ind]), modified = TRUE, clean = FALSE),
                           onca = onca(as.character(dataset$name[ind]), clean = FALSE),
                           onca.modified = onca(as.character(dataset$name[ind]), modified = TRUE, clean = FALSE),
                           onca.refined = onca(as.character(dataset$name[ind]), refined = TRUE, clean = FALSE),
                           onca.modified.refined = onca(as.character(dataset$name[ind]), modified = TRUE, refined = TRUE, clean = FALSE),
                           phonex = phonex(as.character(dataset$name[ind]), clean = FALSE),
                           rogerroot = rogerroot(as.character(dataset$name[ind]), clean = FALSE),
                           soundex = soundex(as.character(dataset$name[ind]), clean = FALSE),
                           soundex.refined = refinedSoundex(as.character(dataset$name[ind]), clean = FALSE),
                           statcan = statcan(as.character(dataset$name[ind]), clean = FALSE)) 
      key.dict <- rbind(key.dict, newrow) 
    } else if (length(all4_matching_rows) > 0) {
      print(paste0("The entry ", dataset$name[ind], " seems to already exist in the key dictionary in the following rows: ", all4_matching_rows))
    }
  }
  return(key.dict)
}
