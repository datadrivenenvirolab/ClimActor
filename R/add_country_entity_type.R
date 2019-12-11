add_country_entity_type <- function(dataframe, type = NA) {
  if (!"country" %in% names(dataframe)) {
    dataframe$country <- NA
  }
  if (!"entity.type" %in% names(dataframe)) {
    dataframe$entity.type <- type
  } else if (!is.na(type)) {
    dataframe$entity.type <- type
  }
  return(dataframe)
}