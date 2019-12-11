rename_col <- function(dataset, raw_name_col, raw_country_col, raw_type_col) {
  # Function that takes in the raw name, country, and entity.type columns and returns
  # standardized names for further analysis
  names(dataset)[names(dataset) == raw_name_col] <- "name"
  names(dataset)[names(dataset) == raw_country_col] <- "country"
  names(dataset)[names(dataset) == raw_type_col] <- "entity.type"
  return(dataset)
}
