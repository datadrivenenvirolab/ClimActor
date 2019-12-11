add_entity_type <- function(dataset, type){
  # Function that takes in a dataset and returns a new column "entity.type", with 
  # type input as the entry for the column
  dataset$entity.type <- type
  return(dataset)
}

