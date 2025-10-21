#' @title Download, load, and assign the ClimActor key dictionary (v2.0.0) to the global environment
#' @description This function downloads a specific Parquet file containing a key dictionary
#' from the 'datadrivenenvirolab/ClimActor' GitHub repository using the
#' 'piggyback' package, loads it into R using 'arrow', and assigns the resulting
#' data frame to the variable \code{key_dict} in the \strong{global environment}.
#'
#' @return The function returns \code{invisible(NULL)} and is executed for its
#'   side effect of assigning \code{key_dict} globally.
#' @importFrom piggyback pb_download
#' @importFrom arrow read_parquet
#' @export

load_key_dict <- function() {
  data_file <- file.path(tempdir(), "ClimActor_1_keydict.parquet")

  # Use piggyback::pb_download, which is optimized for this
  piggyback::pb_download(file = "ClimActor_1_keydict.parquet",
                         repo = "datadrivenenvirolab/ClimActor",
                         tag = "v2.0.0", # The release tag where you stored it
                         dest = tempdir())

  # Load the parquet file using a package like arrow
  key_dict_data <- arrow::read_parquet(data_file)

  # Assign to the global environment using <<-
  key_dict <<- key_dict_data

}
