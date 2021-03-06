% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_phonetic_matching_functions.R
\name{contextualize_data}
\alias{contextualize_data}
\title{Merge contextual data into original dataset}
\usage{
contextualize_data(
  dataset,
  contextual_df,
  context = c("region", "population", "population_year", "lat", "lng", "area",
    "area_units", "initiatives_committed", "num_commit", "state")
)
}
\arguments{
\item{dataset}{Original dataset}

\item{contextual_df}{Contextuals database included in the package. Called
using `r contextuals`}

\item{context}{Vector consisting of contextual column name to be merged
into the dataset includes "region", "population", "population_year", "lat",
"lng", "area", "area_units", "intiatives_committed", "num_commit", "state".
Defaults to taking all the contextual information.}
}
\value{
Dataset with contextuals merged
}
\description{
Contextual data from the contextuals database obtained from a variety
of sources. Contextual information includes region, population, latitude,
longitude, area, elevation, and the initiatives committed by the actor. Merging is done
based on actors' name, iso, and entity type.
}
