require(dplyr)
require(tidyr)
require(stringi)

#' @title Match Names to Climactor_id using and exact match to Key_dict with extra variables.
#' @description This function takes a primary data frame to be cleaned (\code{x}) containing at least the 'name' and 'iso' columns
#' and attempts to match the names to  a 'climactor_id' using the key_dict (\code{key_dict}) primarily its right and wrong columns
#' It performs several lookups based on original name, modified (lowercase, ASCII)
#' name, and both 'right' (correct) and 'wrong' (alternative) entries in the dictionary.
#' It filter for duplicates keeping only the matches where all retrieved \code{climactor_id}s are consistent.
#' Finally, it joins the retrieved \code{climactor_id} back to the original data and also retrieves
#' a specified \code{checkvar} associated with the matched ID which can be either population or area.
#'
#' This function requires the \code{key_dict} data frame to be available in the global environment,
#' and it assumes \code{x} has 'name' and 'iso' columns. Requieres \code{dplyr},
#' \code{tidyr}, and \code{stringi}.
#'
#' @param x A data frame containing at least 'name' (the text to be matched) and 'iso' (country code) columns.
#' @param checkvar A character string specifying the column name (e.g., "population") from \code{key_dict}
#'   that should be retrieved and joined back to \code{x} alongside the matched \code{climactor_id} to be used for supplementary checks.
#'
#' @return A data frame based on the input \code{x}, but with three new columns added:
#'   \enumerate{
#'     \item The original \code{checkvar} column is renamed to \code{\{checkvar\}\_check}.
#'     \item \code{climactor_id}: The resulting matched identifier.
#'     \item \code{checkvar}: The value from \code{key_dict} corresponding to the matched \code{climactor_id}.
#'   }
#'   The function filters for matches where all lookups results are the same and non-NA \code{climactor_id}.
#' @export

name_checkvar_match <- function(x, checkvar){
  base_df <- x %>%
    mutate(name_mod = stri_trans_general(tolower(name), id = "Latin-ASCII"))%>%
    left_join(., key_dict %>% select(right, iso, climactor_id) %>% distinct(),
              by = c("name"="right", "iso"))%>%
    rename(climactor_id_right = climactor_id)%>%
    left_join(., key_dict %>% select(right, iso, entity_type, climactor_id) %>% mutate(right = tolower(right)) %>% distinct(),
              by = c("name_mod"="right", "iso"))%>%
    rename(climactor_id_rightn = climactor_id)%>%
    left_join(., key_dict %>% select(wrong, iso, climactor_id) %>% distinct(),
              by = c("name"="wrong", "iso"))%>%
    rename(climactor_id_wrong = climactor_id)%>%
    left_join(., key_dict %>% select(wrong, iso, climactor_id) %>% mutate(wrong= tolower(wrong)) %>% distinct(),
              by = c("name_mod"="wrong", "iso"))%>%
    rename(climactor_id_wrongn = climactor_id)%>% distinct()

  eval_df <- base_df %>%
    mutate(eval_right_rightn = (climactor_id_right == climactor_id_rightn))%>%
    mutate(eval_rightn_wrong = (climactor_id_rightn == climactor_id_wrong))%>%
    mutate(eval_wrong_wrongn = (climactor_id_wrong == climactor_id_wrongn))%>%
    mutate(eval_right_wrong = (climactor_id_right == climactor_id_wrong))%>%
    mutate(eval_right_wrongn = (climactor_id_right == climactor_id_wrongn))%>%
    mutate(eval_rightn_wrongn = (climactor_id_rightn == climactor_id_wrongn))%>%
    group_by(name, iso)%>%
    mutate_at(vars(starts_with("eval_")), ~ as.integer(.))%>%
    add_count()%>% ungroup() %>% mutate(row_id=row_number())

  m1_df <- eval_df%>%
    filter(n == 1)%>%
    filter(eval_right_rightn & eval_rightn_wrong & eval_wrong_wrongn & eval_right_wrong & eval_right_wrongn & eval_rightn_wrongn)%>%
    mutate(climactor_id = coalesce(climactor_id_right, climactor_id_rightn, climactor_id_wrong, climactor_id_wrongn))%>%
    filter(!is.na(climactor_id))

  m2_df<- eval_df%>%
    filter(!row_id  %in% m1_df$row_id)%>%
    mutate(no_false = rowMeans(select(., starts_with("eval_")), na.rm = TRUE))%>%
    filter(n == 1)%>%
    filter(no_false == 1)%>%
    mutate(climactor_id = coalesce(climactor_id_right, climactor_id_rightn, climactor_id_wrong, climactor_id_wrongn))%>%
    filter(!is.na(climactor_id))

  m3_df <- eval_df%>%
    filter(!row_id  %in% m1_df$row_id)%>%
    filter(!row_id  %in% m2_df$row_id)%>%
    mutate(no_false = rowMeans(select(., starts_with("eval_")), na.rm = TRUE))%>%
    filter(n == 1)%>%
    filter(no_false == "NaN")%>%
    mutate(climactor_id = coalesce(climactor_id_right, climactor_id_rightn, climactor_id_wrong, climactor_id_wrongn))%>%
    filter(!is.na(climactor_id))

  pass1_df <- bind_rows(m1_df, m2_df, m3_df)%>%
    select(name, iso, climactor_id)

  full_pass1_df<- x  %>%
    rename("{checkvar}_check" := checkvar)%>%
    left_join(pass1_df, by = c("name", "iso"))%>%
    left_join(key_dict %>% select(climactor_id, checkvar) %>%
              distinct(), by = 'climactor_id')

  return(full_pass1_df)

}
