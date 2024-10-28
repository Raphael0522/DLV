#' Title read DLV file and format footnotes.
#'
#' @param env_list An object that must contain study root, program name, program side.
#' @param output_name TLF output name.
#' @param dlv_full_path Assigned DLV full path, including file name.
#'
#' @return A list contains page footnotes.
#' @export footers_from_dlv
#'
# @examples
footers_from_dlv <- function (env_list, output_name, dlv_full_path = NULL){

  if (dlv_full_path == '' || is.na(dlv_full_path) || is.null(dlv_full_path) ){
    in_dlv_full_path <- NULL
  }else {
    in_dlv_full_path <- dlv_full_path
  }

  dlvOutput <- read_dlv(study_root = env_list$study_root, pgm_name = env_list$pgm_name,
                        pgm_side = env_list$pgm_side, output_name = {{output_name}},
                        dlv_full_path = in_dlv_full_path)$dlvOutput

  # Generate text for 'source'
  footnotes <- dlvOutput %>% select(starts_with("fn"))
  footnotes_t <- t(footnotes)
  footnotes_t <- footnotes_t[!is.na(footnotes_t)]
  footer_vector <- c(footnotes_t)

  return(footer_vector)
}
