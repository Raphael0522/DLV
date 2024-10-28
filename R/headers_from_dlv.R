#' Title To read DLV file and format page header and titles.
#'
#' @param env_list An object that must contain study root, program name, program side.
#' @param output_name TLF output name.
#' @param dlv_full_path Assigned DLV full path, including file name.
#'
#' @return A list contains page header and titles.
#' @import stringr
#' @importFrom purrr as_vector
#' @export headers_from_dlv
#'
# @examples
headers_from_dlv <- function (env_list, output_name, dlv_full_path = NULL){

  # source_data <- output_nr <- ttl3 <- ttl4 <- NULL

  if (dlv_full_path == '' || is.na(dlv_full_path) || is.null(dlv_full_path) ){
    in_dlv_full_path <- NULL
  }else {
    in_dlv_full_path <- dlv_full_path
  }

  # env_list <- get('env_list', envir = .GlobalEnv)
  dlvOutput <- read_dlv(study_root = env_list$study_root, pgm_name = env_list$pgm_name,
                        pgm_side = env_list$pgm_side, output_name = {{output_name}},
                        dlv_full_path = in_dlv_full_path)$dlvOutput
  # Generate text for 'output name'
  output_name_text <- paste0("[Output: ", env_list$pgm_name, output_name, ".rtf", "]")
  # Generate text for program path/name and output name
  pgm_path_output_name_text <- paste0("Program: ", env_list$pgm_path, " ", output_name_text)

  # Assign output status
  output_status <- env_list$output_status

  # Deliverable Name
  deliverable_text <- env_list$deliverable_name

  # Generate text for 'source'
  source_data_list <- dlvOutput %>% dplyr::select("source_data") %>%
    stringr::str_replace_all(" ", "") %>% tolower()
  data_source_text <- paste0("Source: ", source_data_list)

  # Generate text for table title
  tbl_number <- dlvOutput %>% dplyr::select("output_nr")

  # Generate text for table title
  tbl_title <- dlvOutput %>% dplyr::select("ttl3")

  tbl_titles <- dlvOutput %>% dplyr::select(matches("^ttl"))
  tbl_titles <- tbl_titles |> purrr::as_vector()
  tbl_titles <- tbl_titles[!is.na(tbl_titles)] |> as.list()
  names(tbl_titles) <- NULL

  message(tbl_titles)

  # Generate text for population
  population <- dlvOutput %>% dplyr::select("ttl4")
  text_list = list(first_line = list(pgm_path_output_name_text, output_status),
                   second_line = list(deliverable_text, tbl_number ,data_source_text),
                   titles = tbl_titles)

  text_list
}




