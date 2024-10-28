#' Read Deliverable file
#'
#' @param study_root Study root path.
#' @param pgm_name Program short name (without file extension name)
#' @param pgm_side Program Side (prod / qc)
#' @param output_name Specific output name. If null, return all the records.
#' @param dlv_full_path DLV file full path, including the file name and extension file name.
#'
#' @return A list includes global variables, outputs, libraries, and deliverable file path
#' @import dplyr
#' @import rlang
#' @export read_dlv
#'
# @examples

read_dlv <- function(study_root = NULL, pgm_name = NULL, pgm_side = NULL, output_name = NULL, dlv_full_path = NULL){

  stopifnot(!is.null(study_root) || !is.null(dlv_full_path))
  if (is.null(dlv_full_path) || is.na(dlv_full_path) || dlv_full_path == ''){
    dlvFilePath <- paste(study_root,"/documents/dlv_read_only.xlsx", sep = "")
  }else {
    dlvFilePath <- dlv_full_path
  }

  message(paste0('Start to read DLV file. DLV file path = '), dlvFilePath)
  message(paste0('Programming side = '), pgm_side)
  message(paste0('Output name = '), output_name)
  message(paste0('Program name = '), pgm_name)

  dlvGlobal<- readxl::read_excel(dlvFilePath, sheet = "global",.name_repair = file_name_repair)
  dlvOutput <- readxl::read_excel(dlvFilePath, sheet = "outputs", .name_repair = file_name_repair)
  colnames(dlvOutput) <- ifelse(is.na(colnames(dlvOutput)) | colnames(dlvOutput) == "",
                                 paste0("Missing_Column_Name_", seq_along(colnames(dlvOutput))),
                                 colnames(dlvOutput))
  dlvOutput <- dlvOutput |> dplyr::filter(.data$program_name != 'Program Name')
  dlvLib<- readxl::read_excel(dlvFilePath, sheet = "lib",.name_repair = file_name_repair)

  if (!is.null(output_name)){
    if (pgm_side=='qc'){
      select_pgm_name <- substring(pgm_name,1,str_length(pgm_name)-2)
    }else if (pgm_side=='prod'){
      select_pgm_name <- pgm_name
    }

    dlvOutput <- dlvOutput  %>% dplyr::filter (.data$program_name == select_pgm_name & .data$output_name == {{output_name}})
    if (NROW(dlvOutput) == 0){
      stop(paste0("TFL not found by using program name =' ", pgm_name, "' and output name ='", output_name ,"'."))
    }
  }

  # dlvOutput <- dlvOutput |> dplyr::filter(program_name != 'Program Name')

  rtnList <- list(`dlvFilePath` = dlvFilePath, `dlvGlobal` = dlvGlobal, `dlvOutput` = dlvOutput,
                  `dlvLib` = dlvLib)

  rtnList
}

#' Handling of column names. Ensures column names are not empty and are unique.
#'
#' @param nms Column Names
#'
#' @return List of validated column names
#' @import stringr
#'
# @examples
file_name_repair <- function(nms) {
  str_replace_all(ifelse(substr(nms, 1, 1) == '_', sub("^.", "", nms), nms),"[.]","_")
}
