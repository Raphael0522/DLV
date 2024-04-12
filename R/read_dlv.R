read_dlv <- function(output_name = NULL){

  in_output_name <- output_name
  env_list <- get('env_list', envir = .GlobalEnv)
  dlvFilePath <- paste(env_list$study_root,"/documents/dlv_read_only.xlsx",sep = "")

  if (!is.null(output_name)){
    if (!(exists("runall_mode") && runall_mode == 'Y')){
      dlvGlobal<- read_excel(dlvFilePath, sheet = "global",.name_repair = gfuncs$file_name_repair)
      dlvOutput<- read_excel(dlvFilePath, sheet = "outputs", .name_repair = gfuncs$file_name_repair)
      dlvLib<- read_excel(dlvFilePath, sheet = "lib",.name_repair = gfuncs$file_name_repair)
      select_pgm_name <- env_list$pgm_name
    } else{
      select_pgm_name <- runall_exec_pgm
    }

    if (env_list$pgm_side=='qc'){
      select_pgm_name <- substring(env_list$pgm_name,1,str_length(env_list$pgm_name)-2)
    }

    dlvOutput <- dlvOutput  %>% filter (program_name == select_pgm_name & output_name == in_output_name)
    if (NROW(dlvOutput) == 0){
      stop(paste0("TFL not found by using program name =' ", env_list$pgm_name, "' and output name ='", output_name ,"'."))
    }
    source_datasets <- dlvOutput %>% select(source_data) %>% str_replace_all(" ", "") %>% tolower()
  }else {
    dlvGlobal<- read_excel(dlvFilePath, sheet = "global",.name_repair = gfuncs$file_name_repair)
    dlvOutput<- read_excel(dlvFilePath, sheet = "outputs", .name_repair = gfuncs$file_name_repair)
    dlvLib<- read_excel(dlvFilePath, sheet = "lib",.name_repair = gfuncs$file_name_repair)
  }

  dlvOutput <- dlvOutput |> filter(program_name != 'Program Name')

  rtnList <- list(`dlvFilePath` = dlvFilePath, `dlvGlobal` = dlvGlobal, `dlvOutput` = dlvOutput,
                  `dlvLib` = dlvLib)

  rtnList
}
