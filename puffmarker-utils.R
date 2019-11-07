# About this script: Contains functions to extract puffmarker data from zip files
# and perform data manipulation tasks

ExtractThisInfo <- function(this.participant, use.study=this.study, use.info=this.info){
  
  unzip(zipfile = file.path(path.input_data, use.study, folder.puffmarker_data, this.participant),
        exdir =  file.path(path.input_data, use.study, "tmp_unzipped"),
        overwrite = TRUE)
  files.here <- list.files(path = file.path(path.input_data, use.study, "tmp_unzipped"))
  idx <- grep(use.info, files.here, invert = FALSE)
  
  df <- try(read.csv(file.path(path.input_data, use.study, "tmp_unzipped", files.here[idx]), header = FALSE), silent = TRUE)
  
  if(class(df) == "try-error"){
    
    unlink(x = file.path(path.input_data, use.study, "tmp_unzipped"), recursive = TRUE)
    return(NULL)
    
  }else{ # class(df) == "data.frame"
    
    df$id <- substring(this.participant, first = 1, last = nchar(this.participant)-4)
    # Delete the folder tmp_unzipped and all of its contents
    unlink(x = file.path(path.input_data, use.study, "tmp_unzipped"), recursive = TRUE)
    return(df)
    
  }
}


