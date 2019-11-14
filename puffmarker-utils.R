# About this script: Contains functions to extract puffmarker data from zip files
# and perform data manipulation tasks

ExtractThisInfo <- function(this.participant, this.study=use.study, this.info=use.info){
  
  unzip(zipfile = file.path(path.input_data, this.study, folder.puffmarker_data, this.participant),
        exdir =  file.path(path.input_data, this.study, "tmp_unzipped"),
        overwrite = TRUE)
  files.here <- list.files(path = file.path(path.input_data, this.study, "tmp_unzipped"))
  idx <- grep(this.info, files.here, invert = FALSE)
  
  df <- try(read.csv(file.path(path.input_data, this.study, "tmp_unzipped", files.here[idx]), header = FALSE), silent = TRUE)
  
  if(class(df) == "try-error"){
    
    unlink(x = file.path(path.input_data, this.study, "tmp_unzipped"), recursive = TRUE)
    return(NULL)
    
  }else{ # class(df) == "data.frame"
    
    if(this.study=="BreakFree"){
      char.adjust <- 4
    }else if(this.study=="OnTrack"){
      char.adjust <- 6
    }else{
      char.adjust <- NULL
    }
    
    df$id <- substring(this.participant, first = 1, last = nchar(this.participant)-char.adjust)
    # Delete the folder tmp_unzipped and all of its contents
    unlink(x = file.path(path.input_data, this.study, "tmp_unzipped"), recursive = TRUE)
    
    if(is.null(char.adjust)){
      df <- NULL
    }
    
    return(df)
  }
}


