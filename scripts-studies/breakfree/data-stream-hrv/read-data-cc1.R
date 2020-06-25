library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)
library(jsonlite)

path.breakfree.cc1.input_data <- Sys.getenv("path.breakfree.cc1.input_data")
path.breakfree.staged_data <- Sys.getenv("path.breakfree.staged_data")
path.breakfree.output_data <- Sys.getenv("path.breakfree.output_data")
path.breakfree.code <- Sys.getenv("path.breakfree.code")
path.shared.code <- Sys.getenv("path.shared.code")

source(file.path(path.shared.code, "shared-data-manip-utils.R"))

# Participant IDs for data collected using CC1 platform
ids.cc1 <- list.files(path = path.breakfree.cc1.input_data)

# -----------------------------------------------------------------------------
# Read raw data
# -----------------------------------------------------------------------------
list.df.raw <- list()

for(i in 1:length(ids.cc1)){
  this.id <- ids.cc1[i]
  
  # List all file names within folder corresponding to this.id
  all.files <- list.files(file.path(path.breakfree.cc1.input_data, this.id))
  # Pick out file names related to data stream of interest
  idx <- grepl(pattern = "CSTRESS_FEATURE_VECTOR+PHONE.csv.zip", x = all.files, fixed=TRUE)
  # Pick out corresponding files
  this.file <- all.files[idx]
  len <- length(this.file)
  
  # Check whether file exists for this given participant
  if(len>0){
    # Create a new temporary folder named tmp_unzipped
    # tmp_unzipped will contain this.file
    # this.file is a zipped file which will be unzipped with unzip()
    unzip(zipfile = file.path(path.breakfree.cc1.input_data, this.id, this.file),
          exdir =  file.path(path.breakfree.staged_data, "tmp_unzipped"),
          overwrite = TRUE)
    
    # The call to unzip() will produce folder.here inside tmp_unzipped
    # folder.here is a folder which contains the csv file file.here
    # file.here is the csv file we would like to read for participant this.id
    folder.here <- list.files(path = file.path(path.breakfree.staged_data, "tmp_unzipped"))
    file.here <- list.files(path = file.path(path.breakfree.staged_data, "tmp_unzipped", folder.here))
    idx <- grep("CSTRESS_FEATURE_VECTOR+PHONE.csv.zip", file.here, invert = FALSE)
    
    # Finally, we have located file.here
    # Now, let's read this csv file into the df.raw variable
    df.raw <- try(read.csv(file.path(path.breakfree.staged_data, "tmp_unzipped", folder.here, file.here), 
                           header = FALSE), 
                  silent=TRUE)
    
    # A try-error occurs when file.here exists but does not have any data recorded
    if(class(df.raw) == "try-error"){
      unlink(x = file.path(path.breakfree.staged_data, "tmp_unzipped"), recursive = TRUE)
      next
    }else{
      # Add column to record participant ID
      df.raw <- df.raw %>% mutate(user.id = this.id) %>% select(user.id, everything())
      # Clean up afterwards: unlink() deletes the folder tmp_unzipped
      unlink(x = file.path(path.breakfree.staged_data, "tmp_unzipped"), recursive = TRUE)
      # Add df.raw to collection
      list.df.raw <- append(list.df.raw, list(df.raw))
    }
  }else{
    # In this case, the file  does not exist
    next
  }
}

# -----------------------------------------------------------------------------
# Read raw data labels from a JSON file
# -----------------------------------------------------------------------------
this.id <- ids.cc1[1]
# List all file names within folder corresponding to this.id
all.files <- list.files(file.path(path.breakfree.cc1.input_data, this.id))
# Pick out file names related to data stream of interest
idx <- grepl(pattern = "CSTRESS_FEATURE_VECTOR+PHONE.json", x = all.files, fixed=TRUE)
# Pick out corresponding files
this.file <- all.files[idx]
# Read JSON file and pull out portion describing the contents of the file CSTRESS_FEATURE_VECTOR+PHONE.csv.zip
this.json <- fromJSON(file.path(path.breakfree.cc1.input_data, this.id, this.file), flatten=FALSE)
datadescriptor <- this.json$datadescriptor
# Correct data entry errors
datadescriptor[34,"STATISTIC"] <- "quartile deviation"
datadescriptor[35,"STATISTIC"] <- "average"
datadescriptor[36,"STATISTIC"] <- "median"

# -----------------------------------------------------------------------------
# Save objects into an RData file
# -----------------------------------------------------------------------------
list.df.raw.hrv.cc1 <- list.df.raw
datadescriptor.cc1 <- datadescriptor

remove(list.df.raw, datadescriptor)
save(list.df.raw.hrv.cc1, datadescriptor.cc1, file = file.path(path.breakfree.staged_data, "raw.hrv.cc1.RData"))

