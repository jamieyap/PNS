library(testthat)
library(dplyr)
path.output_data <- Sys.getenv("path.output_data")

# -----------------------------------------------------------------------------
# Check Output Data: Data for analyses
# -----------------------------------------------------------------------------

all.studies <- c("PNS", "BreakFree")

for(i in 1:length(all.studies)){
  
  use.study <- all.studies[i]
  
  context(paste("Sanity check", use.study, "output data"))
  
  test_that("Either min.time and max.time are both NA or not NA",{
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv"))  
    true.conditions <- (is.na(df[,"max.time"]) & is.na(df[,"min.time"])) | (!is.na(df[,"max.time"]) & !is.na(df[,"min.time"]))
    actual.result <- sum(!true.conditions)
    expected.result <- 0
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  test_that("Check non NA entries of first intervals",{
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv")) 
    df <- df[which((df[,"assessment.id"]==1) & (df[,"smoking.label"]=="UNKNOWN") & (df[,"interval.duration.hours"]<=24) & (df[,"with.timing.conflict"]==0)),]
    
    true.conditions <-  !is.na(df[,"num.cigs.smoked"]) & !is.na(df[,"min.time"]) & !is.na(df[,"max.time"])
    actual.result <- sum(true.conditions)
    expected.result <- nrow(df)
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  test_that("Check non NA entries of first intervals",{
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv")) 
    df <- df[which((df[,"assessment.id"]==1) & (df[,"smoking.label"]=="UNKNOWN") & (df[,"interval.duration.hours"]>24)),]
    
    true.conditions <-  is.na(df[,"num.cigs.smoked"]) & is.na(df[,"min.time"]) & is.na(df[,"max.time"])
    actual.result <- sum(true.conditions)
    expected.result <- nrow(df)
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  test_that("Check NA and non-NA entries after first intervals",{
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv")) 
    df <- df[which((df[,"assessment.id"]>1) & (df[,"smoking.label"]=="NO")),]
    true.conditions <-  is.na(df[,"min.time"]) & is.na(df[,"max.time"]) & (df[,"num.cigs.smoked"]==0)
    actual.result <- sum(true.conditions)
    expected.result <- nrow(df)
    expect_equal(object = actual.result, expected = expected.result)
    
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv"))
    df <- df[which((df[,"assessment.id"]>1) & (df[,"smoking.label"]=="UNKNOWN")),]
    true.conditions <-  is.na(df[,"min.time"]) & is.na(df[,"max.time"]) & is.na(df[,"num.cigs.smoked"])
    actual.result <- sum(true.conditions)
    expected.result <- nrow(df)
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  test_that("After the first interval, with.timing.conflict should be NA when smoking.label==UNKNOWN or NO",{
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv"))
    df <- df[which(df[,"assessment.id"]>1 & df[,"smoking.label"]!="YES"),]
    
    true.conditions <- is.na(df$with.timing.conflict)
    actual.result <- sum(true.conditions)
    expected.result <- nrow(df)
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  test_that("At the first interval, with.timing.conflict should be NA when smoking.label==UNKNOWN & interval.duration.hours>24 or NO",{
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv"))
    df <- df[which(df[,"assessment.id"]==1 & df[,"smoking.label"]=="NO"),]
    true.conditions <- is.na(df$with.timing.conflict)
    actual.result <- sum(true.conditions)
    expected.result <- nrow(df)
    expect_equal(object = actual.result, expected = expected.result)
    
    df <- read.csv(file.path(path.output_data, use.study, "out_smoking_timing.csv"))
    df <- df[which(df[,"assessment.id"]==1 & df[,"smoking.label"]=="UNKNOWN" & df[,"interval.duration.hours"]>24),]
    true.conditions <- is.na(df$with.timing.conflict)
    actual.result <- sum(true.conditions)
    expected.result <- nrow(df)
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  #############################################################################
  #############################################################################
  #############################################################################
  
  test_that("LB should come before UB",{
    df <- read.csv(file.path(path.output_data, use.study, "df.analysis.02a.csv"))  
    actual.result <- sum(df[,"UB.seconds"] - df[,"LB.seconds"] < 0)
    expected.result <- 0
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  
  test_that("Lower bound of time intervals are in ascending order",{
    df <- read.csv(file.path(path.output_data, use.study, "df.analysis.02a.csv"))  
    
    ids.participants <- unique(df$id)
    n.participants <- length(ids.participants)
    
    # Check LB.seconds
    tmp.list <- list()
    for(i in 1:n.participants){
      # Obtain subset of rows corresponding to participant i
      this.group <- which(df$id==ids.participants[i])
      df.this.group <- df[this.group,]
      orig.order <- 1:nrow(df.this.group)
      new.order <- order(df.this.group[,"LB.seconds"])
      this.group.result <- sum(new.order != orig.order)
      this.list <- list(list(id = ids.participants[i], this.group.result = this.group.result))
      tmp.list <- append(tmp.list, this.list)
    }
    
    tmp.df <- bind_rows(tmp.list)
    
    actual.result <- sum(tmp.df$this.group.result)
    expected.result <- 0
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  test_that("Upper bound of time intervals are in ascending order",{
    df <- read.csv(file.path(path.output_data, use.study, "df.analysis.02a.csv"))  
    
    ids.participants <- unique(df$id)
    n.participants <- length(ids.participants)
    
    # Check UB.seconds
    tmp.list <- list()
    for(i in 1:n.participants){
      # Obtain subset of rows corresponding to participant i
      this.group <- which(df$id==ids.participants[i])
      df.this.group <- df[this.group,]
      orig.order <- 1:nrow(df.this.group)
      new.order <- order(df.this.group[,"UB.seconds"])
      this.group.result <- sum(new.order != orig.order)
      this.list <- list(list(id = ids.participants[i], this.group.result = this.group.result))
      tmp.list <- append(tmp.list, this.list)
    }
    
    tmp.df <- bind_rows(tmp.list)
    
    actual.result <- sum(tmp.df$this.group.result)
    expected.result <- 0
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  test_that("Check whether LB of current interval equals UB of previous interval",{
    df <- read.csv(file.path(path.output_data, use.study,"df.analysis.02a.csv"))  
    
    ids.participants <- unique(df$id)
    n.participants <- length(ids.participants)
    
    tmp.list <- list()
    for(i in 1:n.participants){
      # Obtain subset of rows corresponding to participant i
      this.group <- which(df$id==ids.participants[i])
      df.this.group <- df[this.group,]
      compare1 <- tail(df.this.group[,"LB.seconds"], n=-1)
      compare2 <- head(df.this.group[,"UB.seconds"], n=-1)
      this.group.result <- sum(compare1 != compare2)
      this.list <- list(list(id = ids.participants[i], this.group.result = this.group.result))
      tmp.list <- append(tmp.list, this.list)
    }
    
    tmp.df <- bind_rows(tmp.list)
    
    actual.result <- sum(tmp.df$this.group.result)
    expected.result <- 0
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  
  
}

