library(testthat)
library(dplyr)
path.output_data <- Sys.getenv("path.output_data")

# -----------------------------------------------------------------------------
# Check Output Data
# -----------------------------------------------------------------------------

all.studies <- c("PNS", "BreakFree")

for(i in 1:length(all.studies)){
  
  use.study <- all.studies[i]
  
  context(paste("Sanity check", use.study, "output data"))
  
  test_that("Start clock should come before end clock",{
    quit.dates <- read.csv(file.path(path.output_data, use.study, "out_quit_dates.csv"))  
    actual.result <- sum(quit.dates[,"end.clock"] - quit.dates[,"start.clock"] < 0)
    expected.result <- 0
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  #############################################################################
  #############################################################################
  #############################################################################
  
  test_that("LB should come before UB",{
    df <- read.csv(file.path(path.output_data, use.study, "df.analysis.01.csv"))  
    actual.result <- sum(df[,"UB.seconds"] - df[,"LB.seconds"] < 0)
    expected.result <- 0
    expect_equal(object = actual.result, expected = expected.result)
  })
  
  
}


