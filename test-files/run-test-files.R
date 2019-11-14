library(testthat)
library(dplyr)
path.code <- Sys.getenv("path.code")

test_file(file.path(path.code, "test-files/test-file-01.R"))
test_file(file.path(path.code, "test-files/test-file-02a.R"))
test_file(file.path(path.code, "test-files/test-file-02b.R"))

test_file(file.path(path.code, "test-files/test-file-03.R"))
test_file(file.path(path.code, "test-files/test-file-04a.R"))
test_file(file.path(path.code, "test-files/test-file-04b.R"))