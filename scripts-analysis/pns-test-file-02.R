context("Construction of Post-Quit Random or Urge EMA Datasets")

test_that(desc = "No missing time.unixts timestamps", code = {
  actual.result <- sum(1*(is.na(df$time.unixts)))
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})

test_that(desc = "No missing engaged.yes values", code = {
  actual.result <- sum(1*(is.na(df$engaged.yes)))
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})

test_that(desc = "Order of total prompts since start should match order of time.unixts timestamps", code = {
  df.out <- df %>% 
    group_by(id) %>% 
    do(
      actual.order1 = order(.$time.unixts),
      actual.order2 = order(.$total.prompts.since.start),
      compare = sum((order(.$time.unixts)) != ( order(.$total.prompts.since.start)))
    )
  
  actual.result <- sum(unlist(df.out$compare))
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})

test_that(desc = "there are no missing record.id values", code = {
  these.records <- as.character(df$record.id)
  these.records <- replace(these.records, these.records=="", NA_character_)
  actual.result <- sum(is.na(these.records))
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})

test_that(desc = "there are no duplicate record.id values", code = {
  actual.result <- length(unique(as.character(df$record.id)))
  expected.result <- nrow(df)
  expect_equal(object = actual.result, expected = expected.result)
})