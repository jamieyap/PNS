context("Construction of smoking intervals")

test_that(desc = "LB should come before UB", code = {
  actual.result <- sum(df[,"UB.ts"] - df[,"LB.ts"] < 0)
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})


test_that("Lower bound of time intervals are in ascending order",{
  df.out <- df %>% 
    group_by(id) %>% 
    do(
      t = .$LB.ts,
      actual.order = order(.$LB.ts),
      expected.order = 1:nrow(.),
      compare = sum((order(.$LB.ts)) != (1:nrow(.)))
    )
  
  actual.result <- sum(unlist(df.out$compare))
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})


test_that("Upper bound of time intervals are in ascending order",{
  df.out <- df %>% 
    group_by(id) %>% 
    do(
      t = .$UB.ts,
      actual.order = order(.$UB.ts),
      expected.order = 1:nrow(.),
      compare = sum((order(.$UB.ts)) != (1:nrow(.)))
    )
  
  actual.result <- sum(unlist(df.out$compare))
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})


test_that("Check whether LB of current interval equals UB of previous interval",{
  df.out <- df %>% 
    group_by(id) %>% 
    do(
      array1 = tail(.$LB.ts, n=-1),
      array2 = head(.$UB.ts, n=-1),
      compare = sum(tail(.$LB.ts, n=-1) != head(.$UB.ts, n=-1))
    )
  
  actual.result <- sum(unlist(df.out$compare))
  expected.result <- 0
  expect_equal(object = actual.result, expected = expected.result)
})


