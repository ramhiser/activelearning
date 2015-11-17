context("Random querying")

x <- iris[, -5]
y <- iris[, 5]
y_missing <- replace(y, -c(1:10, 51:60, 101:110), NA)

test_that("Random querying works", {
  query_out <- query_random(y=y_missing)
  expect_equal(length(query_out$query), 1)

  query_out <- query_random(y=y_missing, num_query=5)
  expect_equal(length(query_out$query), 5)
})
