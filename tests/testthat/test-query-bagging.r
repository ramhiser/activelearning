context("Query by Bagging")

x <- data.matrix(iris[, -5])
y <- iris$Species

# Issue #9
test_that("Handle errors from classifiers", {
  y_missing <- replace(y, -c(1:10, 51:60, 101:110), NA)
  query_out <- query_bagging(x=x, y=y_missing, classifier="qda",
                             disagreement="vote_entropy", num_query=5)
  expect_true(FALSE)
})
