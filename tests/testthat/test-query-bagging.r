context("Query by Bagging")

x <- iris[, -5]
y <- iris[, 5]
y_missing <- replace(y, -c(1:10, 51:60, 101:110), NA)

test_that("QBB works with Kullback disagreement", {
  fit_f <- function(x, y, ...) {
    MASS::lda(x, y, ...)
  }
  predict_f <- function(object, x) {
    predict(object, x)$class
  }

  query_out <- query_bagging(x=x, y=y_missing, fit=fit_f, predict=predict_f,
                             disagreement="kullback", C=10)
  expect_equal(length(query_out$query), 1)
})

test_that("QBB works with vote-entropy disagreement", {
  fit_f <- function(x, y, ...) {
    MASS::lda(x, y, ...)
  }
  predict_f <- function(object, x) {
    predict(object, x)$class
  }

  set.seed(42)
  query_out <- query_bagging(x=x, y=y_missing, fit=fit_f, predict=predict_f,
                             disagreement="vote_entropy", C=5)
  expect_equal(length(query_out$query), 1)
})

test_that("QBB works with posterior-entropy disagreement", {
  fit_f <- function(x, y, ...) {
    MASS::lda(x, y, ...)
  }
  predict_f <- function(object, x) {
    predict(object, x)$class
  }

  query_out <- query_bagging(x=x, y=y_missing, fit=fit_f, predict=predict_f,
                             disagreement="post_entropy", C=10)
  expect_equal(length(query_out$query), 1)
})

# Issue #9
test_that("Handle errors from classifiers", {
  # TODO: Update this to match new interface but use QDA like in Issue #9.
  y_missing <- replace(y, -c(1:10, 51:60, 101:110), NA)
  query_out <- query_bagging(x=x, y=y_missing, classifier="qda",
                             disagreement="vote_entropy", num_query=5)
  expect_equal(length(query_out$query), 1)
})
