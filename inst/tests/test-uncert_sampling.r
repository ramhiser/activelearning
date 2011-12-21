context("Uncertainty Sampling")

x <- data.matrix(iris[, -5])
y <- iris$Species

# Below, we use e_msg to indicate the 'error message'

test_that("An error is thrown when the specified classifier is NULL", {
  e_msg <- 'A classifier must be specified'
  expect_error(
               uncert_sampling(x = x, y = y, classifier = NULL),
               e_msg
               )
  expect_error(
               uncert_sampling(x = x, y = y, uncertainty = "least_confidence",
                               classifier = NULL),
               e_msg
               )
})
test_that("An error is thrown when the specified classifier is NA", {
  e_msg <- 'A classifier must be specified'
  expect_error(
               uncert_sampling(x = x, y = y, classifier = NA),
               e_msg
               )
  expect_error(
               uncert_sampling(x = x, y = y, uncertainty = "least_confidence",
                               classifier = NA),
               e_msg
               )
})

test_that("An error occurs when the classifier is not found in 'caret'", {
  classifier <- "wtf"
  e_msg <- paste("Cannot find, ", classifier, " in the 'caret' package", sep = "'")

  expect_error(
               uncert_sampling(x = x, y = y, classifier = classifier),
               e_msg
               )
  expect_error(
               uncert_sampling(x = x, y = y, uncertainty = "margin",
                               classifier = classifier),
               e_msg
               )
})
