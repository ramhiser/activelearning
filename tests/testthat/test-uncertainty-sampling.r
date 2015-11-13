context("Uncertainty Sampling")

x <- iris[, -5]
y <- iris[, 5]
y_missing <- replace(y, -c(1:10, 51:60, 101:110), NA)

# Below, we use e_msg to indicate the 'error message'

test_that("An error is thrown when the specified classifier is NULL", {
  e_msg <- 'A classifier must be specified'
  expect_error(uncertainty_sampling(x=x, y=y, classifier=NULL),
               e_msg)
  expect_error(uncertainty_sampling(x=x, y=y, uncertainty="least_confidence",
                                    classifier=NULL),
               e_msg)
})
test_that("An error is thrown when the specified classifier is NA", {
  e_msg <- 'A classifier must be specified'
  expect_error(uncertainty_sampling(x=x, y=y, classifier=NA),
               e_msg)
  expect_error(uncertainty_sampling(x=x, y=y, uncertainty="least_confidence",
                                    classifier=NA),
               e_msg)
})

test_that("An error occurs when the classifier is not found in 'caret'", {
  classifier <- "wtf"
  e_msg <- paste("Cannot find, ", classifier, " in the 'caret' package", sep="'")

  expect_error(uncertainty_sampling(x=x, y=y, classifier=classifier),
               e_msg)
  expect_error(uncertainty_sampling(x=x, y=y, uncertainty="margin",
                                    classifier=classifier),
               e_msg)
})

test_that("uncertainty_sampling works correctly with the LDA classifier and the iris data set", {
  require('MASS')

  split_out <- activelearning:::split_labeled(x, y_missing)

  # Manually classify the labeled observations and predict the unlabeled
  # observations with the 'lda' function in the 'MASS' package.
  lda_out <- MASS:::lda(x=split_out$x_labeled, grouping=split_out$y_labeled)
  lda_pred <- predict(lda_out, newdata=split_out$x_unlabeled)
  lda_posterior <- lda_pred$posterior

  # Now, we calculate the three uncertainty sampling measures from the returned
  # posterior probabilities from LDA.
  lda_least_conf <- apply(lda_posterior, 1, max)
  lda_margin <- apply(lda_posterior, 1, function(post_i) {
    post_i[order(post_i, decreasing=T)[1:2]] %*% c(1, -1)
  })
  lda_entropy <- apply(lda_posterior, 1, entropy.plugin)

  al_least_conf <- uncertainty_sampling(x=x, y=y_missing,
                                        classifier="lda",
                                        uncertainty="least_confidence")
  al_margin <- uncertainty_sampling(x=x, y=y_missing,
                                    classifier="lda",
                                    uncertainty="margin")
  al_entropy <- uncertainty_sampling(x=x, y=y_missing,
                                     classifier="lda",
                                     uncertainty="entropy")

  # Tests the equality of the uncertainy measures
  expect_equal(lda_least_conf, al_least_conf$uncertainty)
  expect_equal(lda_margin, al_margin$uncertainty)
  expect_equal(lda_entropy, al_entropy$uncertainty)
})
