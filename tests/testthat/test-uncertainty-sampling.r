context("Uncertainty Sampling")

x <- data.matrix(iris[, -5])
y <- iris$Species

# Below, we use e_msg to indicate the 'error message'

test_that("An error is thrown when the specified classifier is NULL", {
  e_msg <- 'A classifier must be specified'
  expect_error(
               uncertainty_sampling(x=x, y=y, classifier=NULL),
               e_msg
               )
  expect_error(
               uncertainty_sampling(x=x, y=y, uncertainty="least_confidence",
                                    classifier=NULL),
               e_msg
               )
})
test_that("An error is thrown when the specified classifier is NA", {
  e_msg <- 'A classifier must be specified'
  expect_error(
               uncertainty_sampling(x=x, y=y, classifier=NA),
               e_msg
               )
  expect_error(
               uncertainty_sampling(x=x, y=y, uncertainty="least_confidence",
                                    classifier=NA),
               e_msg
               )
})

test_that("An error occurs when the classifier is not found in 'caret'", {
  classifier <- "wtf"
  e_msg <- paste("Cannot find, ", classifier, " in the 'caret' package", sep="'")

  expect_error(
               uncertainty_sampling(x=x, y=y, classifier=classifier),
               e_msg
               )
  expect_error(
               uncertainty_sampling(x=x, y=y, uncertainty="margin",
                                    classifier=classifier),
               e_msg
               )
})

test_that("uncertainty_sampling works correctly with the LDA classifier and the iris data set", {
  require('MASS')
  seed <- 42
  num_unlabeled <- 50

  set.seed(seed)
  x <- data.matrix(iris[, -5])
  y <- iris$Species

  # We randomly select observations that are unlabeled and replace the labels
  # with NA to designate that they are unlabeled.
  unlabeled <- sample(seq_along(y), num_unlabeled)
  y[unlabeled] <- NA

  # Manually classify the labeled observations and predict the unlabeled
  # observations with the 'lda' function in the 'MASS' package.
  lda_out <- MASS:::lda(x=x[-unlabeled, ], grouping=y[-unlabeled])
  lda_pred <- predict(lda_out, newdata=x[unlabeled, ])
  lda_posterior <- lda_pred$posterior

  # Now, we calculate the three uncertainty sampling measures from the returned
  # posterior probabilities from LDA.
  lda_least_conf <- apply(lda_posterior, 1, max)
  lda_margin <- apply(lda_posterior, 1, function(post_i) {
      post_i[order(post_i, decreasing=T)[1:2]] %*% c(1, -1)
  })
  lda_entropy <- apply(lda_posterior, 1, entropy.plugin)

  al_least_conf <- uncertainty_sampling(x=x, y=y,
                                        classifier="lda",
                                        uncertainty="least_confidence")
  al_margin <- uncertainty_sampling(x=x, y=y,
                                    classifier="lda",
                                    uncertainty="margin")
  al_entropy <- uncertainty_sampling(x=x, y=y,
                                     classifier="lda",
                                     uncertainty="entropy")

  # Tests the equality of the uncertainy measures
  # TODO: Check the 'caret' implementation. This fails at the moment.
  # NOTE: The uncertainty scores are off by only a little bit.
  expect_equal(lda_least_conf, al_least_conf$least_confidence)
  expect_equal(lda_margin, al_margin$margin)
  expect_equal(lda_entropy, al_entropy$entropy)

  # TODO:
  # Test that the posterior probabilities from MASS:::lda match those from activelearning.
  # Tests that the observations to be queried are the same with both methods.
})
