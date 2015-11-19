context("Query by Committee")

x <- iris[, -5]
y <- iris[, 5]
y_missing <- replace(y, -c(1:10, 51:60, 101:110), NA)

test_that("QBC works with Kullback disagreement", {
  fit_committee <- list(
    lda=function(x, y) { MASS::lda(x, y) },
    qda=function(x, y) { MASS::qda(x, y) },
    random_forest=function(x, y) { randomForest::randomForest(x, y, ntree=50, maxnodes=5) }
  )

  predict_committee <- list(
    lda=function(object, x) { predict(object, x)$posterior },
    qda=function(object, x) { predict(object, x)$posterior },
    random_forest=function(object, x) { predict(object, x, type="prob") }
  )

  set.seed(42)
  query_out <- query_committee(x=x, y=y_missing,
                               fit_committee=fit_committee,
                               predict_committee=predict_committee,
                               disagreement="kullback")
  expect_equal(length(query_out$query), 1)

  query_out <- query_committee(x=x, y=y_missing,
                               fit_committee=fit_committee,
                               predict_committee=predict_committee,
                               disagreement="kullback",
                               num_query=5)
  expect_equal(length(query_out$query), 5)
})

test_that("QBC works with vote-entropy disagreement", {
  fit_committee <- list(
    lda=function(x, y) { MASS::lda(x, y) },
    qda=function(x, y) { MASS::qda(x, y) },
    random_forest=function(x, y) { randomForest::randomForest(x, y, ntree=50, maxnodes=5) }
  )

  predict_committee <- list(
    lda=function(object, x) { predict(object, x)$class },
    qda=function(object, x) { predict(object, x)$class },
    random_forest=function(object, x) { predict(object, x, type="response") }
  )

  set.seed(42)
  query_out <- query_committee(x=x, y=y_missing,
                               fit_committee=fit_committee,
                               predict_committee=predict_committee,
                               disagreement="vote_entropy")
  expect_equal(length(query_out$query), 1)

  query_out <- query_committee(x=x, y=y_missing,
                               fit_committee=fit_committee,
                               predict_committee=predict_committee,
                               disagreement="vote_entropy",
                               num_query=5)
  expect_equal(length(query_out$query), 5)
})

test_that("QBC works with posterior-entropy disagreement", {
  fit_committee <- list(
    lda=function(x, y) { MASS::lda(x, y) },
    qda=function(x, y) { MASS::qda(x, y) },
    random_forest=function(x, y) { randomForest::randomForest(x, y, ntree=50, maxnodes=5) }
  )

  predict_committee <- list(
    lda=function(object, x) { predict(object, x)$posterior },
    qda=function(object, x) { predict(object, x)$posterior },
    random_forest=function(object, x) { predict(object, x, type="prob") }
  )

  set.seed(42)
  query_out <- query_committee(x=x, y=y_missing,
                               fit_committee=fit_committee,
                               predict_committee=predict_committee,
                               disagreement="post_entropy")
  expect_equal(length(query_out$query), 1)

  query_out <- query_committee(x=x, y=y_missing,
                               fit_committee=fit_committee,
                               predict_committee=predict_committee,
                               disagreement="post_entropy",
                               num_query=5)
  expect_equal(length(query_out$query), 5)
})
