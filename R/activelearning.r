#' A Collection of Active Learning Methods in R
#'
#' Active learning is a machine learning paradigm for optimally choosing
#' unlabeled observations in a training data set to query for their true
#' labels.  The framework is particularly useful when there are very few
#' labeled observations relative to a large number of unlabeled observations,
#' and the user seeks to determine as few true labels as possible to achieve
#' highly accurate classifiers. This package is a collection of various active
#' learning methods from the literature to optimally query observations with
#' respect to a variety of objective functions. Some active learning methods
#' require posterior probability estimates of the unlabeled observations from a
#' single classifier or a committee of classifiers; this package allows the
#' user to specify custom classifiers. An excellent literature survey has been
#' provided by Dr. Burr Settles.
#' 
#' @docType package
#' @name activelearning
#' @aliases activelearning package-activelearning
NULL
