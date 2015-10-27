# activelearning

Active learning is a machine-learning paradigm for optimally choosing unlabeled
observations in a training data set to query for their true labels. The
framework is particularly useful when there are very few labeled observations
relative to a large number of unlabeled observations, and the user seeks to
determine as few true labels as possible to achieve highly accurate
classifiers.

The `activelearning` R package is a collection of various active-learning
methods from the literature to optimally query observations with respect to a
variety of objective functions. Some active learning methods require posterior
probability estimates of the unlabeled observations from a single classifier or
a committee of classifiers; this package allows the user to specify custom
classifiers or specify a variety of classifiers by interfacing with the
[`caret` R package](https://github.com/topepo/caret).

The `activelearning` package implements the active learning methods as defined
by the excellent
[literature survey](http://www.cs.cmu.edu/~bsettles/pub/settles.activelearning.pdf)
from [Dr. Burr Settles](http://www.cs.cmu.edu/~bsettles/). This literature
survey is also available in
[book form](http://www.amazon.com/Learning-Synthesis-Lectures-Artificial-Intelligence/dp/1608457257),
which is highly recommended.

## Installation

You can install the stable version on [CRAN](http://cran.r-project.org/package=activelearning):

```r
install.packages('activelearning', dependencies = TRUE)
```

If you prefer to download the latest version, instead type:

```r
library(devtools)
devtools::install_github('ramhiser/activelearning')
```
