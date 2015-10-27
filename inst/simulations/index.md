###### Active Learning ######
##### John Ramey, 21 September 2011 #####

# Introduction to Active Learning #

# Future Research #

## Noisy Oracles ##

In our current consideration, we assume that the oracle is infallible and will always provide the correct label. This assumption may not be reasonable for multiple reasons, some of which include:

1. The oracle is human and may provide an incorrect labeling periodically. A first approach would be to assume that the oracle has a fixed probability of success in providing the correct labels and then to model the probability with a calibration approach where we occasionally provide the oracle with observations for which we know the labeling.
2. The oracle gets bored, tired, or becomes distracted, and their accuracy degrades over time. This would increase the complexity of modeling their chance of success, but this information is important to capture because if we continue to build a model based on data that has likely been mislabeled, we may either discard the labelings if the oracle is in a **slump** (oracle slump modeling in active learning...I like it as a paper title.) or we may weight the fallible-labeled data less than data we are 100% about.
3. There are multiple oracles who may not agree on every observation, and hence, at least one of them is incorrect, or they both agree in the sense that the classifications they provided are actually subclasses of a superclass, whether or not the superclass is known a priori. An initial approach here would be to assume that each oracle has a fixed probability of success, and we wish to model it. To complicate the model, we will occasionally need to calibrate the oracles with each other and with the labeled data that we already have.
4. If one oracle can slump in labeling, then certainly, multiple oracles may go into a slump. This is more along the lines of the [Mechanical Turk project from Amazon.com][2]. The modeling here would be quite difficult, but it is worth researching because if there is a **troll effect** (that is, an oracle logs onto the website and has malicious intent, which is performed by mislabeling every presented observation), then we need to determine that this is happening.

As far as I know, little research has been conducted regarding the four above issues, but there is some literature on the topic, at least enough to name the approach "active learning with a noisy oracle." The term "noisy oracle" induces the assumption that the oracle always provides the truth, subject to noise. Although we may reference the phrase in our work, I recommend that we propose an alternative term to circumvent potential controversies with the greater scientific community. Also, I do not intend to research the psychological aspects of the oracle modeling; I simply wish to know if what they are providing is accurate. A psychologist can jump in and provide feedback about the human factors if they wish.

## Correlated Data ##

The general assumption often made regarding active learning models is that the data is independent and identically distributed from some set of populations or probability distributions. Although this assumption is widely used in all areas of statistics and machine learning, the approach does not take into account if the data are correlated, especially in a time-series setup. As far as I know, no active learning models directly account for or attempt to model autocorrelations in the data, although some active learning approaches may do this implicitly with ad-hoc reasoning. The application of active learning to time series models should not be ignored because it is plausible that there are events occurring over time and that an expert may wish to determine if that event is interesting. For example, consider city-wide or nation-wide blackouts. If we query the oracle (ask the expert) regarding a set of features collected a specific point in time, the oracle may suggest that the moment in time is not interesting but that another moment in time is atypical or is behaving strangely. The collection of categories for time-dependent events may further help a user in understanding their system as we begin to determine automatically events that are unique or peculiar.

Additionally, it may not be advisable to query an instance in time to categorize it because if a user is presented with a set of events that occurred at 9:47:23 PM PST, the user may have no grasp of the exact event at the specific time. But if the user was presented instead with a range of times, say 9:30 PM to 10:00 PM and a summary of the events over that time period, then the user may be able to provide detailed information. This also may be useful if a couple of observations within the time window are atypical; the user may be able to label specific observations within the window, especially because they may differ greatly with the other observations. This approach may be feasible with [multiple instance active learning][3], which essentially approaches the active learning problem hierarchically so that a number of observations may be labeled (the labeling might be considered soft in that it is subject to change) if they are similar enough to the specific instance that was queried or a summary of a set of similar observations.

## Other ##

The **information density** approach to active learning has been proposed by one of the key players in the active learning literature. Rather than discussing it here, I have devoted a section below to discuss it.

It is important to note that a recent Journal of Machine Learning Research conference proceedings outlined a number of future directions in the active learning community, and several of them are similar to the ones that I discuss above. Then, it really is a matter of racing to be the first to propose a good solution. [The conference proceedings paper is found here][4].

# Information Density #

In this section, we discuss the **information density** approach to active learning. First, we introduce the **uncertainty sampling** framework, which can be viewed as a special case of **information density** and then mention a couple of its well-known models. Then, we define the **information density** method as a generalization of the **uncertain sampling** approach. We comment on the ad-hoc nature of **information density** and propose an alternative method.

## Uncertainty Sampling ##

The **information density** approach to active learning has been proposed by [Dr. Burr Settles][5] in [his Ph.D. thesis][6]. First, he discusses the **uncertainty sampling** method, where an oracle is queried with the unlabeled observation that is most *uncertain*. The uncertainty is typically defined by a loss or risk function, but there are a few ad hoc approaches to determining uncertainty. Dr. Settles notes that there have been several examples discussed in the literature where the most uncertain observations are the outlying observations. Hence, if we do not have much data from the individual groups, which is the nature of the active learning approach, then the outliers can significantly distort our knowledge of the groups. We define the uncertainty function about an observation [eqn=$\textbf{x}$] as [eqn=$\phi(\textbf{x})$], so that we will query the observation
[equation]
\textbf{x}^* = \text{argmax}_x \phi(\textbf{x})
[/equation]


[Culotta and McCallum (2005) have proposed the least confidence function][7] (TODO: Do I have the wrong reference for the LC approach?)
[equation]
\phi^{LC}(\textbf{x}) = 1 - P_{\theta}(\widehat{y} | \textbf{x}),
[/equation]

where [eqn=$\widehat{y} = \text{arg max}_y P_{\theta}(\widehat{y} | \textbf{x})$] is the observation that maximizes the *a posteriori* probability under model [eqn=$\theta$]. Hence, the least confidence observation that is queried is
[equation]
\textbf{x}^*_{LC} = \text{argmax}_x 1 - P_{\theta}(\widehat{y} | \textbf{x}) = \text{argmin}_x P_{\theta}(\widehat{y} | \textbf{x})
[/equation]
which is the observation that has minimizes the *a posteriori* probability under the model [eqn=$\theta$]. Although the least confidence function is simple to interpret, it often leads to the selection of outlying feature vectors, which can degrade future classification performance for classifiers that are not robust to outliers. Even if a classifier is robust outliers, if the classifier is trained on mostly outlying observations, then there will a significant training bias.

Consider, for example, the binary classification problem with two univariate normal distributions. Also, assume that each population has a population variance of 1 and that the means of classes 1 and 2 are 1 and 2 respectively. Now, suppose that we have a training data set with 10 observations, where the majority of the observations are between 0 and 3. For our thought experiment, consider that only two of the observations are labeled: the first observation is 1 and is labeled as class 1, and the second observation is labeled as class 2. Finally, we assume that we have the unlabeled, outlying observation 7. Clearly, this observation will be queried as it is the least likely of the observations to have occurred from either population or from the marginal distribution of both populations. If the oracle determines that the observation 7 is from the second class, then the sample mean of the second population then will shift from 2 to 4.5. If the oracle determines that the observation 7 is from the first class, the bias induced is even worse. Hence, our knowledge about the underlying distributions can be dramatically shifted with the least confidence function approach to active learning.

Other uncertainty sampling methods include **margin sampling**, which is a correction for the **least confidence** model in the multi-class setting. Perhaps, the most popular proposal for the **uncertainty sampling** function is the Shannon entropy function, so that we query the observation
[equation]
\textbf{x}^{*} = \text{arg max}_{\textbf{x}} -\sum_{i=1}^K P_{\theta}(y_i | \textbf{x}) \log P_{\theta}(y_i | \textbf{x}), 
[/equation]
where [eqn=$K$] is the number of populations from which the data was generated. Notice that for binary classification, the Shannon entropy function approach is equivalent to the **least confidence** approach and is also equivalent to **margin sampling**.

We note that the **uncertainty sampling** function can be considered as a loss function from a decision theoretic perspective. Hence, we query observations that maximize the specified loss function. This will be important from a Bayesian perspective as we will rely on this feature to extend the **information density** approach to active learning.

Finally, a more detailed discussion of **uncertainty sampling** is given in the [active learning overview from Settles][1].

## Information Density ##

As we noted above, the **uncertainty sampling** approach queries the feature vector that maximizes the specified uncertainty (or loss) function. As we considered above, the approach typically leads to outliers being queried, which is especially problematic if the number of labeled observations is small. Also, note that the loss function is specified in terms of what is known about the classes themselves and their corresponding labeled observations and that the unlabeled observations are ignored.

The **information density** approach seeks to overcome some of the disadvantages of **uncertainty sampling** by finding the unlabeled observation [eqn=$\textbf{x}$] that simultaneously maximizes the specified **uncertainty sampling** function and the similarity of [eqn=$\textbf{x}$] to the other unlabeled observations. This approach is advantageous if we consider the above example; with the **least confidence** approach, we queried the least likely observation that could have been generated from the underlying marginal distribution. Now, suppose that we wished to weight the least likely observation by its proximity to other observations. Then, because the outlying observation is not weighted as high as the observations near the means of the two observations, the outlying observation will not be queried, and an observation between 0 and 3 will be queried instead.

TODO: Define the information density function in more detail here.
TODO: Mention that the choice of similarity function may not be obvious, and furthermore, Euclidean distance may not be appropriate for defining similarity of continuous variables.

Now, consider the interpretation of a similarity function. For continuous random variables, maximizing the similarity function in some sense determines the location of the marginal probability distribution that is most dense. In other words, maximizing the similarity function is equivalent (???) to maximizing the marginal probability density function.

 


We contend that the similarity function is an ad-hoc replacement for the marginal posterior predictive, and that we can improve the active learning approach by viewing the entire problem from a Bayesian perspective.

## Information Density as an Approximation to the Posterior Predictive Distribution ##

**A Bayesian Alternative to Information Density in Active Learning**

# Active Learning in R #

There are very few implementations of active learning that I have found. Moreover, I have found no implementations of active learning in R. Because we are interested in investigating various models, it seems reasonable to try several of them out. Most of them are fairly simple to implement if a classifier is already specified. As an example, the **uncertainty sampling** approach only requires that a posterior probability be returned. The **query-by-committee** approach requires a committee of classifiers be used, and we average their *hard* class labels (or the uncertainty in them). Alternatively, we average their *soft* class labels, which is an alternative name for posterior probability.

For a set of classifiers in R, these approaches are easy to implement. It seems reasonable to implement all of the ones discussed in Sections 3.1 and 3.2 in the [Active Learning Literature Review][1]. As I develop these, we can apply the methods to some of the data sets used in the literature. If we write these methods up and then add their results to an R vignette, we can have an unfinished draft of a paper to submit to the Journal of Statistical Software or the Journal of Machine Learning Research. Of course, we cannot implement all methods in the literature, but rather a large proportion of the methods discussed in Section 3.

For implementation, the user should be able to specify classifier(s) whether in **uncertainty sampling** (uncertainty_sampling) or **query-by-committee** (qbc) and then specify the metric (loss/risk?) to be used.

For the vignette, follow the format of the lubridate paper in the Journal of Statistical Software.

Perhaps, optimizing the expected error rate may not be as appropriate as optimizing the conditional error rate?
[1]: http://www.cs.cmu.edu/~bsettles/pub/settles.activelearning.pdf
[2]: https://www.mturk.com/mturk/welcome
[3]: http://www.cs.cmu.edu/~bsettles/pub/settles.nips08.pdf 
[4]: http://www.cs.cmu.edu/~bsettles/pub/settles.jmlrwcp11.pdf
[5]: http://www.cs.cmu.edu/~bsettles/
[6]: http://www.cs.cmu.edu/~bsettles/pub/settles.thesis.pdf
[7]: http://www.cs.umass.edu/~culotta/pubs/culotta05reducing.pdf
