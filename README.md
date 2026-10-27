2026-Oct-27
* First commit with crude DSM
* Scoring options and weights for five criteria
* Score multiplier for metals and pathogens (pre-weight) based on two behavioral assays. Each behavior assay can have a different multiplier, but it is applied to metals and pathogens the same
* Returns site scores for comparison, could be used to classify a single site for action, but we need to determine score cutoffs for particular actions or other binary classifiers
* Smaller score is "better" or more optimal
* Scores are weighted and summed

I'm looking to start seeing whether this interface is desirable, whether the scoring should be more like a "radio button" input, and to develop some idea of what score leads to what kind of management action.

At this point, it is designed to input three different sites to see how their scores differ in the end.
