# link_prediction in CQA networks
Predicting future collaborations in Community-based Question Answering networks

Uses curated stackoverflow dataset from UCI - Machine learning repository

Uses network analysis tool - 'igraph' 

link_pred.R ---> machine learning training and classification for link prediction. Uses models like Logistic Regression, Decision Trees, Gradient boosting, Random Forests, Multilayer Perceptron.

distribution_plots.R ---> plotting the distribution of features with respect to links that are formed and not formed in the future.

roc.R ---> plotting receiver operating characteristic curves for the models.

extractTags.py ---> to extract tags from the interactions in data set and associate it with individual users
