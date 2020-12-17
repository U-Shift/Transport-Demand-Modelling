# 8. Discrete Choice Models

This chapter explores how to estimate and analyze Discrete Choice Models, their uses and applications.

#### Goals:
* Understand Biogeme concepts 
* Understand possible applications for Biogeme
* Be able to create discrete choice models
* Learn some tools that can be used to create more complicated models in the future
* Learn some handy tricks/tools/packages

#### Biogeme Overview
> Biogeme is a open source Python package designed for the maximum likelihood estimation of parametric models in general, with a special emphasis on discrete choice models.
>
> --https://biogeme.epfl.ch/index.html


## Summary

#### [6.1. Multinomial Logit and Probit Models](6.1-MultinomialLogitAndProbitModels)

|File   | Topic  | Description  |
|---|---|---|
|[01-biogeme-basics.ipynb](6.1-MultinomialLogitAndProbitModels/01-biogeme-basics.ipynb)   |Logit Basics   | How to estimate a simple multinomial logit model.  |
|[02-logit-estimators-results.ipynb](6.1-MultinomialLogitAndProbitModels/02-logit-estimators-results.ipynb)   |Goodness of fit   |How to analyze the models goodness of fit indicators.   |
|[03-logit-validation.ipynb](6.1-MultinomialLogitAndProbitModels/03-logit-validation.ipynb)   |Cross-validation   | How to perform cross-validation on a dataset using a logit model. |
|[04-logit-train-test.ipynb](6.1-MultinomialLogitAndProbitModels/04-logit-train-test.ipynb)   |Prediction   |How to split a dataset into train and test dataset to perform prediction. Evaluate results on a confusion matrix. |
|[05-probit-basics.ipynb](6.1-MultinomialLogitAndProbitModels/05-probit-basics.ipynb)| Probit  | How to train a binomial probit model.  |



#### [6.2. Nested Logit Models](6.2-NestedLogitModels)

|File   | Topic  | Description  |
|---|---|---|
|[01-logit-nested.ipynb](6.2-NestedLogitModels/01-logit-nested.ipynb)   |Nested Logit Basics   | How to estimate a simple nested logit model.  |
|[02-logit-nested-market.ipynb](6.2-NestedLogitModels/02-logit-nested-market.ipynb)   |Market Share   |How to compute market shares of alternatives.   |
|[03-logit-nested-revenue-scenarios.ipynb](6.2-NestedLogitModels/03-logit-nested-revenue-scenarios.ipynb)   |Market Share Scenarios   |How to create scenarios and compute their market share outcomes. |
|[04-logit-nested-elasticities.ipynb](6.2-NestedLogitModels/04-logit-nested-elasticities.ipynb)   |Elasticities   |How to compute elasticities. |
|[05-logit-nested-WTP.ipynb](6.2-NestedLogitModels/05-logit-nested-WTP.ipynb)| Willingness To Pay (WTP)  | How to compute willingness to pay indicators.  |


#### [6.3. Ordered Logit Models](6.3-OrderedLogitModels)

|File   | Topic  | Description  |
|---|---|---|
|[01-biogeme-ordered.ipynb](6.3-6.3-OrderedLogitModels/01-biogeme-ordered.ipynb)   |Ordered Logit Basics   | How to estimate a simple ordered logit model.  |





#### Other Useful Links

* Installation: https://biogeme.epfl.ch/install.html
* Datasets: https://biogeme.epfl.ch/data.html
* Documentation: https://biogeme.epfl.ch/documents.html
* Code Documentation: https://biogeme.epfl.ch/sphinx/index.html
* Examples: https://biogeme.epfl.ch/examples.html
* Google Groups Forum: https://groups.google.com/forum/#!forum/biogeme
