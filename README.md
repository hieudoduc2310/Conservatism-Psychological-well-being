# Conservatism-Psychological-well-being
1. Introduction:
This project discusses conceptualization, operationalization and inferential modelling for the main research question “Are conservative attitudes good or bad for your psychological well-being" using the World Survey Data Wave 6. To investigate the aforementioned research question, I wil go into details and step by step of the analysis, including data cleaning, inferential analysis and prediction efforts.
2. Analytic Tasks
2.1 Data Preparation
2.1.1. Exploring missing data
The initial check revealed that the dataset contains 13156 observations, 179 variables and has no
missing values. However, the codebook suggests that negative values indicate a form of non-response
(e.g. “No answer”). Therefore, we chose to treat non-response values as missing values by replacing
negative values with “NA”. This resulted in an overall proportion of missing data (PMI) that is not higher
than 0.17. Additionally, the covariance coverage for all variables is reasonably high (cc > .77).
2.1.2. Variable selection and reverse coding
For a more efficient data cleaning process, we subsetted the data with only relevant variables. For
inference, we chose to keep variables based on theory. For prediction, all variables that are at least
moderately correlated with the dependent variables were kept (r ≥ .20). A dataset of 30 variables
remained (appendix A). Additionally, the assumption of multicollinearity has not been violated since
none of the variables are too highly correlated with each other (r < .64). The correlations and its plot are
presented in appendix G and H.
To improve interpretation, some variables were reverse coded. The scale of happiness (V10) was
reverse coded to be in accordance with that of life satisfaction (V23). Additionally, to align all items of
the Schwartz scale to measure the same concept, the variables V72, V77 and V79 were reverse coded.
The reverse coding is based on the theory of the founder of the scale (Schwartz, 1999).
