# Conservatism-Psychological-well-being
**1. Introduction**\
&emsp; This project discusses conceptualization, operationalization and inferential modelling for the main research question ***“Are conservative attitudes good or bad for your psychological well-being"*** using the World Survey Data Wave 6. To investigate the aforementioned research question, I wil go into details and step by step of the analysis, including data cleaning, EDA, and inferential analysis.\
**2. Analytic Tasks**
**2.1 Data Preparation**\
**2.1.1. Exploring missing data**\
&emsp; The initial check revealed that the dataset contains *13156* observations, *179* variables and has no
missing values. However, the codebook suggests that negative values indicate a form of non-response
(e.g. “No answer”). Therefore, we chose to treat non-response values as missing values by replacing
negative values with *“NA”*. This resulted in an overall proportion of missing data (*PMI*) that is not higher
than *0.17*. Additionally, the covariance coverage for all variables is reasonably high (*cc* > .77).\
**2.1.2. Variable selection and reverse coding**\
&emsp; For a more efficient data cleaning process, I subsetted the data with only relevant variables. For
inference, I chosed to keep variables based on theory. For prediction, all variables that are at least
moderately correlated with the dependent variables were kept (*r ≥ .20*). A dataset of 30 variables
remained (appendix *A*). Additionally, the assumption of multicollinearity has not been violated since
none of the variables are too highly correlated with each other (*r < .64*). The correlations and its plot are
presented in appendix *G* and *H*.\
&emsp; To improve the interpretation, some variables were reverse coded. The scale of happiness (V10) was
reverse coded to be in accordance with that of life satisfaction (V23). Additionally, to align all items of
the Schwartz scale to measure the same concept, the variables V72, V77 and V79 were reverse coded.
The reverse coding is based on the theory of the founder of the scale ([Schwartz, 1999](https://psycnet.apa.org/record/1999-00535-002)).\
**2.1.3. Outliers**\
&emsp; I have detected the univariate outliers using Tukey’s boxplot method and multivariate outliers using
Mahalanobis Distance. Nevertheless, kept univariate outliers because (1) there were no probable
outliers (i.e. points that fall outside the outer fence) and (2) the floor and ceiling in Likert Scale still
represent a valid response of the respondent’s opinion.\
&emsp; Contrariwise, removed the multivariate outliers as they represent inconsistency in given
answers over multiple questions in the survey. Since the Mahalanobis function cannot handle infinite or
NA values, I created a subset in which NA’s were temporarily replaced by the median in order to
involve all rows of data in the outlier analysis. I removed 975 multivariate outliers, leaving 12181
observations. Therefore, I also provide analyses including outliers for inference (appendix *M*) and
prediction (appendix *N*).\
**2.1.4. Missing data treatment**\
&emsp; I treated missing data with Multiple Imputation using “mice”, as the procedure gives unbiased
standard errors for estimates. Additionally, the covariance coverage allows for multiple imputation (cc >
*0.77*). The hyperparameters were set as follows: m = *20*, maxit = *10* and method = pmm / logreg.
Predictive mean matching was used as the method stays close to the values of the completely observed
dataset, while the categorical gender variable was imputed using logistic regression. The multiply
imputed dataset shows convergence (appendix *E*) and sensible density (appendix *F*). These multiply
imputed datasets were used for both inferential and prediction tasks.\
**2.2 Inferential Modeling**\
&emsp; This chapter discusses conceptualization, operationalization and inferential modelling for the main
research question “Are conservative attitudes good or bad for your psychological well-being”.\
**2.2.1. Conceptualization**\
&emsp; Psychological well-being (DV). A conceptualization, which gained prominence among sociologists, places
life satisfaction and happiness at the core of psychological well-being ([Diener, Lucas & Oishi, 2002](https://www.oxfordhandbooks.com/view/10.1093/oxfordhb/9780195187243.001.0001/oxfordhb-9780195187243-e-017)). This
conception emphasizes life satisfaction as the cognitive dimension, and happiness as the affective
dimension of psychological well-being. Therefore, I conceptualized psychological well-being as a
multidimensional construct, containing life satisfaction (V23) and happiness (V10) as its underlying
dimensions.\
&emsp; Conservatism (IV). Conservative attitudes encompass the degree to which individuals maintain
and conform to the status quo and traditional order ([Schwartz, 1999](https://psycnet.apa.org/record/1999-00535-002)). Our dataset includes items of
Schwartz’s Cultural Dimension scale, which have been demonstrated to capture the concept of
conservative attitudes well in previous research ([Schwartz, 1999](https://psycnet.apa.org/record/1999-00535-002)). Therefore, I computed
“Mean_Scaled_Score” from these items, which demonstrated good reliability (Cronbach’s α = .73).
There was no need to delete items.\
**2.2.2. Operationalization of the RQ**\
&emsp; Based on the conceptualization of the DV and IV, I operationalize the RQ as described in table 1. The
control variables that were included in the models and its reason for inclusion are shown in appendix I.\
![213123123](https://user-images.githubusercontent.com/57223360/124988748-abcf0f00-e03e-11eb-9b8b-1217d2ed1c7b.png)
**2.2.3. Results from the inferential modelling**\
&emsp; The specifications of all models can be found in appendix K, and its output in appendix J. To address the
first sub-question, we first regressed life satisfaction (V23) on all items (R2 = *.45*, F[*15,12165*] = *608.93*, p
< *.001*, RIV = *.094*). As shown in appendix *J*, the regression coefficients of V70, V72, V78, V79 were nonsignificant in this model. After controlling for the control variables, V71 appears to have the largest partial effect on life satisfaction, aside from the control variables (β = *.042*, SE = *.012*, t[*12165*] = *3.522*, p < *.001*, FMI = *.142*), while V73 appears to have the smallest partial effect on the DV (β = *-.028*, SE = *.012*, t[*12165*] = *-2.374*, p < *.05*, FMI = *.142*). Looking at the direction of the partial effects, V71 and V76 have a positive effect, while V73, V75 and V77 have a negative effect on the DV, after controlling for the
control variables. Furthermore, we regressed life satisfaction on Mean_Scaled_Score and this model
significantly explains 43% of the variability in life satisfaction (R2 = *.43*, F[*7,12173*] = *1363.587*, p < *.001*,
RIV = *.040*). However, the effect of Mean_Scaled_Score is statistically non-significant (β = .019, SE = .026,
t[12173] = *.73*, p > *.05*, FMI = *.11*).\
&emsp; To address the second sub-question, we first regressed happiness (V10) on all items (R
2 = *.34*, F[*15,12165*] = *375.18*, p < *.001*, RIV = *.11*). As presented in appendix *J*, the regression coefficients V72
and V78 were not significant in this model. After controlling for the control variables, V79 appears to
have the largest partial effect on happiness, aside from the control variables (β = *-.024*, SE = *.004*,
t[*12165*] = *5.859*, p < *.001*, FMI = *.12*), while similarly to model 1, V73 appears to have the smallest
partial effect on happiness (β = *-.01*, SE = *.004*, t[*12165*] = *-2.353*, p < *.01*, FMI = *.19*). Looking at the
direction of the partial effects, V71, V75, V77 and V79 have a positive effect, while V70, V73 and V76
have a negative effect on the DV, after controlling for the control variables. Furthermore, we regressed
happiness on Mean_Scaled_Score and this model significantly explains 33% of the variability in
happiness (R2 = *.33*, F[*7,12165*] = *812.274*, p < *.001*, RIV = *.05*). However, the effect of
Mean_Scaled_Score is statistically non-significant (β = *.016*, SE = *.009*, t[*12173*] = *1.87*, p > *.05*, FMI
= *.16*).\
**3. Conclusion**\
&emsp; In this report, I investigated the RQ ***“Are conservative attitudes good or bad for your psychological
well-being?”***.\
&emsp; Even though the results suggest a small and positive overall effect of conservative attitudes on
life satisfaction and happiness, no statistical support for this finding at a p <.05 level. However,
on an item-by-item basis I did find significant, but multidirectional effects. This is surprising, given that
the items are reverse-coded and measure the same underlying construct unidirectionally. Therefore,
future research must subject this scale to validity and reliability checks beyond Cronbach’s alpha. As a
consequence, to prevent a Type II error, this report lacks evidence to provide a conclusive answer to the
research questions.\
**3. Lisence**\
&emsp; This project was originially conducted by a group of four: Hieu D. DO, Hoang Phuc Pham, Larissa Chua, Ryan van Velzen in the course Stats & Methodology. However, there are significant modifications after the course is completed. All the credits of new insights and modification belongs to the repository's author.

