##-----------------------------------------------------------------------------##
##--------------------Conservatism-Psychological-well-being--------------------##
##-----------------------------------------------------------------------------##
### Author:   #Hieu.D.DO


##-----------------------------------------------------------------------------##
##------------------------------Preliminaries----------------------------------##
##-----------------------------------------------------------------------------##
rm(list = ls(all = TRUE))

##Install required library that not included in the course | practical sections
install.packages("imputeR")
install.packages("visdat")
install.packages("VIM")
install.packages("Hmisc")
install.packages("miceadds")
install.packages("corrplot")

## Load packages:
library(mice)
library(imputeR)
library(visdat)
library(VIM)
library(MASS)
library(Hmisc)
library(MLmetrics)
library(mitools)
library(miceadds)
library(corrplot)
library(here)
library(psych)

## 3) Check and load data set
#SET WORKING DIRECTORY TO THE "CODE" DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dataDir <- "../data/"
data <- readRDS(paste0(dataDir, "wvs_data.rds"))

## Write plots to external PDF file:
plotDir  <- "../figs/"

## Source the "studentFunctions.R" script to get the cv.lm function:
source("studentFunctions.R")

## Source the "miPredictionRoutines.R" script to get MI-based prediction stuff:
source("miPredictionRoutines.R")

##-----------------------------------------------------------------------------##
##------------------------2.1 DATA Prepareration-------------------------------##
##-----------------------Missing Data Descriptives-----------------------------##
##-----------------------------------------------------------------------------##
##Since every datapoint lower than zero can be considered as NA
data[data<0] <- NA
#Check variables that highly correlated with life_satisfaction V23
cor_test_V23 <- cor(data, data$V23, method = "pearson", use = "na.or.complete")
cor_test_V23 <- data.frame(colnames = row.names(cor_test_V23),correlation= round(cor_test_V23,3))
high_cor_V23 <- (subset(cor_test_V23,abs(correlation) >= 0.20 )[,"colnames"])
high_cor_V23 <- vapply(high_cor_V23, paste, collapse = ", ", character(1L))

#Check variables that highly correlated with Level of Happiness V10
cor_test_V10 <- cor(data, data$V10, method = "pearson", use = "na.or.complete")
cor_test_V10 <- data.frame(colnames = row.names(cor_test_V10),correlation= round(cor_test_V10,3))
high_cor_V10 <- (subset(cor_test_V10,abs(correlation) >= 0.20 )[,"colnames"])
high_cor_V10 <- vapply(high_cor_V10, paste, collapse = ", ", character(1L))

#The set of variables that highly correlate with V10 is a subset of V23's
#Besides those variables contain high correlation coefficient with
#outcome variables, also include variables (non high corr coefficient)
#retrieved from theoretical approach
targets <- c(#Control variables
             "V240", "V242",
             #Invidual conservatism variables
             "V70", "V71", "V72", "V73", "V75", "V76", "V77", "V78", "V79")
variables<-append(high_cor_V23,targets)
data <- data[,variables]
#Reverse code for certain variables
data[,"V10"]<-5-data[,"V10"]
#data[,"V71"]<-7-data[,"V71"]
data[,"V72"]<-7-data[,"V72"]
#data[,"V73"]<-7-data[,"V73"]
#data[,"V75"]<-7-data[,"V75"]
data[,"V77"]<-7-data[,"V77"]
data[,"V78"]<-7-data[,"V78"]
data[,"V79"]<-7-data[,"V79"]
data_withoutNA <- data[complete.cases(data),]

## Checking the proportion of missing values for each variable planned to analyze.
proportion_missingp <- colMeans(is.na(data))*100
proportion_missingp

##Visuallzing the proportion of missing values for each variable (from highest to lowest)
pdf(paste0(plotDir, "data_missingness.pdf"),onefile = TRUE)
visdat::vis_miss(data, sort_miss = T)
dev.off()

## Calculate the covariance coverage matrix.
covariance_coveragep <- md.pairs(data)$rr / nrow(data)
covariance_coveragep ##High=> pariwise combination is indeed high => interpretable

## Detecting the range of covariance coverage values
range(covariance_coveragep)

##Checking for Multicolinearity in the data:
m_col <- cor(data_withoutNA)

pdf(paste0(plotDir, "corrplot.pdf"),onefile = TRUE)
corrplot(m_col)
dev.off()

##Find highest multicollinear variables
max(m_col[m_col != 1.000])
##Test its significance
cor.test(data_withoutNA$V190, data_withoutNA$V191, method  = "pearson")

##Some other correlation tests for EDA
cor.test(data_withoutNA$V55, data_withoutNA$V23, method  = "pearson")
cor.test(data_withoutNA$V59, data_withoutNA$V23, method  = "pearson")
cor.test(data_withoutNA$V23, data_withoutNA$V10, method  = "pearson")

##Check for Cronbach's Alpha
subset_scale <- data[,c("V70", "V71", "V72", "V73", "V75" , "V76", "V77", "V78", "V79")]
alpha(subset_scale, check.keys = TRUE)

##-----------------------------------------------------------------------------##
##--------------------Detect and treat univariate outliers---------------------##
##-----------------------------------------------------------------------------##

## Detecting possible and proable outliers using Turkey's Boxplot:
bpOutliers <- function(x) {
  ## Compute inner and outer fences:
  iFen <- boxplot.stats(x, coef = 1.5)$stats[c(1, 5)]
  oFen <- boxplot.stats(x, coef = 3.0)$stats[c(1, 5)]

  ## Return the row indices of flagged 'possible' and 'probable' outliers:
  list(possible = which(x < iFen[1] | x > iFen[2]),
       probable = which(x < oFen[1] | x > oFen[2])
  )
}
bpOut <- lapply(data, bpOutliers)
bpOut # 140 170 175 188 190 192 237 238 130 139 78 72

## using Interquartile to  potential outliers:(1.5 Interpretation purposes)
# #extract_outlier <-function(x, na.rm=T,...){
#   #Get an interval of 25th and 75th
#   quant<-quantile(x, prob=c(0.25,0.75), na.rm = T)
#   #Get the boundaries distinguishing outlers
#   boundary<-1.5*IQR(x, na.rm=T)
#   dat0<-x
#   #if data falls out of range of 1.5*quantile -> remove
#   dat0[x<quant[1] - boundary]<-NA
#   dat0[x<quant[2] + boundary]<-NA
#   x<-dat0
#   x[!is.na(x)]
# }
#table(extract_outlier(data$V78))



##-----------------------------------------------------------------------------##
##--------------------Detect and treat Multivariate outliers-------------------##
##-----------------------------------------------------------------------------##
#temporarily replace missing value with median of each variable
temp<-data
for(i in 1:ncol(temp)){
  temp[[i]][is.na(temp[[i]])] <- median(temp[[i]], na.rm=TRUE)

}
## Recheck the proportion of missing values.
proportion_missing <- colMeans(is.na(temp))*100
proportion_missing

#Compile the mcdMahalanobis Function
mcdMahalanobis <- function(data, prob, ratio = 0.75, seed = NULL) {
  ## Set a seed, if one is provided:
  if(!is.null(seed)) set.seed(seed)

  ## Compute the MCD estimates of the mean and covariance matrix:
  stats <- cov.mcd(data, quantile.used = floor(ratio * nrow(data)))

  ## Compute robust squared Mahalanobis distances
  md <- mahalanobis(x = data, center = stats$center, cov = stats$cov)

  ## Find the cutoff value:
  crit <- qchisq(prob, df = ncol(data))

  ## Return row indices of flagged observations:
  which(md > crit)
}

#Getting indices of multivariate Outliers
mcdMahalanobis(data = temp, prob = 0.999, seed = 32)->vector

#Number of row removed
length(vector) #975
#Subset a newdata that is free from Multivariate Outliers (On data contain NA)
data_withoutOutliers<- data[-vector,]


##-----------------Treating missing data - Multiple Imputation----------------##
### Imputation Phase
## Conduct MI using all of the defaults:
# Reconfig columns name for V240
data_withoutOutliers$V240<-as.factor(data_withoutOutliers$V240)
data_withoutOutliers$V240 <- factor(data_withoutOutliers$V240, labels = c("Males", "Females"))
# Set method for MI
meth        <- rep("pmm", ncol(data_withoutOutliers))
names(meth) <- colnames(data_withoutOutliers)

#Since V240-Gender is categorical data -> change method to logistic regression
meth["V240"] <-"logreg"
MI <- mice(data_withoutOutliers, m = 20, maxit = 10, method = meth, seed = 32)
miceOut<-MI

##------------------------------Convergence Checks-----------------------------##
## Create traceplots of imputed variables' means and SDs:
pdf(paste0(plotDir, "convegence_check.pdf"),onefile = TRUE)
plot(miceOut, layout = c(4, 8))
dev.off()

## Sanity check the imputations by plotting observed vs. imputed densities:
pdf(paste0(plotDir, "density_check.pdf"),onefile = TRUE)
densityplot(miceOut)
memory.limit(size = 1000)
dev.off()

##------------------------------Create Mean Scale Score-----------------------------##

# Add Mean_scale_score feature to our data set
# Convert to Long
long <- complete(miceOut, action='long', include=TRUE)
# Generate new variable
long$Mean_scale_score <- (long$V70 + long$V71 + long$V72 + long$V73 + long$V75 + long$V76 + long$V77 + long$V78 + long$V79)/9
# Convert back to Mids
miceOut <- as.mids(long)
## Create list of multiply imputed datasets:
impList <- complete(miceOut, "all") #get total of complete filed data set (20)

##-----------------------------------------------------------------------------##
##---2.2 Inference task: Conservative attitudes on psychological well-beinng---##
##-----------------------------------------------------------------------------##

## Fit some regression models to the MI data:

##Level of Satisfaction regressed on Complete model:
Complete_SF<- lm.mids(V23 ~ V10 + V11 + V55+ V59 + V70 + V71  +
                      V72 + V73 + V75 + V76 + V77 + V78 + V79 + 
                      V240 + V242, data = miceOut)
#Level of Satisfaction regressed on Mean_Scale_score:
Complete_SF_SC<- lm.mids(V23 ~Mean_scale_score + V10 + V11 + 
                         V55+ V59 + V240 + V242, data = miceOut)

##Level of Happiness regressed on Complete model:
Complete_HP<- lm.mids(V10 ~ V23 + V11 + V55+ V59 + V70 + V71  +
                      V72 + V73 + V75 + V76 + V77 + V78 + V79 + 
                      V240 + V242, data = miceOut)
#Level of Happiness regressed on Mean_Scale_score:
Complete_HP_SC<- lm.mids(V10 ~ Mean_scale_score+ V23 + V11 + V55 + 
                         V59 + V240 + V242, data = miceOut)

#Level of Satisfaction intercept model only
Intercept_SF<- lm.mids(V23 ~ 1, data = miceOut)
#Level of Satisfaction intercept model only
Intercept_HP<- lm.mids(V10 ~ 1, data = miceOut)

###-------------------------------Pool results------------------------------------------###

## Pool the fitted models:
poolComplete_SF <- pool(Complete_SF)
poolComplete_SF_SC <- pool(Complete_SF_SC)
poolComplete_HP <- pool(Complete_HP)
poolComplete_HP_SC <- pool(Complete_HP_SC)

## Summarize the pooled estimates:
summary(poolComplete_SF)
summary(poolComplete_SF_SC)
summary(poolComplete_HP)
summary(poolComplete_HP_SC)

## Compute the pooled R^2:
pool.r.squared(Complete_SF)
pool.r.squared(Complete_SF_SC)
pool.r.squared(Complete_HP)
pool.r.squared(Complete_HP_SC)

##Subset for clearer interpretation
#Outcome as Level of Satisfaction
subset(summary(poolComplete_SF),term %in% c('V70','V71','V72','V73','V75','V76','V77','V78','V79','V240') & p.value >0.05  )

#See the variables that is not statistically significant
subset(summary(poolComplete_SF),p.value >0.05 )

#See the variables that is statistically significant
subset(summary(poolComplete_SF),p.value <0.05 )->subset1
subset1[order(-abs(subset1$estimate)),]

#Outcome as Level of Happiness
subset(summary(poolComplete_HP),term %in% c('V70','V71','V72','V73','V75','V76','V77','V78','V79','V240') )
subset(summary(poolComplete_HP),p.value >0.05 )

#See the variables that is statistically significant
subset(summary(poolComplete_HP),p.value <0.05 )->subset2
subset2[order(-abs(subset2$estimate)),]

##Show the Ftest of the model
##For life satisfaction
anova(Complete_SF, Intercept_SF)
##For Mean Scale Score on Life Satisfaction
anova(Complete_SF_SC, Intercept_SF )
##For level of happiness
anova(Complete_HP, Intercept_HP)
##For Mean Scale Score on level of happiness
anova(Complete_HP_SC, Intercept_HP )

##---------------------------------------------------------------------------------------##
##----------------2.3 Prediction task: Predicting Life satisfaction----------------------##
##---------------------------------------------------------------------------------------##

##----------------MI-Based Prediction K-Fold Cross-Validation----------------------------##

## Split the multiply imputed data into training, validation, and testing sets:
n <- nrow(impList[[1]]) #getting total observation
set.seed(32)
index <- sample(
  c(rep("train", round(n*0.8)), rep("test", n-round(n*0.8))))

impListNew <- splitImps(imps = impList, index = index)


##Finding features groups for model construction
# Based on correlation coefficient with the target (>= 0.2, 0.25 and 0.3)
higher_cor_V23 <- (subset(cor_test_V23,abs(correlation) >= 0.3 )[,"colnames"])
higher_cor_V23 <- vapply(higher_cor_V23, paste, collapse = ", ", character(1L))
higher_cor_V23
# Based on statistical significant level of regression slopes (at 0.05, 0.01 and 0.001)
signi_feature<-subset(summary(poolComplete_SF),p.value <0.05 )
signi_feature$term

# Constructing models
mods <- rev(c(#Complete model (with everything) [7]
              "V23 ~ . - V23 - Mean_scale_score",
              #Model with highly correlated variables with outcome >= 0.2 [6]
              "V23 ~ V10 + V11 + V24 + V55 +V56 + V59 + V140 + V170 + V172 + V175 + V181 + V188 + V190 + V191 + V192 +V237 +V238 +V239",
              #Model with highly correlated variables with outcome >= 0.25 [5]
              "V23 ~ V10 + V11 + V55 + V56 + V59 + V191 + V239",
              #Model with highly correlated variables with outcome >= 0.3 [4]
              "V23 ~ V10 + V11 + V55 + V59 + V239",
              #Model with only statistical significant regression coefficient at 0.05 [3]
              "V23 ~ V10 + V11 + V55 + V59 + V71 + V72 + V73 + V75 + V76 + V77 + V240",
              #Model with only statistical significant regression coefficient at 0.01 [2]
              "V23 ~ V10 + V11 + V55 + V59 + V71 + V73 + V75 + V76 + V77 + V240",
              #Model with only statistical significant regression coefficient at 0.001 [1]
              "V23 ~ V10 + V11 + V55 + V59 + V71 + V73"
))


### K-Fold Cross-Validation:

## Conduct 10-fold cross-validation in each multiply imputed dataset:
tmp <- sapply(impListNew$train, cv.lm, K = 10, models = mods, seed = 32)

## Aggregate the MI-based CVEs:
cve <- rowMeans(tmp)
cve

# Calculate Test mse for all models
mse<-c()
i=1
for (model in mods) {
  fits2<-lapply(X   = impListNew$train,
                FUN = function(x, mod) lm(mod, data = x),
                mod = model
  )
  mse[i] <- mseMi(fits = fits2, newData = impListNew$test)
  i=i+1
}
mse

# Visualization Training vs Test mse
library(ggplot2)

mse_graph <- as.data.frame(cve)
name_graph <- c(rep(1:length(cve)))
mse_graph<-cbind(mse_graph,mse)
mse_graph<-cbind(mse_graph,name_graph)

library(lsr)

colnames(mse_graph)<- c('error_train','error_test','model')
graphdata<-wideToLong(mse_graph,within = "Set")
graphdata$model<-as.factor(graphdata$model)
graphdata$Set<-as.factor(graphdata$Set)

## Write plots to external PDF file:
pdf(paste0(plotDir, "predictionErrors.pdf"), onefile = TRUE)
ggplot(graphdata, aes(x=model, fill=Set, y=error)) +
  geom_bar(position=position_dodge(width = 0.7), stat="identity", width = 0.6) +
  coord_cartesian(ylim = c(2.2,2.8)) +
  scale_y_continuous( breaks = c(seq(2.2,2.8,0.05))) +
  ylab("Test MSE") +
  xlab("Model") +
  ggtitle("Test-Prediction Error (MSE) per Model", subtitle = "Train vs. Test Set") +
  theme_classic()
dev.off()

##-----------------------------------------FIN---------------------------------------------##

# ##---------------------------------------------------------------------------------------##
# ##-------------------------Extra Analysis: MI with outliers------------------------------##
# ##---------------------------------------------------------------------------------------##
# #(For the bes performance of implementation on Professors'computer, we commented this whole block)
# 
# #MI is performed with outliers for reporting prediction and inference WITH outliers as supplementary material.
# 
# ### Imputation Phase
# ## Conduct MI using all of the defaults:
# # Reconfig columns name for V240
# data$V240<-as.factor(data$V240)
# data$V240 <- factor(data$V240, labels = c("Males", "Females"))
# 
# # Set method for MI
# meth        <- rep("pmm", ncol(data))
# names(meth) <- colnames(data)
# 
# #Since V240-Gender is categorical data -> change method to logistic regression
# meth["V240"] <-"logreg"
# MI2 <- mice(data, m = 20, maxit = 10, method = meth, seed = 32)
# miceOut_WO<-MI2
# 
# ##------------------------------Convergence Checks-----------------------------##
# ## Create traceplots of imputed variables' means and SDs:
# pdf(paste0(plotDir, "convegence_check_WO.pdf"),onefile = TRUE)
# plot(miceOut_WO, layout = c(4, 8))
# dev.off()
# 
# ## Sanity check the imputations by plotting observed vs. imputed densities:
# pdf(paste0(plotDir, "density_check_WO.pdf"),onefile = TRUE)
# densityplot(miceOut_WO)
# memory.limit(size = 1000)
# dev.off()
# 
# ##------------------------------Create Mean Scale Score-----------------------------##
# 
# # Add Mean_scale_score feature to our data set
# # Convert to Long
# long <- complete(miceOut_WO, action='long', include=TRUE)
# # Generate new variable
# long$Mean_scale_score <- (long$V70 + long$V71 + long$V72 + long$V73 + long$V75 + long$V76 + long$V77 + long$V78 + long$V79)/9
# # Convert back to Mids
# miceOut_WO <- as.mids(long)
# ## Create list of multiply imputed datasets:
# impList_WO <- complete(miceOut_WO, "all") #get total of complete filed data set (20)
# 
# ##---------------------------------------------------------------------------------------##
# ##----------------Extra Analysis: Inference including outliers---------------------------##
# ##---------------------------------------------------------------------------------------##
# 
# ## Fit some regression models to the MI data:
# 
# ##Level of Satisfaction regressed on Complete model:
# Complete_SF_WO<- lm.mids(V23 ~ V10 + V11 + V55+ V59 + V70 + V71 +
#                         V72 + V73 + V75 + V76 + V77 + V78 + V79 + V240 + V242, data = miceOut_WO)
# #Level of Satisfaction regressed on Mean_Scale_score:
# Complete_SF_SC_WO<- lm.mids(V23 ~Mean_scale_score + V10 + V11 + V55+ V59 + V240 + V242, data = miceOut_WO)
# 
# ##Level of Happiness regressed on Complete model:
# Complete_HP_WO<- lm.mids(V10 ~ V23 + V11 + V55+ V59 + V70 + V71 +
#                         V72 + V73 + V75 + V76 + V77 + V78 + V79 + V240 + V242, data = miceOut_WO)
# #Level of Happiness regressed on Mean_Scale_score:
# Complete_HP_SC_WO<- lm.mids(V10 ~Mean_scale_score+ V10 + V11 + V55+ V59 + V240 + V242, data = miceOut_WO)
# 
# #Level of Satisfaction intercept model only
# Intercept_SF_WO<- lm.mids(V23 ~ 1, data = miceOut_WO)
# #Level of Satisfaction intercept model only
# Intercept_HP_WO<- lm.mids(V10 ~ 1, data = miceOut_WO)
# 
# ###-------------------------------Pool results------------------------------------------###
# 
# ## Pool the fitted models:
# poolComplete_SF_WO <- pool(Complete_SF_WO)
# poolComplete_SF_SC_WO <- pool(Complete_SF_SC_WO)
# poolComplete_HP_WO <- pool(Complete_HP_WO)
# poolComplete_HP_SC_WO <- pool(Complete_HP_SC_WO)
# 
# ## Summarize the pooled estimates:
# summary(poolComplete_SF_WO)
# summary(poolComplete_SF_SC_WO)
# summary(poolComplete_HP_WO)
# summary(poolComplete_HP_SC_WO)
# 
# ## Compute the pooled R^2:
# pool.r.squared(Complete_SF_WO)
# pool.r.squared(Complete_HP_WO)
# 
# ##Subset for clearer interpretation
# #Outcome as Level of Satisfaction
# subset(summary(poolComplete_SF_WO),term %in% c('V70','V71','V72','V73','V75','V76','V77','V78','V79','V240') & p.value >0.05  )
# 
# #See the variables that is not statistically significant
# subset(summary(poolComplete_SF_WO),p.value >0.05 )
# 
# #See the variables that is statistically significant
# subset(summary(poolComplete_SF_WO),p.value <0.05 )->subset1
# subset1[order(-abs(subset1$estimate)),]
# 
# #Outcome as Level of Happiness
# subset(summary(poolComplete_HP_WO),term %in% c('V70','V71','V72','V73','V75','V76','V77','V78','V79','V240') )
# subset(summary(poolComplete_HP_WO),p.value >0.05 )
# 
# #See the variables that is statistically significant
# subset(summary(poolComplete_HP_WO),p.value <0.05 )->subset2
# subset2[order(-abs(subset2$estimate)),]
# 
# ##Show the Ftest of the model
# ##For life satisfaction
# anova(Complete_SF_WO, Intercept_SF_WO)
# ##For Mean Scale Score on Life Satisfaction
# anova(Complete_SF_SC_WO, Intercept_SF_WO )
# ##For level of happiness
# anova(Complete_HP_WO, Intercept_HP_WO)
# ##For Mean Scale Score on level of happiness
# anova(Complete_HP_SC_WO, Intercept_HP_WO )
# 
# ##---------------------------------------------------------------------------------------##
# ##----------------Extra Analysis: Prediction including outliers--------------------------##
# ##---------------------------------------------------------------------------------------##
# 
# #Don't forget to rename the prediction stuff slightly differently, so there is no overlap
# #E.g. I typed an extra "_WO" to name the objects, like Complete_SF_WO
# 
# #You can use the dataset miceOut_WO
# 
# ## Create list of multiply imputed datasets:
# impList_WO <- complete(miceOut_WO, "all") #get total of complete filed data set (20)
# 
# ## Split the multiply imputed data into training, validation, and testing sets:
# n <- nrow(impList_WO[[1]]) #getting total observation
# index_WO <- sample(
#   c(rep("train", round(n*0.8)), rep("test", n-round(n*0.8))))
# 
# impListNew_WO <- splitImps(imps = impList_WO, index = index_WO)
# 
# 
# ##Finding features groups for model construction
# 
# # Based on statistical significant level of regression slopes in the inferential model(at 0.05, 0.01 and 0.001)
# signi_feature_WO<-subset(summary(poolComplete_SF_WO),p.value <0.05 )
# signi_feature_WO$term
# 
# # Constructing models
# mods_WO <- rev(c(#Complete model (with everything) [7]
#                 "V23 ~ . - V23 ",
#                 #Model with highly correlated variables with outcome >= 0.2 [6]
#                 "V23 ~ V10 + V11 + V24 + V55 +V56 + V59 + V140 + V170 + V172 + V175 + V181 + V188 + V190 + V191 + V192 +V237 +V238 +V239",
#                 #Model with highly correlated variables with outcome >= 0.25 [5]
#                 "V23 ~ V10 + V11 + V55 + V56 + V59 + V191 + V239",
#                 #Model with highly correlated variables with outcome >= 0.3 [4]
#                 "V23 ~ V10 + V11 + V55 + V59 + V239",
#                 #Model with only statistical significant regression coefficient at 0.05 [3]
#                 "V23 ~ V10 + V11 + V55 + V59 + V71 + V73 + V75 + V76 + V77 + V240",
#                 #Model with only statistical significant regression coefficient at 0.01 [2]
#                 "V23 ~ V10 + V11 + V55 + V59 + V71 + V73 + V76 + V77 + V240",
#                 #Model with only statistical significant regression coefficient at 0.001 [1]
#                 "V23 ~ V10 + V11 + V55 + V59 + V71 + V73 + V77"
# ))
# 
# 
# ### K-Fold Cross-Validation:
# 
# ## Conduct 10-fold cross-validation in each multiply imputed dataset:
# tmp_WO <- sapply(impListNew_WO$train, cv.lm, K = 10, models = mods_WO, seed = 32)
# 
# ## Aggregate the MI-based CVEs:
# cve_WO <- rowMeans(tmp_WO)
# cve_WO
# 
# # Calculate Test mse for all models
# mse_WO<-c()
# i=1
# for (model in mods_WO) {
#   fits2<-lapply(X   = impListNew_WO$train,
#                 FUN = function(x, mod) lm(mod, data = x),
#                 mod = model
#   )
#   mse_WO[i] <- mseMi(fits = fits2, newData = impListNew_WO$test)
#   i=i+1
# }
# mse_WO
# 
# # Visualization Training vs Test mse
# 
# mse_graph_WO <- as.data.frame(cve_WO)
# name_graph_WO <- c(rep(1:length(cve_WO)))
# mse_graph_WO<-cbind(mse_graph_WO,mse_WO)
# mse_graph_WO<-cbind(mse_graph_WO,name_graph_WO)
# 
# 
# colnames(mse_graph_WO)<- c('errorWO_train','errorWO_test','model')
# graphdata_WO<-wideToLong(mse_graph_WO,within = "Set")
# graphdata_WO$model<-as.factor(graphdata_WO$model)
# graphdata_WO$Set<-as.factor(graphdata_WO$Set)
# 
# ## Write plots to external PDF file:
# pdf(paste0(plotDir, "predictionErrors_WO.pdf"), onefile = TRUE)
# ggplot(graphdata_WO, aes(x=model, fill=Set, y=errorWO)) +
#   geom_bar(position=position_dodge(width = 0.7), stat="identity", width = 0.6) +
#   coord_cartesian(ylim = c(2.2,2.8)) +
#   scale_y_continuous( breaks = c(seq(2.2,2.8,0.05))) +
#   theme_classic()
# dev.off()
