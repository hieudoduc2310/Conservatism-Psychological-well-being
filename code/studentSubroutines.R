### Title:    Subroutines for Student-Use Functions
### Author:   Kyle M. Lang
### Created:  2019-09-25
### Modified: 2019-09-25
### Note:     This script is meant to be sourced by "studentFunctions.R". You
###           should probably not be using these functions directly.
###--------------------------------------------------------------------------###
### Student note: This script was used by students of the course Stats & Methodology for a group project.
### Group:    2
### Used by:  #Hieu.D.DO        SNR: 2039710      ANR: 107088
              #Hoang Phuc Pham  SNR: 2037626      ANR: 153913
              #Larissa Chua     SNR: 2046216      ANR: 135335
              #Ryan van Velzen  SNR: 2030909      ANR: 483436

###--------------------------------------------------------------------------###

## Extract the DV name from an lm.fit object
## NOTE: This function only works when lm() is run using the fomula interface.
dvName <- function(x) all.vars(x$terms)[1]

###--------------------------------------------------------------------------###

## Compute the cross-validation error:
getCve <- function(model, data, K, part) {
    ## Loop over K repititions:
    mse <- c()
    for(k in 1 : K) {
        ## Partition data:
        train <- data[part != k, ]
        valid <- data[part == k, ]
        
        ## Fit model, generate predictions, and save the MSE:
        fit    <- lm(model, data = train)
        pred   <- predict(fit, newdata = valid)
        mse[k] <- MSE(y_pred = pred, y_true = valid[ , dvName(fit)])
    }
    ## Return the CVE:
    sum((table(part) / length(part)) * mse)
}

###--------------------------------------------------------------------------###
