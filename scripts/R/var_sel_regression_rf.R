#######################################################################################
## modified by WW from LakeTrophicModeling Repo, Hollister et al. 2016
# https://github.com/USEPA/LakeTrophicModelling/blob/master/R/varsel_regression_rf.R
#######################################################################################

#' Variable Selection with Regression RF
#' 
#' Similar approach to varSelRF, but for regresssion.  Use full model to rank
#' variables based on either %IncMSE or Gini.  To use %IncMSE set 
#' imporatance = TRUE.  It then steps through that sorted variable list with 
#' most important first and runs RF, store variables and %MSE of model.
#' Note: this sorts on Gini, by default.  importance = TRUE ensures %IncMSE.
#' @param y response a vector 
#' @param x predictors a data.frame
#' @param ... options to pass to randomForest
#' @export
#' @import randomForest
#' @examples
#' data(LakeTrophicModelling)
#' predictors_all <- predictors_all[predictors_all!="DATE_COL"]
#' all_dat <- data.frame(ltmData[predictors_all],LogCHLA=log10(ltmData$CHLA))
#' all_dat <- all_dat[complete.cases(all_dat),]
#' x<-varsel_regression_rf(all_dat$LogCHLA,all_dat[,names(all_dat)!="LogCHLA"],
#'                         ntree=100,prog=T, importance = TRUE)




var_sel_regression_rf <- function(target, data, mtry = ncol(data)/3, ntree = 5000, prog = T){
  out <- list(mse=NULL,rsq=NULL,num_var=NULL,vars=NULL)
  fml <- as.formula(paste0(target, " ~ ."))

  init_rf <- randomForest(formula = fml,
                          data = data, 
                          importance = TRUE,
                          ntree = ntree,
                          mtry= mtry)
    
  init_imp <- importance(init_rf)
  var_sort <- rownames(init_imp)[order(init_imp[,1],decreasing = TRUE)]
  vars <- NULL
  for(i in var_sort){
    if(is.null(vars)){ 
      vars <- c(vars,i) 
      idx <- 1
    } else {
      vars <- c(vars,i)
      vars_rf <- randomForest(formula = fml,
                              data = data, 
                              importance = TRUE,
                              ntree = ntree,
                              mtry= mtry)
      out$mse[[idx]] <- vars_rf$mse[length(vars_rf$mse)]
      out$rsq[[idx]] <- vars_rf$rsq[length(vars_rf$rsq)]
      out$num_var[[idx]] <- length(vars)
      out$vars[[idx]] <- vars
      idx <- idx + 1
    }
    if(prog & idx%%3==0){
      print(paste0(round(idx/length(var_sort)*100,1),"% completed"))
    }
  }
  return(out)
}
