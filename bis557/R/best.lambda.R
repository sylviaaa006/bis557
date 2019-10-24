#' Caluculate coefficients using ridge regression and finding the best lambda for ridge regression using cross validation.
#'
#' This is a generic function: we use ridge regression to calculate the linear coeffients between dependent variable and independent wariables and finding the best lambda for ridge regression using cross validation.
#'
#' @param form dependent variable ~ all independent variables
#' @param data the dataframe that will be used to caluculate coefficients
#' @param lambda A user supplied lambda sequence. Typical usage is to have the program compute its own lambda sequence based on nlambda and lambda.min.ratio.
#' @return best.lambda: best lambda
#' @export
#' @examples
#' best.lambda(Sepal.Length ~., iris, seq(0,1, by=0.01))
#' @importFrom stats model.matrix var
#' @import glmnet
#' @import caret
#'

best.lambda <- function(form,data,lambda){
  folds <- createFolds(data[[as.character(form)[2]]][as.numeric(rownames(model.matrix(form, data)))], k=5)
  error <- data.frame(lambda,ave.error=NA)
  for (i in 1:length(lambda)) {
    cv5 <- sapply(folds, function(a) {
      train.fold <- data[-a,]
      test.fold <- data[a,]
      ridge <- ridge_regression1(form, data=train.fold, lambda = lambda[i])
      cv.pred <- as.matrix(model.matrix(form,test.fold)) %*% ridge
      cv.error <- mean((cv.pred-test.fold[[as.character(form)[2]]])^2)
      return(cv.error)
    })
    error[i,2] <- mean(cv5)
  }
  bestlambda <- error$lambda[which.min(error$ave.error)]
  return(bestlambda)
}
