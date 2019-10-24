#' Caluculate coefficients using ridge regression and finding the best lambda for ridge regression using cross validation.
#'
#' This is a generic function: we use ridge regression to calculate the linear coeffients between dependent variable and independent wariables and finding the best lambda for ridge regression using cross validation.
#'
#' @param form dependent variable ~ all independent variables
#' @param data the dataframe that will be used to caluculate coefficients
#' @param lambda A user supplied lambda sequence. Typical usage is to have the program compute its own lambda sequence based on nlambda and lambda.min.ratio.
#' @return ret: ridge regression coefficients
#' @export
#' @examples
#' ridge_regression1(Sepal.Length ~., iris, lambda=10)
#' @importFrom stats model.matrix var
#' @import glmnet
#'
ridge_regression1 <- function(form, data, lambda = 10) {
  X <- model.matrix(form, data)[,-1]
  Y <- as.matrix(data[[as.character(form)[2]]])
  sd_y <- sqrt(var(Y)*(nrow(X)-1)/nrow(X))[1,1]
  ret <- solve( crossprod(X) + diag(rep(lambda*nrow(X)/sd_y, ncol(X))) ) %*% t(X) %*% Y #crossprod is equal to t(x) %*% y
  attributes(ret)$formula <- form
  class(ret) <- c(class(ret), "ridge_regression")
  ret <- rbind(0,ret)
  ret
}

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
