library(testthat)

context("Test the output of homework 2.")

test_that("You ridge regression function works in glmnet case.", {

  data(iris)

  fit_ridge1 <- ridge_regression1(Sepal.Length ~ ., iris)

  fit_glmnet <- glmnet(model.matrix(Sepal.Length ~ ., iris)[,-1],data.matrix(iris[[as.character(Sepal.Length ~ .)[2]]]), alpha=0, lambda = 10, standardize = FALSE, intercept = FALSE,thresh = 1e-20)
  beta2 <- data.matrix(as.vector(coef(fit_glmnet, exact = TRUE, x=model.matrix(Sepal.Length ~ ., iris)[,-1], y=data.matrix(iris[[as.character(Sepal.Length ~ .)[2]]]))))

  expect_equivalent(fit_ridge1, beta2,
                    tolerance = 1e-5)
})

context("Test the output of homework 2.")

test_that("Your best lambda match with cv.glmnet", {

  set.seed(1)

  bestlam1<- best.lambda(Sepal.Length ~., iris, seq(0,1,0.01))

  best.glmnet <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris)[,-1],iris$Sepal.Length, alpha=0, lambda = seq(0,1,0.01))
  bestlam2 <- best.glmnet$lambda.1se

  expect_equivalent(bestlam1, bestlam2,
                    tolerance = 1e-5)
})
