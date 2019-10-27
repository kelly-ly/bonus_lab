context("linreg")
context("MASS")
data("iris")

test_that("coef not equal", {
  l1<-linreg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=0)
  l2<-lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, iris,lambda=0)
  expect_equal(unname(l1$coef()[-1]),unname(l2$coef/l2$scales))
})
