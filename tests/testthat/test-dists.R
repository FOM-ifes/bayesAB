library(bayesAB)
context('dists')

dummyDist <- plotDist('norm', 'Normal', c('mu', 'sd'))
dummyDist2 <- plotDist('norm', 'Normal', c('mu', 'sd'))

test_that("Failures based on inputs", {

  expect_error(dinvgamma(5, -1, 5), "Shape or scale parameter negative")

})

test_that("Closure madness", {

  expect_equal(dummyDist, dummyDist2)
  expect_identical(environment(dummyDist)$distArgs, environment(dummyDist2)$distArgs)
  expect_equal(environment(dummyDist)$name, 'Normal')
  expect_equal(formals(dummyDist), as.pairlist(alist(mu =, sd =)))
})

test_that("Success", {
  
  get_labs <- function(x) x$labels
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    get_labs <- ggplot2::get_labs
  }

  expect_equal(get_labs(plotPoisson(1))$y, 'PDF')
  expect_equal(get_labs(plotPareto(1, 1))$y, 'PDF')
  expect_equal(get_labs(plotNormal(1, 1))$y, 'PDF')
  expect_equal(get_labs(plotGamma(1, 1))$y, 'PDF')
  expect_equal(get_labs(plotBeta(1, 1))$y, 'PDF')
  expect_equal(get_labs(plotInvGamma(1, 1))$y, 'PDF')
  expect_equal(get_labs(plotLogNormal(1, 1))$y, 'PDF')
  expect_equal(qinvgamma(1 - (.Machine$double.eps) / 2, 2, 2), Inf)
  expect_equal(dpareto(c(0, 1, 2), 1, 1), c(0, 0, .25))
  expect_equal(dpareto(c(5, 15), 20, 3), c(0, 0))
  expect_equal(max(plotNormalInvGamma(3, 100, 51, 216)$data$sig_sq), qgamma(.99, 51, 216) * 100)
  expect_equal(get_labs(plotNormalInvGamma(3, 1, 1, 1))$y, 'sig_sq')

})
