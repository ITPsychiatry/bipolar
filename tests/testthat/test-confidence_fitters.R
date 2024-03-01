TIME_WINDOW <- time_window(c(-1, 1))

test_that("fitting with steps works with single value", {
  out <- fit_with_steps(0.5, TIME_WINDOW)

  expect_true(is.data.frame(out))
  expect_length(out, 2)
  expect_named(out, c("time_point", "confidence"))

  expect_equal(out$confidence, rep(0.5, 3))

  expect_error(fit_with_steps(-1))
})


test_that("fitting with steps works for multi-element vector", {
  vals <- c(.1, .3, .28)
  out <- fit_with_steps(vals, TIME_WINDOW)

  expect_true(is.data.frame(out))
  expect_length(out, 2)
  expect_named(out, c("time_point", "confidence"))

  expect_equal(out$confidence, vals)

  expect_error(fit_with_steps(-1, 0, 1))
})


test_that("fitting with steps throws an error", {
  # when wrong length
  expect_error(fit_with_steps(c(.1, .1), TIME_WINDOW))
  # when wrong input
  expect_error(fit_with_steps(NA, TIME_WINDOW))
  expect_error(fit_with_steps(NA_real_, TIME_WINDOW))
  expect_error(fit_with_steps(-1, TIME_WINDOW))
  expect_error(fit_with_steps(c(.1, .1, .1, .1, -1), TIME_WINDOW))

  # wrong time_window argument
  expect_error(fit_with_steps(c(.1, .1, .1, .1, .1), c(1, 2, 3)))
  expect_error(fit_with_steps(c(.1, .1, .1, .1, .1), c(2, 0)))
})


test_that("fit_with_function works properly", {
  custom_f <- function(...) {
    return(1)
  }
  out <- fit_with_function(custom_f, TIME_WINDOW)
  expected_out <- c(1, 1, 1)
  expect_equal(out$confidence, expected_out)

  custom_f <- function(...) {
    return(c(1, 2, 4))
  }
  out <- fit_with_function(custom_f, TIME_WINDOW)
  expected_out <- c(1, 2, 4)
  expect_equal(out$confidence, expected_out)

  set.seed(1234)
  out <- fit_with_function(dnorm, TIME_WINDOW)
  expected_out <- c(0.241970724519143, 0.398942280401433, 0.241970724519143)
  expect_equal(out$confidence, expected_out)
})


cnf <- readRDS(system.file(
  package = "bipolar",
  file.path("testdata/auto_cnf.RDS")
))

test_that("add_confidence throws error when wrong input", {
  expect_error(add_confidence(cnf, values = 1, func = dunif))
  expect_error(add_confidence(iris, values = 1))
  expect_error(add_confidence(iris, func = dunif))
  expect_error(add_confidence(cnf, func = 1))
  expect_error(add_confidence(cnf, values = function(x) return(1)))
})


test_that("add_confidence works properly with func", {
  out <- add_confidence(cnf, func = function(x) return(1))
  expect_equal(rep(1, 40), out$confidence)

  out <- add_confidence(cnf, func = function(x) return(abs(x)), normalize = FALSE)
  expect_equal(rep(c(7:0, 1:2), 4), out$confidence)

  out <- add_confidence(cnf, func = dunif)
  single_phase_expected <- c(rep(0, 7), c(1, 1, 0))
  expect_equal(rep(single_phase_expected, 4), out$confidence)
})


test_that("add_confidence works properly with values", {
  out <- add_confidence(cnf, values = 1)
  expect_equal(rep(1, 40), out$confidence)

  out <- add_confidence(cnf, values = 2, normalize = FALSE)
  expect_equal(rep(2, 40), out$confidence)

  out <- add_confidence(cnf, values = 2)
  expect_equal(rep(1, 40), out$confidence)

  out <- add_confidence(cnf, values = rep(1:2, 5))
  expect_equal(rep(c(.5, 1), 20), out$confidence)

  out <- add_confidence(cnf, values = 1:10, normalize = FALSE)
  expect_equal(rep(1:10, 4), out$confidence)

  out <- add_confidence(cnf, values = 1:10)
  expect_equal(rep(1:10/10, 4), out$confidence)
})
