#' A function helping to create test cases
helper <- function(visit_dates) {
  # prepare sample data
  patient_id <- 888
  visit_date <- visit_dates

  test_visits <- dplyr::tibble(
    patient_id = patient_id,
    visit_date = visit_date,
    visit_id = 1:2,
    phase = c("depression", "mania")
  )

  auto_conf <- auto_create_phases_config(test_visits, phase)
  expanded_visits <- expand_ground_truth_period(
    d = test_visits,
    config = auto_conf,
    phases_col = phase,
    visit_date_col = visit_date
  )

  list(
    conf = auto_conf,
    visits = expanded_visits
  )
}

test_that("overlapping time windows are solved properly 2 days", {
  d <- helper(as.Date(c("2023-02-01", "2023-02-03")))
  auto_conf <- d$conf
  extended_test_visits <- d$visits

  test_cases <- list(
    add_confidence(
      auto_conf,
      values = 1
    ),
    add_confidence(
      auto_conf,
      values = c(rep(c(1, .7), c(8, 2)))
    ),
    add_confidence(
      auto_conf,
      func = dnorm
    )
  )

  time_points <- lapply(test_cases, function(i) {
    out <- transform_overlapping_phases(extended_test_visits, i)
    expect_equal(nrow(out), 12)
    ex_out <-
      c(
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'mania',
        'mania',
        'mania',
        'mania'
      )
    expect_equal(ex_out, out$phase)
    out$time_point
  })
  expect_equal(time_points[[1]], c(-7:0, -1:2))
  expect_equal(time_points[[2]], c(-7:0, -1:2))
  expect_equal(time_points[[3]], c(-7:0, -1:2))
})


test_that("overlapping time windows are solved properly 1 day", {
  d <- helper(as.Date(c("2023-02-01", "2023-02-02")))
  auto_conf <- d$conf
  extended_test_visits <- d$visits

  test_cases <- list(
    add_confidence(
      auto_conf,
      values = 1
    ),
    add_confidence(
      auto_conf,
      values = c(rep(c(1, .7), c(8, 2)))
    ),
    add_confidence(
      auto_conf,
      func = dnorm
    )
  )

  time_points <- lapply(test_cases, function(i) {
    out <- transform_overlapping_phases(extended_test_visits, i)
    expect_equal(nrow(out), 11)
    ex_out <-
      c(
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'depression',
        'mania',
        'mania',
        'mania'
      )
    expect_equal(ex_out, out$phase)
    out$time_point
  })
  expect_equal(time_points[[1]], c(-7:0, 0:2))
  expect_equal(time_points[[2]], c(-7:0, 0:2))
  expect_equal(time_points[[3]], c(-7:0, 0:2))
})


test_that("overlapping time windows are solved properly 4 days", {
  d <- helper(as.Date(c("2023-02-01", "2023-02-05")))
  auto_conf <- d$conf
  extended_test_visits <- d$visits

  test_cases <- list(
    add_confidence(
      auto_conf,
      values = 1
    ),
    add_confidence(
      auto_conf,
      values = c(rep(c(1, .7), c(8, 2)))
    ),
    add_confidence(
      auto_conf,
      func = dnorm
    )
  )

  time_points <- lapply(test_cases, function(i) {
    out <- transform_overlapping_phases(extended_test_visits, i)
    expect_equal(nrow(out), 14)
    out$time_point
  })
  expect_equal(time_points[[1]], c(-7:1, -2:2))
  expect_equal(time_points[[2]], c(-7:0, -3:2))
  expect_equal(time_points[[3]], c(-7:1, -2:2))
})


test_that("overlapping time windows are solved properly 5 days", {
  d <- helper(as.Date(c("2023-02-01", "2023-02-06")))
  auto_conf <- d$conf
  extended_test_visits <- d$visits

  test_cases <- list(
    add_confidence(
      auto_conf,
      values = 1
    ),
    add_confidence(
      auto_conf,
      values = c(rep(c(1, .7), c(8, 2)))
    ),
    add_confidence(
      auto_conf,
      func = dnorm
    )
  )

  time_points <- lapply(test_cases, function(i) {
    out <- transform_overlapping_phases(extended_test_visits, i)
    expect_equal(nrow(out), 15)
    out$time_point
  })
  expect_equal(time_points[[1]], c(-7:2, -2:2))
  expect_equal(time_points[[2]], c(-7:0, -4:2))
  expect_equal(time_points[[3]], c(-7:2, -2:2))
})
