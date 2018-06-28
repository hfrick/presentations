context("test-note.R")

test_that("calc_frequency works", {
  expect_equal(calc_frequency("A", 0), 440)
  expect_equal(calc_frequency("A", -1), 220)
})

test_that("calc_volume works", {
  expect_equal(calc_volume(0.5, force = TRUE), 0.1)
  expect_equal(calc_volume(11, force = TRUE), 1)
  expect_error(calc_volume(0.5, force = FALSE))
})

test_that("calc_length works", {
  expect_equal(calc_length(rate = 6, length = 1, bpm = 120), 1:3)
})

test_that("calc_multiplier works", {
  expect_equal(calc_multiplier(6), 2 * pi / 6)
})

test_that("note returns a note", {
  expect_s3_class(note("A"), "note")
})

test_that("print note works", {
  expect_s3_class(print(note("A")), "audioInstance")
})
