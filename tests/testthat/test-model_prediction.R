test_that("model training, single-run, single-window", {
  set.seed(1)
  data("sampleWatershed")
  mod <- createSWR(param = t(c(0,0.01)),
                   mix = 1)
  expect_equal(length(mod$mix), 1, info = "number of regression parameters")
  expect_equal(nrow(mod$param), 1, info = "number of windows")
  expect_equal(
    predict(mod,
            newdata = 1:20)[-1],
    2:20,
    info = "predictions",
    tolerance = 5e-5
  )
})

test_that("model training, single-run, multi-window", {
  set.seed(1)
  data("sampleWatershed")
  mod <- createSWR(param = cbind(c(0, 2), c(0.01, 0.01)),
                   mix = c(2, 1))
  expect_equal(length(mod$mix), 2, info = "number of regression parameters")
  expect_equal(nrow(mod$param), 2, info = "number of regression parameters")
  expect_equal(
    predict(mod,
            newdata = 1:20)[-(1:3)],
    2 * 4:20 + 1 * 2:18,
    info = "predictions",
    tolerance = 5e-5
  )
})


