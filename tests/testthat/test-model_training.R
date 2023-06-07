test_that("model training, single-run, single-window", {
  set.seed(1)
  data("sampleWatershed")
  mix_gt <- 2
  param_gt <- t(c(5,2))
  mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
                  ts_output = predict(createSWR(param = param_gt,
                                                mix = mix_gt),
                                      sampleWatershed[1:1000,]$rain) + rnorm(1000,0,0.5),
                  iter = 1)
  expect_equal(length(mod$mix), 1, info = "number of regression parameters")
  expect_equal(nrow(mod$param), 1, info = "number of windows")
  expect_equal(abs(mod$mix - mix_gt),
               0,
               info = "regression parameters", tolerance = 5e-2)
  expect_equal(max(abs(mod$param - param_gt)),
               0,
               info = "window parameters", tolerance = 5e-2)
})

test_that("model training, single-run, multi-window", {
  set.seed(1)
  data("sampleWatershed")
  mix_gt <- c(0.5, 2)
  param_gt <- cbind(c(1,5), c(1,3))
  mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
                  ts_output = predict(createSWR(param = param_gt,
                                                mix = mix_gt),
                                      sampleWatershed[1:1000,]$rain) + rnorm(1000,0,0.5),
                  iter = 3)
  expect_equal(length(mod$mix), 2, info = "number of regression parameters")
  expect_equal(nrow(mod$param), 2, info = "number of regression parameters")
  expect_equal(max(abs(mod$mix - mix_gt)),
               0,
               info = "regression parameters", tolerance = 5e-2)
  expect_equal(max(abs(mod$param - param_gt)),
               0,
               info = "window parameters", tolerance = 5e-2)
})


