test_that("AIC and BIC", {
  set.seed(1)
  data("sampleWatershed")
  mod <- trainSWR(ts_input = sampleWatershed[1:7000,]$rain,
                  ts_output = sampleWatershed[1:7000,]$gauge,
                  iter = 3,
                  runs = 1,
                  parallel = FALSE)

  expect_equal(AIC(mod,
                   ts_input = sampleWatershed[1:7000,]$rain,
                   ts_output = sampleWatershed[1:7000,]$gauge), 38587.61, info = "number of regression parameters", tolerance = 5e-6)
  expect_equal(BIC(mod,
                   ts_input = sampleWatershed[1:7000,]$rain,
                   ts_output = sampleWatershed[1:7000,]$gauge), 38628.73, info = "number of regression parameters", tolerance = 5e-6)
})


test_that("RMSE and R2 score", {
  set.seed(1)
  data("sampleWatershed")
  mod <- trainSWR(ts_input = sampleWatershed[1:7000,]$rain,
                  ts_output = sampleWatershed[1:7000,]$gauge,
                  iter = 3,
                  runs = 1,
                  parallel = FALSE)
  pred <- predict(mod,
                  newdata = sampleWatershed[7001:10000,]$rain)
  expect_equal(rmse(pred, sampleWatershed[7001:10000,]$gauge), 3.444031, info = "number of regression parameters", tolerance = 5e-6)
  expect_equal(r2(pred, sampleWatershed[7001:10000,]$gauge), 0.754129, info = "number of regression parameters", tolerance = 5e-6)
})
