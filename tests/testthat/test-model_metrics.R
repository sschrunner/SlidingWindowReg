test_that("AIC and BIC", {
  set.seed(1)
  data("sampleWatershed")
  mod <- createSWR(param = cbind(c(1.5,3.5), c(0.05, 5.8)),
                   mix = c(0.4, 0.3))

  expect_equal(AIC(mod,
                   ts_input = sampleWatershed[1:7000,]$rain,
                   ts_output = sampleWatershed[1:7000,]$gauge), 40124.8, info = "number of regression parameters", tolerance = 5e-6)
  expect_equal(BIC(mod,
                   ts_input = sampleWatershed[1:7000,]$rain,
                   ts_output = sampleWatershed[1:7000,]$gauge), 40165.92, info = "number of regression parameters", tolerance = 5e-6)
})


test_that("RMSE and R2 score", {
  set.seed(1)
  data("sampleWatershed")
  mod <- createSWR(param = cbind(c(1.5,3.5), c(0.05, 5.8)),
                   mix = c(0.4, 0.3))

  pred <- predict(mod,
                  newdata = sampleWatershed[7001:10000,]$rain)
  expect_equal(rmse(pred, sampleWatershed[7001:10000,]$gauge), 3.92781, info = "number of regression parameters", tolerance = 5e-6)
  expect_equal(r2(pred, sampleWatershed[7001:10000,]$gauge), 0.6802026, info = "number of regression parameters", tolerance = 5e-6)
})
