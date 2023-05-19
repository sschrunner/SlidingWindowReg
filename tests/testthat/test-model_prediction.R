test_that("model training, single-run, single-window", {
  set.seed(1)
  data("sampleWatershed")
  mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
                  ts_output = sampleWatershed[1:1000,]$gauge,
                  iter = 1,
                  runs = 1,
                  parallel = FALSE)
  expect_equal(length(mod$mix), 1, info = "number of regression parameters")
  expect_equal(nrow(mod$param), 1, info = "number of windows")
  expect_equal(
    predict(mod,
            newdata = sampleWatershed[1001:1010,]$rain),
    c(0, 0, 0, 0.2422, 0.1491, 1.8770, 7.8509, 9.5720, 3.1325, 0.4844),
    info = "predictions",
    tolerance = 0.0005
  )
})

# test_that("model training, multi-run, single-window", {
#   set.seed(1)
#   data("sampleWatershed")
#   mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
#                   ts_output = sampleWatershed[1:1000,]$gauge,
#                   iter = 1,
#                   runs = 3,
#                   parallel = FALSE)
#   expect_equal(length(mod$mix), 1, info = "number of regression parameters")
#   expect_equal(nrow(mod$param), 1, info = "number of regression parameters")
#   expect_equal(
#     predict(mod,
#             newdata = sampleWatershed[1001:1010,]$rain),
#     c(0, 0, 0, 0.2422, 0.1491, 1.8770, 7.8509, 9.5720, 3.1325, 0.4844),
#     info = "predictions",
#     tolerance = 0.0005
#   )
# })


test_that("model training, single-run, multi-window", {
  set.seed(1)
  data("sampleWatershed")
  mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
                  ts_output = sampleWatershed[1:1000,]$gauge,
                  iter = 3,
                  runs = 1,
                  parallel = FALSE)
  expect_equal(length(mod$mix), 2, info = "number of regression parameters")
  expect_equal(nrow(mod$param), 2, info = "number of regression parameters")
  expect_equal(
    predict(mod,
            newdata = sampleWatershed[1001:1015,]$rain),
    c(0, 0, 0.2822, 0.0726, 0.0333, 0.3791, 4.6886, 6.0611, 0.4926, 0.1732,
      0.0631, 1.1259, 0.7059, 0.4344, 0.2486),
    info = "predictions",
    tolerance = 0.0005
  )
})


# test_that("model training, multi-run, multi-window", {
#   set.seed(1)
#   data("sampleWatershed")
#   mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
#                   ts_output = sampleWatershed[1:1000,]$gauge,
#                   iter = 3,
#                   runs = 3,
#                   parallel = FALSE)
#   expect_equal(length(mod$mix), 3, info = "number of regression parameters")
#   expect_equal(nrow(mod$param), 3, info = "number of regression parameters")
#   expect_equal(
#     predict(mod,
#             newdata = sampleWatershed[1001:1010,]$rain),
#     c(0, 0, 0.2822, 0.0726, 0.0333, 0.3791, 4.6886, 6.0611, 0.4926, 0.1732,
#       0.0631, 1.1259, 0.7059, 0.4344, 0.2486),
#     info = "predictions",
#     tolerance = 0.0005
#   )
# })
#
# test_that("model training, multi-run, multi-window, parallel", {
#   set.seed(1)
#   data("sampleWatershed")
#   mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
#                   ts_output = sampleWatershed[1:1000,]$gauge,
#                   iter = 2,
#                   runs = 4,
#                   parallel = 2)
#   expect_equal(length(mod$mix), 2, info = "number of regression parameters")
#   expect_equal(nrow(mod$param), 2, info = "number of regression parameters")
#   expect_equal(
#     predict(mod,
#             newdata = sampleWatershed[1001:1010,]$rain),
#     c(0, 0, 0.2593, 0.0698, 0.1980, 1.8877, 7.6955, 9.3048, 3.4678, 1.0874),
#     info = "predictions",
#     tolerance = 0.0005
#   )
# })


