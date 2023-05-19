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
  expect_equal(mod$mix,
               0.8000348,
               info = "regression parameters", tolerance = 5e-7)
  expect_equal(as.vector(mod$param),
               c(1.031635, 0.5664964),
               info = "window parameters", tolerance = 5e-7)
})

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
  expect_equal(mod$mix,
               c(0.4804059, 0.4096644),
               info = "regression parameters", tolerance = 5e-7)
  expect_equal(as.vector(mod$param),
               c(0.1942933, 0.8985493, 3.5996312, 0.2091464),
               info = "window parameters", tolerance = 5e-7)
})

# test_that("model training, multi-run, multi-window, parallel", {
#   set.seed(1)
#   data("sampleWatershed")
#   mod <- trainSWR(ts_input = sampleWatershed[1:1000,]$rain,
#                   ts_output = sampleWatershed[1:1000,]$gauge,
#                   iter = 2,
#                   runs = 4,
#                   parallel = 2)
#   expect_equal(mod$mix,
#                c(0.4804059, 0.4096644),
#                info = "regression parameters", tolerance = 5e-7)
#   expect_equal(as.vector(mod$param),
#                c(0.1942933, 0.8985493, 3.5996312, 0.2091464),
#                info = "window parameters", tolerance = 5e-7)
# })


