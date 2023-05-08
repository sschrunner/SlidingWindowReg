test_that("model training, single-run, single-window", {
  set.seed(1)
  data("sampleWatershed")
  mod <- train(ts_input = sampleWatershed[1:1000,]$rain,
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
  mod <- train(ts_input = sampleWatershed[1:1000,]$rain,
               ts_output = sampleWatershed[1:1000,]$gauge,
               iter = 3,
               runs = 1,
               parallel = FALSE)
  expect_equal(length(mod$mix), 3, info = "number of regression parameters")
  expect_equal(nrow(mod$param), 3, info = "number of regression parameters")
  expect_equal(mod$mix,
               c(0.3867506, 0.2070009, 0.2344473),
               info = "regression parameters", tolerance = 5e-7)
  expect_equal(as.vector(mod$param),
               c(0.7406802, 0.8904870, 2.2136074, 0.3011700, 0.4051864, 0.8375424),
               info = "window parameters", tolerance = 5e-7)
})

test_that("model training, multi-run, multi-window, parallel", {
  set.seed(1)
  data("sampleWatershed")
  mod <- train(ts_input = sampleWatershed[1:1000,]$rain,
               ts_output = sampleWatershed[1:1000,]$gauge,
               iter = 2,
               runs = 4,
               parallel = 2)
  expect_equal(mod$mix,
               c(0.4581691, 0.3634183),
               info = "regression parameters", tolerance = 5e-7)
  expect_equal(as.vector(mod$param),
               c(0.6504002, 1.5920654, 0.2118891, 0.7844528),
               info = "window parameters", tolerance = 5e-7)
})


