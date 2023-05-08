## ----loading, message = FALSE-------------------------------------------------
library(SlidingWindowReg)

data("sampleWatershed")

## ----splits-------------------------------------------------------------------
train_inds <- 1 : (nrow(sampleWatershed) * 0.75)

## ----execute------------------------------------------------------------------
mod <- SlidingWindowReg::train(sampleWatershed$rain[train_inds],
                               sampleWatershed$gauge[train_inds],
                               iter = 3,
                               runs = 1,
                               parallel = FALSE,
                               param_selection = "best_bic")[[1]]

## ----evaluate_rmse------------------------------------------------------------
pred_on_test <- SlidingWindowReg::predict(sampleWatershed$rain[-train_inds],
                                          mix = mod$mix,
                                          param = mod$param)

print(rmse(pred_on_test, sampleWatershed$gauge[-train_inds]))

## ----evaluate_plot, width = 20, height = 12-----------------------------------
plot_prediction(pred_on_test,
                sampleWatershed$gauge[-train_inds],
                sampleWatershed$rain[-train_inds])

## ----evaluate_param-----------------------------------------------------------
# window parameters
print(mod$param)

# regression parameters
print(mod$mix)

## ----evaluate_param_plot, warning = FALSE, width = 12, height = 8-------------
plot_kernel(param = mod$param, 
            mix = mod$mix,
            include_text = FALSE)

