## ----loading, message = FALSE-------------------------------------------------
library(SlidingWindowReg)

data("sampleWatershed")

## ----splits, warning=FALSE----------------------------------------------------
library(lubridate) # package to handle date formats
hydr_year <- cumsum(format(sampleWatershed$date, "%d.%m.") == "01.10.") # determine hydrological years (Oct 01 to Sep 30)

train_inds <- which(hydr_year <= 30)

## ----execute------------------------------------------------------------------
mod <- SlidingWindowReg::train(sampleWatershed$rain[train_inds],
                               sampleWatershed$gauge[train_inds],
                               iter = 3,
                               runs = 1,
                               parallel = FALSE,
                               param_selection = "best_bic")

## ----summary------------------------------------------------------------------
summary(mod)

## ----evaluate_param-----------------------------------------------------------
# window parameters
print(mod$param)

# regression parameters
print(mod$mix)

## ----evaluate_param_plot, warning = FALSE, fig.width = 5, fig.height = 3, fig.align = 'center'----
plot_kernel(param = mod$param, 
            mix = mod$mix,
            include_text = FALSE)

