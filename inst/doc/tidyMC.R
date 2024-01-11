## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 7, 
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("stefanlinner/tidyMC")

## -----------------------------------------------------------------------------
library(tidyMC)

## ----warning=FALSE, message=FALSE---------------------------------------------
# install.packages("magrittr")
library(magrittr)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("kableExtra")
library(kableExtra)

## -----------------------------------------------------------------------------
# fun
ols_test <- 
  function(b0, b1, b2, n, sigma2, param_x1, param_x2, inc_x2){
    
    # generation of data
    x1 <- rnorm(n = n, mean = param_x1[1], sd = param_x1[2])
    x2 <- rnorm(n = n,  mean = param_x2[1], sd = param_x2[2])
    e <- rnorm(n, sd = sqrt(sigma2))
    y <- b0 + b1*x1 + b2*x2 + e
    
    if (inc_x2 == 0){
      x2 <- x2 * inc_x2
    }
    
    # application of method
    estim <- lm(y ~ x1 + x2)
    
    # evaluation of the result for a single repetition and parameter combination
    out <- list(b0 = estim$coefficients[1],
                b1 = estim$coefficients[2],
                b2 = estim$coefficients[3],
                sigma2 = var(estim$residuals))
    return(out)
  }

## -----------------------------------------------------------------------------
# param_list
param_list_ols <- 
  list(n = c(100, 200, 300), inc_x2 = c(0,1))

## ----error=TRUE, warning=FALSE------------------------------------------------
set.seed(101)
first_mc_ols <- future_mc(
  fun = ols_test, 
  repetitions = 1000, 
  param_list = param_list_ols, 
  b0 = 1, 
  b1 = 4, 
  b2 = 5, 
  sigma2 = -2,
  param_x1 = c(0,5),
  param_x2 = c(0,6),
  check = TRUE
)

## -----------------------------------------------------------------------------
set.seed(101)
first_mc_ols <- future_mc(
  fun = ols_test, 
  repetitions = 1000, 
  param_list = param_list_ols, 
  b0 = 1, 
  b1 = 4, 
  b2 = 5, 
  sigma2 = 2, # correctly specify sigma2
  param_x1 = c(0,5),
  param_x2 = c(0,6),
  check = TRUE
)

## -----------------------------------------------------------------------------
first_mc_ols$output

## -----------------------------------------------------------------------------
first_mc_ols

## -----------------------------------------------------------------------------
# Default
summary_default <- summary(first_mc_ols)
summary_default
str(summary_default[[1]])

## ----eval = FALSE-------------------------------------------------------------
#  # summarizing output for each parameter combination with one combination
#  sum_funs_ols <- list(b0 = mean, b1 = mean , b2 = mean, sigma2 = var)

## -----------------------------------------------------------------------------
quantile_sum <- function(x) quantile(x, probs = 0.75)

# summarizing output differently for different parameter combinations
sum_funs2 <-
  list(
    list(b0 = quantile_sum, b1 = min, b2 = min, sigma2 = mean),
    list(b0 = mean, b1 = quantile_sum, b2 = mean, sigma2 = mean),
    list(b0 = median, b1 = median, b2 = median, sigma2 = mean),
    list(b0 = max, b1 = max, b2 = max, sigma2 = mean),
    list(b0 = min, b1 = min, b2 = min, sigma2 = quantile_sum),
    list(b0 = mean, b1 = mean, b2 = quantile_sum, sigma2 = quantile_sum)
  )
names(sum_funs2) <- first_mc_ols$nice_names
summary_out_param_spec <- summary(first_mc_ols, sum_funs = sum_funs2)

## -----------------------------------------------------------------------------
mc_ols_plot <- plot(first_mc_ols, plot = FALSE)
names(mc_ols_plot)

## -----------------------------------------------------------------------------
mc_ols_plot$b1 + 
  ggplot2::geom_vline(xintercept = 4, col = "red") + 
  ggplot2::theme_minimal()

## -----------------------------------------------------------------------------
# subsetting by nice_names
mc_ols_plot_subset1 <- 
  plot(first_mc_ols, plot = FALSE, which_setup = first_mc_ols$nice_names[4:6])
#subsetting by parameter values
mc_ols_plot_subset2 <- 
  plot(first_mc_ols, plot = FALSE, parameter_comb = list(inc_x2 = 1))

mc_ols_plot_subset1$sigma2

## -----------------------------------------------------------------------------
mc_ols_plot_joint <- plot(first_mc_ols, plot = FALSE, 
                          join = first_mc_ols$nice_names)
mc_ols_plot_joint$b2

## -----------------------------------------------------------------------------
sum_mc_plot <- plot(summary_default, plot = FALSE)
sum_mc_plot$b1 + 
  ggplot2::geom_vline(xintercept = 100, col = "red") +
  ggplot2::theme(axis.text.x = element_text(angle = 45, 
                                            hjust = 0.1, 
                                            vjust = 0.2))

sum_mc_plot_subset1 <- 
  plot(summary_default, 
       plot = FALSE, 
       which_setup = first_mc_ols$nice_names[4:6])

sum_mc_plot_subset2 <- 
  plot(summary_default, 
       plot = FALSE, 
       parameter_comb = list(inc_x2 = 1))

sum_mc_plot_subset2$b1

sum_mc_plot_joint <- 
  plot(summary_default, plot = FALSE, join = first_mc_ols$nice_names[4:6])

sum_mc_plot_joint$b1

## -----------------------------------------------------------------------------
tidy_mc_latex(
  x = summary(first_mc_ols),
  repetitions_set = c(10, 1000)
) %>% 
  print()

## -----------------------------------------------------------------------------
tidy_mc_latex(
  x = summary(first_mc_ols),
  repetitions_set = c(10, 1000),
  which_setup = first_mc_ols$nice_names[1]) %>% 
  print()

tidy_mc_latex(
  x = summary(first_mc_ols),
  repetitions_set = c(10, 1000),
  parameter_comb = list(n = 100, inc_x2 = 1)) %>% 
  print()

## -----------------------------------------------------------------------------
tidy_mc_latex(summary(first_mc_ols), 
              repetitions_set = c(10, 1000),
              kable_options = list(
                col.names = c("Number of observations",
                              "$x_2$ included or not",
                              "$\\beta_0$", "$\\beta_1$",
                              "$\\beta_2$", "$s^2$"), 
                caption = "Ommited variable bias MC results"
              )
) %>%
  kableExtra::kable_styling(latex_options = "HOLD_position") %>% 
  print()

