# try out GAMs using Nick Clark's physalia forecasting course: https://github.com/nicholasjclark/physalia-forecasting-course

# update any installed R packages
update.packages(ask = FALSE, checkBuilt = TRUE)

# packages to install
pkgs <- c("brms", "dplyr", "gratia", "ggplot2",
          "marginaleffects", "tidybayes", "zoo",
          "viridis", "remotes")

# install those packages by setting Ncpus to number of CPU cores you have available
install.packages(pkgs, Ncpus = 4)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")
check_cmdstan_toolchain()

# Download and install mvgam
remotes::install_github('nicholasjclark/mvgam', force = TRUE)

# use mvgam's plot utility to view properties of the observations
plot_mvgam_series(data = model_data, y = 'count')

# yearly random intercept
year_random <- mvgam(count ~ 
                       s(year, bs = 're') - 1,
                     family = poisson(),
                     data = model_data,
                     trend_model = 'None',
                     burnin = 500,
                     samples = 500,
                     chains = 4)