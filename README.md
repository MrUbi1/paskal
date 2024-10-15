
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paskal

<!-- badges: start -->
<!-- badges: end -->

The goal of paskal is to assist performance auditors with some useful
calculations related to statistical sampling, including functions to
calculate the sample size, the sampling error, or the confidence
interval; when estimating the proportion, the mean, or the total through
different sampling plans.

## Installation

You can install paskal from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MrUbi1/paskal", build_vignettes = TRUE)
```

## Example

How many measurements should we make to be 95% confident that the
average transaction duration at a government agency is no more than 2
minutes up or down from the true value? We don’t know the population
standard deviation, but a small pilot sample gave us sd = 6.43 minutes.
We can use simple random sampling. A basic code to solve this problem
could be as follows:

``` r
library(paskal)

# Arguments
C <- 0.95 # Level of confidence
E <- 2 # Sampling error
sd_exp = 6.43 # Expected standard deviation
N = Inf # Population size

# Function
n <- nx_srs(C = C, E = E, sd_exp = sd_exp, N = N) # Call the function

# Output
print(paste("Sample size:",n,"measurements")) # Print the resulting sampling size
#> [1] "Sample size: 40 measurements"
```

If you’re still unsure if the package is right for you, take a look at
the “Intro to paskal” vignette. There you’ll find three use cases that
demonstrate the full scope of this library.

Much of the formulas that make up the functions of the package are based
on those included in Scheaffer, R. et al. 2007. Elementos de muestreo.
6ª edición. Madrid: International Thomson Editores Spain Paraninfo.
