#' Confidence interval of the mean under a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param sd_est Estimated standard deviation; s_exp > 0.
#' @param x_est Sample mean.
#' @param n_real Real sample size; n_real > 0.
#' @param N A positive integer indicating the number of elements in the population. Defaults to infinite.
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#'
#' @return The function returns the confidence interval of the population mean.
#' @export
#'
#' @details
#' The function to calculate the confidence interval is:
#' \deqn{CI = \hat{x} \pm MOE}
#' where:
#' \deqn{MOE = Z \cdot \sqrt{\frac{\text{sd}^2}{n} \cdot \frac{(N - n)} {N}}},
#' \eqn{\hat{x}} is the sample mean, 'sd' is parameter 'sd_est', and 'Z' is the quantile of the two-tailed normal distribution function, compatible with the chosen confidence level 'C'.
#' If 'sd_est' is unknown, the t-student is used instead of the normal distribution.
#'
#' @examples cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 250)
#' @examples cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 400)
#' @examples cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 250, N = 5000, parameter = TRUE)

#Confidence interval function
cx_srs <- function(C, sd_est, x_est, n_real, parameter = FALSE, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (sd_est < 0) {
    stop("Parameter 'sd_exp' must be a positive number")
  }

  if (n_real != round(n_real) || n_real <= 0) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Calculate the confidence interval

  fcf <- ifelse(is.infinite(N), 1, ((N - n_real) / (N - 1)))

  sd_x_est <- sd_est / sqrt(n_real) * sqrt(fcf)

  MOE <- ifelse(parameter == TRUE,
               qnorm(C + (1 - C) / 2, 0, 1), # qnorm: quantile of the normal distribution
               qt(C + (1 - C) / 2, N) # qt: quantile of the t-student distribution
               ) * sd_x_est

  p_upper <- round(x_est + MOE, 3)

  p_lower <- round(x_est - MOE, 3)

  inference <- paste0("The population mean is between ", p_lower, " and ", p_upper, " with ", C * 100, "% confidence.")

  return(list(x_est = x_est, margin_of_error = MOE, inference = inference))

}
