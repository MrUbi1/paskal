#' Confidence interval of the mean under a stratified sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real A vector of positive integers representing the real sample size of each stratum; n_real(i) > 0, for every 'i' stratum.
#' @param x_est A vector with the sample mean of each stratum.
#' @param sd_est A vector with the expected standard deviation in each stratum; sd_exp(i) > 0, for every 'i' stratum.
#' @param N A vector of positive integers representing the number of elements in each stratum; N(i) > 0.
#' @param parameter Type TRUE if you do know the populations SD in sd_exp, or type FALSE (default) if they are estimates.
#'
#' @return This function returns the global confidence interval when using a stratified sampling design without replacement to estimate the mean, given the sample size.
#' @export
#'
#' @details
#' The function to calculate the confidence interval is:
#' \deqn{CI = \hat{x} \pm MOE}
#' where:
#' \deqn{MOE = Z \cdot \sqrt{ \frac{1}{N^2} \cdot \sum_{i=1}^{s} N_i^2  \cdot \frac{\text{sd}_i^2}{n_i} \cdot \frac{(N_i - n_i)}{N_i} }}
#' \eqn{\hat{x}} is the sample mean, 'sd' is the parameter 'sd_est', and 'Z' is the quantile of the two-tailed normal distribution function, compatible with the chosen confidence level 'C'.
#' If 'sd_est' is unknown, the t-student is used instead of the normal distribution.
#'
#' @examples cx_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300), parameter = TRUE)

# Confidence interval function
cx_sts <- function(C, n_real, x_est, sd_est, N, parameter = FALSE) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(n_real != round(n_real)) || any(n_real <= 0)) {
    stop("All elements in 'n_real' must be positive integers")
  }

  if (any(sd_est < 0)) {
    stop("All elements in 'sd_est' must be positive numbers")
  }

  if (any(N != round(N)) || any(N <= 0)) {
    stop("All elements in 'N' must be positive integers")
  }

  # Ensure 'x_est', 'n_real', 'sd_est' and 'N' are of the same length
  if (length(x_est) != length(n_real) ||
      length(x_est) != length(sd_est) ||
      length(x_est) != length(N)) {
    stop("'x_est', 'n_real', 'sd_est' and 'N' must have the same length")
  }

  # Calculate the confidence interval

  sd_x_est <- sqrt(sum(N^2 * (N - n_real) / N * sd_est^2 / n_real) / sum(N)^2)

  MOE <- ifelse(parameter == TRUE,
               qnorm(C + (1 - C) / 2, 0, 1), # qnorm: quantile of the normal distribution
               qt(C + (1 - C) / 2, sum(n_real)) # qt: quantile of the t-student distribution
               ) * sd_x_est

  x_est <- sum(N / sum(N) * x_est)

  p_upper <- round(x_est + MOE, 3)

  p_lower <- round(x_est - MOE, 3)

  inference <- paste0("The population mean is between ", p_lower, " and ", p_upper, " with ", C * 100, "% confidence.")

  return(list(global_x_est = x_est, margin_of_error = MOE, inference = inference))

}
