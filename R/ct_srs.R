#' Confidence interval of the total under a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param sd_est Expected standard deviation; s_exp > 0.
#' @param t_est Sample total.
#' @param n_real Real sample size; n_real > 0.
#' @param N A positive integer indicating the number of elements in the population. Defaults to infinite.
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#'
#' @details
#' The function to calculate the confidence interval is:
#' \deqn{CI = \hat{t} \pm MOE}
#' where:
#' \deqn{MOE = Z \cdot \sqrt{ \frac{\text{sd}^2}{n} \cdot \frac{(N - n)} {N} \cdot N^2}}
#' \eqn{\hat{t}} is the sample total, 'sd' is parameter 'sd_est', and 'Z' is the quantile of the two-tailed normal distribution function, compatible with the chosen confidence level 'C'.
#' If 'sd_est' is unknown, the t-student is used instead of the normal distribution.
#'
#' @return The function returns the confidence interval of the population total.
#' @export
#'
#' @examples ct_srs(C = 0.95, t_est = 3500, sd_est = 4.1, n_real = 87, N = 1200)
#' @examples ct_srs(C = 0.95, t_est = 3500, sd_est = 4.1, n_real = 87, N = 1200, parameter = TRUE)

#Confidence interval function
ct_srs <- function(C, sd_est, t_est, n_real, parameter = FALSE, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (sd_est < 0) {
    stop("Parameter 'sd_est' must be a positive number")
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

  sd_t_est <- ifelse(is.infinite(N), 1, N) * sd_est / sqrt(n_real) * sqrt(fcf)

  MOE <- ifelse(parameter == TRUE,
               qnorm(C + (1 - C) / 2, 0, 1), # qnorm: quantile of the normal distribution
               qt(C + (1 - C) / 2, N) # qt: quantile of the t-student distribution
               ) * sd_t_est

  p_upper <- round(t_est + MOE, 3)

  p_lower <- round(t_est - MOE, 3)

  inference <- paste0("The population total is between ", p_lower, " and ", p_upper, " with ", C * 100, "% confidence.")

  return(list(t_est = t_est, margin_of_error = MOE, inference = inference))

}



