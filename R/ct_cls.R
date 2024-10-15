#' Confidence interval of the total under a cluster sampling design
#'
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param x_est Sample mean.
#' @param n_real Real sample size (id est, the number of clusters sampled); n_real > 0.
#' @param N A positive integer indicating the population size (id est, the total number of clusters). Defaults to infinite.
#' @param m Average cluster size (number of elements), either of the population -preferred- or of a preliminary sample; m > 0.
#' @param M Number of elements in the population; M > 0. Leave it Null if unknown.
#' @param sd_est Estimated standard deviation; sd_est > 0.
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#'
#' @details
#' The function to calculate the confidence interval if M is unknown is:
#' \deqn{CI = \hat{t} \pm MOE}
#' where:
#' \deqn{MOE = Z \cdot \sqrt{ N^2 \cdot \frac {\text{sd}^2}{n} \cdot \frac{(N - n)} {N} }}
#' \eqn{\hat{t}} is the sample total, 'sd' is parameter 'sd_est', and 'Z' is the quantile of the two-tailed normal distribution function, compatible with the chosen confidence level 'C'.
#' If 'sd_est' is unknown, the t-student is used instead of the normal distribution.
#'
#' Alternatively, if M is known, the function is as follows:
#' \deqn{MOE = Z \cdot \sqrt{ M^2 \cdot \frac {\text{sd}^2}{n} \cdot \frac{(N - n)} {N \cdot m^2} }}
#'
#' @return This function returns the confidence interval of the population total when using a cluster sampling design without replacement, given the sample size.
#' @export
#'
#' @examples ct_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = 8, M = 2500, sd_est = 15000, TRUE)
#' @examples ct_cls(C = 0.95, x_est = 9990, n_real = 19, N = 500, m = 8, sd_est = 15000, parameter = TRUE)


#Confidence interval function
ct_cls <- function(C, x_est, n_real, sd_est, m, N = Inf, M = NULL, parameter = FALSE) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n_real != round(n_real) || n_real <= 0) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (sd_est <= 0) {
    stop("Parameter 'sd_est' must be a positive number")
  }

  if (m <= 0) {
    stop("Parameter 'm' must be a positive number")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  if (!missing(M)) {
    if (M <= 0) {
      stop("Parameter 'M' must be Null or a positive number")
    }
  }

  # Calculate the confidence interval
  N <- ifelse(is.infinite(N), 10^10, N)

  sd_t_est <- ifelse(missing(M),
               sqrt(N^2 * ((N - n_real) / (N * n_real)) * sd_est^2), # unknown M
               sqrt(M^2 * ((N - n_real) / (N * n_real * m^2)) * sd_est^2)) # known M

  MOE <- ifelse(parameter == TRUE,
               qnorm(C + (1 - C) / 2, 0, 1), # qnorm: quantile of the normal distribution
               qt(C + (1 - C) / 2, N) # qt: quantile of the t-student distribution
  ) * sd_t_est

  t_est <- ifelse(missing(M),
                  x_est * N * m, # unknown M
                  x_est * M) # known M

  p_upper <- round(t_est + MOE, 3)

  p_lower <- round(t_est - MOE, 3)

  inference <- paste0("The population mean is between ", p_lower, " and ", p_upper, " with ", C * 100, "% confidence.")

  if (n_real < 20) {
    inference <- paste0(inference, " Note that n_real < 20 and the estimated variance may not be unbiased. Consider increasing the sample size.")
  }

  return(list(t_est = t_est, margin_of_error = MOE, inference = inference))

}
