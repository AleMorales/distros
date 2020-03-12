
# Calculate standard parameters of distributions
lognormal_mulog = function(mu, cv) {
  mulog = log(mu/sqrt(cv*cv + 1))
}

lognormal_sdlog = function(mu, cv) {
  sdlog = sqrt(log(1 + cv*cv))
}

#' Log-normal density function parameterized by mean and coefficient of variation
#'
#' @param x Random variable
#' @param mu Mean of the distribution
#' @param cv coefficient of variation of the distribution
#'
#' @return The probability density of \code{x}
#' @export
#'
#' @examples
#' curve(dlnorm2(x, mu = 0.5, cv = 1), 0, 2)
dlnorm2 =  function(x, mu, cv) {
  mulog = lognormal_mulog(mu, cv)
  sdlog = lognormal_sdlog(mu, cv)
  dlnorm(x, mulog, sdlog)
}

#' Random number generator for log-normal distribution parameterized by mean and coefficient of variation
#'
#' @param n Number of random values to be drawn from the distribution
#' @param mu Mean of the distribution
#' @param cv coefficient of variation of the distribution
#'
#' @return A vector with \code{n} random values from the distribution
#' @export
#'
#' @examples
#' rlnorm2(10, mu = 0.5, cv = 1)
rlnorm2 = function(n, mu, cv) {
  mulog = lognormal_mulog(mu, cv)
  sdlog = lognormal_sdlog(mu, cv)
  rlnorm(n, mulog, sdlog)
}

