
# Calculate standard parameters of distributions
beta_shape1 = function(mu, tau, cv) {
  if(!is.null(tau)) {
    shape1 = mu*tau
  } else if(!is.null(cv)) {
    shape1 = (1 - mu - cv*cv*mu)/(cv*cv)
  }
}

beta_shape2 = function(mu, tau, cv) {
  if(!is.null(tau)) {
    shape2 = (1 - mu)*tau
  } else if(!is.null(cv)) {
    shape2 = (1 - mu - cv*cv*mu)*(1 - mu)/(mu*cv*cv)
  }
}


#' Beta density function parameterized by mean and precision parameter
#'
#' @param x Random variable
#' @param mu Mean of the distribnution
#' @param tau Precision parameter of the distribution
#' @param cv Coefficient of variation of the distribution
#'
#' @return The probability density of \code{x}
#' @export
#'
#' @examples
#' curve(dbeta2(x, mu = 0.5, tau = 3), 0, 1)
#' curve(dbeta2(x, mu = 0.5, cv = 0.5), 0, 1)
dbeta2 = function(x, mu, tau = NULL, cv = NULL) {
  shape1 = beta_shape1(mu, tau, cv)
  shape2 = beta_shape2(mu, tau, cv)
  dbeta(x, shape1, shape2)
}

#' Random number generator for beta distribution parameterized by mean and precision parameter
#'
#' @param n Number of random values to be drawn from the distribution
#' @param mu Mean of the distribution
#' @param tau Precision parameter of the distribution
#' @param cv Coefficient of variation of the distribution
#'
#' @return A vector with \code{n} random values from the distribution
#' @export
#'
#' @examples
#' rbeta2(10, mu = 0.5, tau = 3)
#' rbeta2(10, mu = 0.5, cv = 0.5)
rbeta2 = function(n, mu, tau) {
  shape1 = beta_shape1(mu, tau, cv)
  shape2 = beta_shape2(mu, tau, cv)
  rbeta(n, shape1, shape2)
}
