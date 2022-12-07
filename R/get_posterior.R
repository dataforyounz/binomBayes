
#' Compute Posterior Distribution
#'
#' @description Computes the posterior distribution given the supplied data and Beta distirbution
#' priors.
#'
#' @param successes number of success from binomial process
#' @param trials number of trials
#' @param alpha_prior alpha parameter for Beta distribution prior. Defaults to 1.
#' @param beta_prior beta parameter for Beta distribution prior. Defaults to 1.
#'
#' @export
#'
#' @examples
#' get_posterior( successes = 5, trials = 10 )
#' get_posterior( successes = 5, trials = 10, alpha_prior = 2, beta_prior = 2)
get_posterior <- function( successes, trials, alpha_prior = 1, beta_prior = 1 )
{

  if( successes > trials ) stop( "ERROR: Number of successes cannot be larger than number of trials!")

  # for plotting
  theta_seq     <- ppoints( n = 1001 )
  prior_density <- dbeta( x = theta_seq, shape1 = alpha_prior, shape2 = beta_prior )

  alpha_post <- alpha + successes - 1
  beta_post  <- alpha + trials - successes - 1

}
