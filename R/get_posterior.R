
#' Compute Posterior Distribution
#'
#' @description Computes the posterior distribution given the supplied data and Beta distirbution
#' priors.
#'
#' @param successes Number of success from binomial process
#' @param trials Number of trials
#' @param alpha_prior Alpha parameter for Beta distribution prior. Defaults to 1.
#' @param beta_prior Beta parameter for Beta distribution prior. Defaults to 1.
#' @param ci Posterior credible interval. Defaults to 95%.
#'
#' @export
#'
#' @examples
#' get_posterior( successes = 5, trials = 10 )
#' get_posterior( successes = 5, trials = 10, alpha_prior = 2, beta_prior = 2 )
get_posterior <- function( successes, trials, alpha_prior = 1, beta_prior = 1, ci = .95 )
{

  # Error handling
  if( length(successes) > 1 ) stop("ERROR: Vector inputs not allowed! Provide total number of successes.")
  if( length(trials) > 1 ) stop("ERROR: Vector inputs not allowed! Provide total number of trials.")
  if( successes > trials ) stop( "ERROR: Number of successes cannot be larger than number of trials!")

  # For plotting
  #theta_seq     <- ppoints( n = 1001 )
  #prior_density <- dbeta( x = theta_seq, shape1 = alpha_prior, shape2 = beta_prior )

  alpha_post <- alpha_prior + successes - 1
  beta_post  <- beta_prior + trials - successes - 1

  #posterior_density <- dbeta( x = theta_seq, shape1 = alpha_post, shape2 = beta_post )
  posterior_hdi     <- compute_credible_interval( alpha = alpha_post, beta = beta_post, ci = ci )
  posterior_stats   <- compute_posterior_stats( alpha = alpha_post, beta = beta_post )

  # Outputs
  output <- list()
  #output$data <- y
  #output$n_obs <- n
  output$prior <- paste0("Beta(", alpha_prior, ", ", beta_prior, ")")
  #output$prior_density <- data.frame( lambda = lambda_seq, density = prior )
  output$posterior <- paste0("Beta(", alpha_post, ", ", beta_post, ")")
  output$posterior_pars <- c( alpha = alpha_post, beta = beta_post )
  #output$posterior_density <- data.frame( lambda = lambda_seq, density = posterior )
  #output$posterior_pred <- data.frame( k = k_seq, density = posterior_pred )
  output$posterior_summary <- posterior_stats
  output$posterior_hdi     <- posterior_hdi

  return( output )
}




