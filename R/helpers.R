compute_credible_interval <- function( alpha, beta, ci = .95 ){

  hdi <- c( ci_lower = qbeta(p = (1 - ci) / 2, shape1 = alpha, shape2 = beta),
            ci_upper = qbeta(p = 1 - ((1 - ci) / 2), shape1 = alpha, shape2 = beta ) )

  round( hdi, 3 )
}

compute_posterior_stats <- function( alpha, beta )
{

  posterior_mode   <- (alpha - 1) / (alpha + beta - 2)
  posterior_median <- qbeta( p = .50, shape1 = alpha, shape2 = beta )
  posterior_mean   <- alpha / (alpha + beta)
  posterior_var    <- (alpha * beta) / (alpha + beta) ^ 2 * (alpha + beta + 1)
  posterior_sd     <- sqrt( posterior_var )

  round( c( mode     = posterior_mode,
            median   = posterior_median,
            mean     = posterior_mean,
            variance = posterior_var,
            std_dev  = posterior_sd ), 3)
}

bayes_summary <- function( fit )
{

  cat( paste0(
    "--------------------------------------------\n",
    "Poisson-Gamma Model\n--------------------------------------------\n",
    "Data",
    "\nObs: ", fit$n_obs,
    "\nSum: ", sum( fit$data ),
    "\nMLE: ", mean( fit$data ),
    "\n\nDistributions",
    "\nPrior:\t   ", fit$prior,
    "\nPosterior: ", fit$posterior,
    "\n\nPosterior Parameters",
    "\nMode: \t", round( fit$posterior_summary["mode"], 3 ),
    "\nMedian: ", round( fit$posterior_summary["median"], 3 ),
    "\nMean: \t", round( fit$posterior_summary["mean"], 3 ),
    "\nCI: \t", "[", round( fit$posterior_ci[1], 3 ), " ", round( fit$posterior_ci[2], 3 ), "]"

  ))

}



