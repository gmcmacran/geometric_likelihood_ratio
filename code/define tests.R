source("code/factories.R")

calc_MLE_negative_binomial_p <- function(arg1, arg2) {
  ops_p <- arg2 / (arg2 + arg1)
  return(ops_p)
}

calc_test_stat_negative_binomial_p <- function(arg1, arg2, p, alternative) {
  obs_p <- calc_MLE_negative_binomial_p(arg1, arg2)
  W <- 2 * (sum(stats::dnbinom(x = arg1, size = arg2, prob = obs_p, log = TRUE)) -
    sum(stats::dnbinom(x = arg1, size = arg2, prob = p, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_p - p) * (W^.5)
  }

  return(W)
}

negative_binomial_p_one_sample <- create_test_function_one_sample_case_two(calc_MLE_negative_binomial_p, calc_test_stat_negative_binomial_p, num_failures, num_successes)

geometric_p_one_sample <- function(num_failures, p, alternative, conf.level = .95) {
  force(negative_binomial_p_one_sample)
  negative_binomial_p_one_sample(num_failures = num_failures, num_successes = 1, p = p, alternative = alternative, conf.level = conf.level)
}

calc_test_stat_negative_binomial_p_one_way <- function(num_failures, num_successes, fctr) {
  # Null
  obs_p <- sum(num_successes) / (sum(num_successes) + sum(num_failures))

  W1 <- sum(stats::dnbinom(x = num_failures, size = num_successes, prob = obs_p, log = TRUE))

  # alt
  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempFailure <- num_failures[index]
    tempSuccess <- num_successes[index]
    tempP <- tempSuccess / (tempSuccess + tempFailure)
    likelihoods[i] <- sum(stats::dnbinom(x = tempFailure, size = tempSuccess, prob = tempP, log = TRUE))
  }

  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

negative_binomial_p_one_way <- create_test_function_one_way_case_two(calc_test_stat_negative_binomial_p_one_way, negative_binomial_p_one_sample)

geometric_p_one_way <- function(num_failures, fctr, conf.level = .95) {
  force(negative_binomial_p_one_way)
  negative_binomial_p_one_way(num_failures = num_failures, num_successes = rep(1, length(num_failures)), fctr, conf.level)
}
