#' A function factory
#' Function to return a function that performs likelihood ratio test.
#' binomial and negative binomial special case.
create_test_function_one_sample_case_two <- function(calc_MLE, calc_test_stat, arg1, arg2) {
  arg1 <- rlang::ensym(arg1)
  arg2 <- rlang::ensym(arg2)

  force(calc_MLE)
  # Confirm function looks right
  if (!inherits(calc_MLE, "function")) {
    stop("Argument calc_MLE must be a function.")
  }
  args <- names(formals((calc_MLE)))
  if (args[1] != "arg1") {
    stop("calc_MLE's first argument is not arg1.")
  }
  if (args[2] != "arg2") {
    stop("calc_MLE's second argument is not arg2.")
  }
  if (length(args) != 2) {
    stop("calc_MLE has too many arguments.")
  }
  rm(args)

  force(calc_test_stat)
  # Confirm function looks right
  if (!inherits(calc_test_stat, "function")) {
    stop("Argument calc_test_stat must be a function.")
  }
  args <- names(formals(calc_test_stat))
  if (args[1] != "arg1") {
    stop("calc_test_stat's first argument is not arg1.")
  }
  if (args[2] != "arg2") {
    stop("calc_test_stat's second argument is not arg2.")
  }
  if (args[3] != "p") {
    stop("calc_test_stat's third argument is not p.")
  }
  if (args[4] != "alternative") {
    stop("calc_test_stat's fourth argument is not alternative.")
  }
  if (length(args) != 4) {
    stop("calc_test_stat has too many arguments.")
  }
  rm(args)

  LB <- 0
  UB <- 1

  if (rlang::as_string(arg2) == "n") {
    # binomial case
    sizeCheck <- rlang::expr(
      if (!!arg2 < 25) {
        stop("At least 25 trials should be done for likelihood ratio test.")
      }
    )
    rangeCheck <- rlang::expr(
      if (!!arg1 > !!arg2) {
        stop("Argument x cannot be larger than n.")
      }
    )
  } else if (rlang::as_string(arg2) == "num_successes") {
    # negative binomial case
    sizeCheck <- rlang::expr(
      if (sum(!!arg1 + !!arg2) < 1) {
        stop("num_failures plus num_successes should be at least 1 for likelihood ratio test.")
      }
    )
    rangeCheck <- rlang::expr(
      if (!!arg2 < 1) {
        stop("There must be at least one success.")
      }
    )
  } else {
    stop("Arg2 is not n or num_successes.")
  }


  calc_CI <- function(arg1, arg2, alternative, conf.level) {
    alpha <- 1 - conf.level
    ops_p <- calc_MLE(arg1, arg2)

    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(arg1, arg2, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      searchLB <- LB + 10 * .Machine$double.eps
      searchUB <- UB - 10 * .Machine$double.eps
      side_one <- helper(searchLB)
      side_two <- helper(searchUB)

      if (sign(side_one) != sign(side_two)) {
        out <- stats::uniroot(helper, lower = searchLB, upper = searchUB, tol = .Machine$double.eps^.50)$root
      } else {
        out <- NA_real_
      }
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(arg1, arg2, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      searchLB <- LB + 10 * .Machine$double.eps
      searchUB <- UB - 10 * .Machine$double.eps
      side_one <- helper(searchLB)
      side_two <- helper(searchUB)

      if (sign(side_one) != sign(side_two)) {
        out <- stats::uniroot(helper, lower = searchLB, upper = searchUB, tol = .Machine$double.eps^.50)$root
      } else {
        out <- NA_real_
      }
      return(out)
    }

    if (alternative == "two.sided") {
      alpha <- alpha / 2
      # deal with edge case of MLE on boundary
      if (ops_p == 1) {
        CI <- c(calc_left_side_CI(alpha), UB)
      } else if (ops_p == 0) {
        CI <- c(LB, calc_right_side_CI(alpha))
      } else {
        CI <- c(calc_left_side_CI(alpha), calc_right_side_CI(alpha))
      }
    } else if (alternative == "less") {
      if (ops_p == 1) {
        CI <- c(calc_left_side_CI(alpha), UB)
      } else {
        CI <- c(LB, calc_right_side_CI(alpha))
      }
    } else {
      if (ops_p == 0) {
        CI <- c(LB, calc_right_side_CI(alpha))
      } else {
        CI <- c(calc_left_side_CI(alpha), UB)
      }
    }

    return(CI)
  }

  # Build function
  args <- rlang::pairlist2(holder1 = , holder2 = , p = , alternative = "two.sided", conf.level = 0.95)
  names(args)[1] <- rlang::as_string(arg1)
  names(args)[2] <- rlang::as_string(arg2)

  body <- rlang::expr({
    if (length(!!arg1) != 1) {
      stop("First argument should have length 1.")
    }
    if (!is.numeric(!!arg1)) {
      stop("First argument should be numeric.")
    }
    if (!!arg1 != as.integer(!!arg1)) {
      stop("First argument should be an integer.")
    }
    if (!!arg1 < 0) {
      stop("First argument should be 0 or above.")
    }
    if (length(!!arg2) != 1) {
      stop("Second argument should have length 1.")
    }
    if (!is.numeric(!!arg2)) {
      stop("Second argument should be numeric.")
    }
    if (!!arg2 != as.integer(!!arg2)) {
      stop("Second argument should be an integer.")
    }
    if (!!arg2 < 0) {
      stop("Second argument should be 0 or above.")
    }
    !!sizeCheck
    !!rangeCheck

    if (!is.numeric(p)) {
      stop("Argument p should be numeric.")
    }
    if (length(p) != 1) {
      stop("Argument p should have length one.")
    }
    if (p < 0 || p > 1) {
      stop("Argument p should be between 0 and 1.")
    }
    if (length(alternative) != 1) {
      stop("Argument alternative should have length one.")
    }
    if (!is.character(alternative)) {
      stop("Argument alternative should be a character.")
    }
    if (!(alternative %in% c("two.sided", "less", "greater"))) {
      stop("Argument alternative should be 'two.sided', 'less', or 'greater.'")
    }
    if (length(conf.level) != 1) {
      stop("conf.level should have length one.")
    }
    if (!is.numeric(conf.level)) {
      stop("conf.level should be numeric.")
    }
    if (conf.level <= 0 || conf.level >= 1) {
      stop("conf.level should between zero and one.")
    }

    W <- calc_test_stat(!!arg1, !!arg2, p, alternative)

    # calculate p value
    if (alternative == "two.sided") {
      p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
    } else if (alternative == "less") {
      p.value <- stats::pnorm(q = W, lower.tail = TRUE)
    } else {
      p.value <- stats::pnorm(q = W, lower.tail = FALSE)
    }

    CI <- calc_CI(!!arg1, !!arg2, alternative, conf.level)

    out <- list(statistic = W, p.value = p.value, conf.int = CI, conf.level = conf.level, alternative = alternative)
    class(out) <- c("one_sample_case_two", "lrtest")
    return(out)
  })

  exec_globals <- list(LB = LB, UB = UB, calc_MLE = calc_MLE, calc_test_stat = calc_test_stat, calc_CI = calc_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}

create_test_function_one_way_case_two <- function(calc_test_stat, calc_individual_CI) {
  force(calc_test_stat)
  force(calc_individual_CI)
  # Confirm function looks right
  if (!inherits(calc_test_stat, "function")) {
    stop("Argument calc_test_stat must be a function.")
  }
  # Confirm function looks right
  if (!inherits(calc_individual_CI, "function")) {
    stop("Argument calc_individual_CI must be a function.")
  }

  args <- names(formals(calc_test_stat))
  args_02 <- names(formals(calc_individual_CI))
  if (args[1] != args_02[1]) {
    stop("calc_test_stat's first argument does not match calc_individual_CI first argument.")
  }
  if (args[2] != args_02[2]) {
    stop("calc_test_stat's second argument does not match calc_individual_CI second argument.")
  }
  if (args[3] != "fctr") {
    stop("calc_test_stat's third argument is not fctr.")
  }
  if (length(args) != 3) {
    stop("calc_test_stat has too many arguments.")
  }
  rm(args, args_02)

  args <- names(formals(calc_individual_CI))
  if (args[4] != "alternative") {
    stop("calc_individual_CI's fourth argument is not alternative.")
  }
  if (args[5] != "conf.level") {
    stop("calc_individual_CI's fifth argument is not conf.level.")
  }
  if (length(args) != 5) {
    stop("calc_individual_CI has too many arguments.")
  }
  rm(args)

  arg1 <- rlang::sym(names(formals(calc_individual_CI))[1])
  arg2 <- rlang::sym(names(formals(calc_individual_CI))[2])
  if (rlang::as_string(arg2) == "n") {
    # binomial case
    sizeCheck <- rlang::expr(
      if (sum(!!arg2) < 50) {
        stop("At least 50 trials should be done for likelihood ratio test.")
      }
    )
    rangeCheck <- rlang::expr(
      if (any(!!arg1 > !!arg2)) {
        stop("No values in  x can be larger than values in n.")
      }
    )
  } else if (rlang::as_string(arg2) == "num_successes") {
    # negative binomial case
    sizeCheck <- rlang::expr(
      if (sum(!!arg1 + !!arg2) < 1) {
        stop("num_failures plus num_successes should be at least 1 for likelihood ratio test.")
      }
    )

    rangeCheck <- rlang::expr(
      if (any(!!arg2 < 1)) {
        stop("There must be at least one success in num_successes per group.")
      }
    )
  } else {
    stop("arg2 was not n or num_successes.")
  }

  # Build function
  args <- rlang::pairlist2(holder1 = , holder2 = , fctr = , conf.level = 0.95)
  for (i in 1:2) {
    names(args)[i] <- names(formals(calc_individual_CI))[i]
  }

  body <- rlang::expr({
    if (length(!!arg1) != length(!!arg2)) {
      stop("The first two arguments should have the same length.")
    }
    if (length(!!arg1) < 1) {
      stop("First argument should have positive length.")
    }
    if (!is.numeric(!!arg1)) {
      stop("First argument should be numeric.")
    }
    if (any(!!arg1 != as.integer(!!arg1))) {
      stop("First argument should only contain integers.")
    }
    if (any(!!arg1 < 0)) {
      stop("All elements in first argument should be 0 or above.")
    }
    if (length(!!arg2) < 1) {
      stop("Second argument should have positive length.")
    }
    if (!is.numeric(!!arg2)) {
      stop("Second argument should be numeric.")
    }
    if (any(!!arg2 != as.integer(!!arg2))) {
      stop("Second argument should only contain integers.")
    }
    if (any(!!arg2 < 0)) {
      stop("All elements in second argument should be 0 or above.")
    }
    !!sizeCheck
    !!rangeCheck
    if (length(fctr) != length(!!arg1)) {
      stop("Argument fctr should have same length as first argument.")
    }
    if (!is.factor(fctr)) {
      stop("Argument fctr should be a factor.")
    }
    if (length(base::unique(fctr)) < 2) {
      stop("Argument fctr should have at least two unique values.")
    }
    if (length(conf.level) != 1) {
      stop("conf.level should have length one.")
    }
    if (!is.numeric(conf.level)) {
      stop("conf.level should be numeric.")
    }
    if (conf.level <= 0 || conf.level >= 1) {
      stop("conf.level should between zero and one.")
    }

    W <- calc_test_stat(!!arg1, !!arg2, fctr)

    # Under null, 1 parameter (overall value) is allowed to vary
    # Under alternative, parameter for each group is allowed to vary
    df <- length(levels(fctr)) - 1

    p.value <- stats::pchisq(q = W, df = df, lower.tail = FALSE)

    # Bonferroni correction and convert back to confidence
    alpha <- 1 - conf.level
    alpha <- alpha / length(levels(fctr))
    individual.conf.level <- 1 - alpha

    CI <- list()
    for (i in seq_along(levels(fctr))) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempOne <- !!arg1
      tempOne <- tempOne[index]
      tempTwo <- !!arg2
      tempTwo <- tempTwo[index]
      tempCI <- calc_individual_CI(tempOne, tempTwo, .5, "two.sided", individual.conf.level)
      tempCI <- tempCI$conf.int
      CI[[l]] <- tempCI
    }

    out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
    class(out) <- c("one_way_case_two", "lrtest")
    return(out)
  })

  exec_globals <- list(calc_test_stat = calc_test_stat, calc_individual_CI = calc_individual_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}
