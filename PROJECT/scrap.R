ntickets <- function(N, gamma, p) {
  nd <- NULL # number of tickets with discrete distribution
  nc <- NULL # number of tickets with normal approximation

  # Calculating nd using the binomial distribution
  # We want to solve N - qbinom(gamma, N, p) = 0 for n
  objective <- function(n) {
    return(N - qbinom(gamma, n, p))
  }
  nd <- round(optimise(objective, interval = c(1, N), maximum = FALSE)$minimum)

  # Calculating nc using the normal approximation
  # We want to solve N - n*(1 - pnorm(1, mean = n*p, sd = sqrt(n*p*(1-p)))) = 0 for n
  objective <- function(n) {
    return(N - n*(1 - pnorm(1, mean = n*p, sd = sqrt(n*p*(1-p)))))
  }
  nc <- round(optimise(objective, interval = c(1, N), maximum = FALSE)$minimum)

  # Plotting objective function vs n
  n_seq <- seq(1, N, by = 1)
  objective <- 1 - gamma - pnorm(1, mean = n_seq * p, sd = sqrt(n_seq * p * (1 - p)))
  plot(n_seq, objective, type = "l", xlab = "Number of tickets", ylab = "Objective function")

  # Returning a named list containing nd, nc, N, p, and gamma
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

