##  credit: https://www.alexejgossmann.com/benchmarking_r/
sleep_for_a_minute <- function() { Sys.sleep(60) }

start_time <- Sys.time()
sleep_for_a_minute()
end_time <- Sys.time()

end_time - start_time
# Time difference of 1.000327 mins
system.time({ sleep_for_a_minute() })



# devtools::install_github("jabiru/tictoc")
# not working


devtools::install_github("olafmersmann/microbenchmarkCore")
devtools::install_github("olafmersmann/microbenchmark")

library(microbenchmark)

set.seed(2017)
n <- 10000
p <- 100
X <- matrix(rnorm(n*p), n, p)
y <- X %*% rnorm(p) + rnorm(100)

check_for_equal_coefs <- function(values) {
  tol <- 1e-12
  max_error <- max(c(abs(values[[1]] - values[[2]]),
                     abs(values[[2]] - values[[3]]),
                     abs(values[[1]] - values[[3]])))
  max_error < tol
}

mbm <- microbenchmark("lm" = { b <- lm(y ~ X + 0)$coef },
                      "pseudoinverse" = {
                        b <- solve(t(X) %*% X) %*% t(X) %*% y
                      },
                      "linear system" = {
                        b <- solve(t(X) %*% X, t(X) %*% y)
                      },
                      check = check_for_equal_coefs)

mbm

library(ggplot2)
autoplot(mbm)
