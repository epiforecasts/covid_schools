library("rbi")
library("rbi.helpers")
library("here")
library("data.table")
library("ggplot2")
library("dplyr")
library("tidyr")

alpha <- 0.8 # rate of imporation (scaled by prevalence)
beta <- 7.0  # SAR of infected pupil at school

prev <- abs(rnorm(100,mean <- 0.03, sd=0.01))
prev_past <- abs(rnorm(100,mean <- 0.007, sd=0.01))
school_size <- round(abs(rnorm(100,mean <- 300, sd=50)))
sample_size <- rep(40, 100)

case_func <- function(alpha, beta, prev, prev_past, school_size, sample) {
  lambda_import <- alpha*prev
  lambda_import_past <- alpha*prev_past
  imports <- mapply(FUN = function(lambda_import, school_size) {
    rpois(1, lambda = lambda_import * school_size)
  }, lambda_import = lambda_import, school_size = school_size)        # simulated number of imports
  imports_past <- mapply(FUN = function(lambda_import, school_size) {
    rpois(1, lambda = lambda_import * school_size)
  }, lambda_import = lambda_import_past, school_size = school_size)        # simulated number of imports
  cases <- imports + sapply(1:length(prev), function(X) {
    sum(rpois(imports_past[X], beta))
  })
  cases
}

cases <- case_func(alpha, beta, prev, prev_past, school_size, sample)
rates <- cases/school_size

observations <- sapply(X = 1:length(rates), function(X) {
  rbinom(n = 1, size = sample_size[X], prob = rates[X])
})

model <- bi_model(here::here("inst", "bi", "model.bi"))

bi <- libbi(model)

n_schools <- 100

input <- list(
  sample = data.frame(s = 0:(n_schools - 1), value = sample_size[1:n_schools]),
  prev = data.frame(s = 0:(n_schools - 1), value = prev[1:n_schools]),
  prev_past = data.frame(s = 0:(n_schools - 1), value = prev_past[1:n_schools]),
  school_size = data.frame(s = 0:(n_schools - 1),value = school_size[1:n_schools])
)

obs <- list(
  detected = data.frame(s = 0:(n_schools - 1),
                        time = rep(1, n_schools),
                        value = observations[1:n_schools])
)

bi <-
  sample(bi, proposal = "prior", nsamples = 100, end_time = 1, noutputs = 1,
         input = input, obs = obs, verbose = TRUE) %>%
  adapt_proposal(min = 0.1, max = 0.3) %>%
  sample(nsamples = 10000, verbose = TRUE)

summary(bi)

params <- get_traces(bi)
lp <- params %>%
  mutate(id = 1:n()) %>%
  pivot_longer(-id, names_to = "param")

true_values <- tibble(param = c("alpha", "beta"),
                      value = c(alpha, beta))

p <- ggplot(lp, aes(x = value)) +
  geom_histogram(bins = 20) +
  geom_vline(data = true_values, aes(xintercept = value)) +
  facet_wrap(~ param, scales = "free_x") +
  theme_minimal() +
  xlab("Value") +
  ylab("Count")

p

## pairs plot
p <- ggplot(params, aes(x = alpha, y = beta)) +
  geom_jitter() +
  theme_minimal()

p
