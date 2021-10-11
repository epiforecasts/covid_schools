
library("rbi.helpers")
library("here")
library("data.table")
library("ggplot2")

alpha <- 0.8 # rate of imporation (scaled by prevalence)
beta <- 7.0  # SAR of infected pupil at school

nschools = 15

prev <- abs(rnorm(nschools,mean <- 0.03, sd=0.01))
prev_past <- abs(rnorm(nschools,mean <- 0.03, sd=0.01))
prev_past <- prev
school_size <- round(abs(rnorm(nschools,mean <- 300, sd=50)))
sample_size <- rep(40, nschools)

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

model <- bi_model('~/Documents/WORK/Analyses/covid_schools/inst/bi/model.bi')

bi <- libbi(model)


#bi <- simulate(model, 
#               nsamples = 5, 
#               end_time = 1, 
#               noutputs = 1, 
#               input=list(
#                 observations = data.frame(s = 1:100, value = observations),
#                 sample = data.frame(s = 1:100, value = sample_size),
#                 prev = data.frame(s = 1:100, value = prev),
#                 school_size = data.frame(s = 1:100,value = school_size)
#               ))
#
#
obsdata <- bi_generate_dataset(model, end_time=1, noutputs=1,
                               input = list(
                                 sample = data.frame(s = 0:(nschools-1), value = sample_size),
                                 prev = data.frame(s = 0:(nschools-1), value = prev),
                                 prev_past = data.frame(s = 0:(nschools-1), value = prev_past),
                                 school_size = data.frame(s = 0:(nschools-1),value = school_size)))
dataset <- bi_read(obsdata)


bi_prior <- sample(bi, target = "prior", nsamples = 1000, end_time = 1, noutputs = 1,
                   input = list(
                     sample = data.frame(s = 0:(nschools-1), value = sample_size),
                     prev = data.frame(s = 0:(nschools-1), value = prev),
                     prev_past = data.frame(s = 0:(nschools-1), value = prev_past),
                     school_size = data.frame(s = 0:(nschools-1),value = school_size)),
                   verbose = TRUE)

bi_prior = rbi.helpers::adapt_proposal(bi_prior, min = 0.1, max=0.3)

bi <- sample(bi_prior, target = "posterior", nparticles = 32,
             obs = list(detected = data.frame(s = 0:(nschools-1), time = rep(1,nschools),
                                              value = observations)),
             input=list(
               sample = data.frame(s = 0:(nschools-1), value = sample_size),
               prev = data.frame(s = 0:(nschools-1), value = prev),
               prev_past = data.frame(s = 0:(nschools-1), value = prev_past),
               school_size = data.frame(s = 0:(nschools-1),value = school_size)))


posterior <- bi_read(bi)
summary(bi)

library('coda')
traces <- mcmc(get_traces(bi))
plot(traces)

obs <- list(
  detected = data.frame(s = 0:(nschools - 1),
                        time = rep(1, nschools),
                        value = observations[1:nschools]))

library('magrittr')
posterior <- sample(proposal="prior", model, nsamples=1000, end_time=1, nparticles=16, input=list(
  sample = data.frame(s = 0:(nschools-1), value = sample_size),
  prev = data.frame(s = 0:(nschools-1), value = prev),
  prev_past = data.frame(s = 0:(nschools-1), value = prev_past),
  school_size = data.frame(s = 0:(nschools-1),value = school_size)),
                    obs = obsdata, seed=1234) %>%
  adapt_particles %>%
  adapt_proposal(min=0.05, max=0.4) %>%
  sample(nsamples=5000)
pred <- posterior %>%
  predict(noutputs=16, with=c("transform-obs-to-state"))


traces <- mcmc(get_traces(posterior))
plot(traces)

library(tidyverse)

params <- get_traces(posterior)
lp <- params %>%
  mutate(id = 1:n()) %>%
  pivot_longer(-id, names_to = "param")

true_values <- tibble(param = c("alpha", "beta"),
                      value = c(dataset$alpha[[1]], dataset$beta[[1]]))

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
  theme_minimal() +
  geom_hline(yintercept = dataset$beta[[1]]) +
  geom_vline(xintercept = dataset$alpha[[1]])

p
