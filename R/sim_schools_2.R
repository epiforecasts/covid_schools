
library("rbi.helpers")
library("here")
library("data.table")
library("ggplot2")
library("tidyverse")
library("magrittr")
library("coda")
library("patchwork")

nschools = 100

prev <- abs(rnorm(nschools,mean <- 0.03, sd=0.01))
prev_past <- abs(rnorm(nschools,mean <- 0.003, sd=0.001))
prev_past <- prev
school_size <- round(abs(rnorm(nschools,mean <- 300, sd=50)))
sample_size <- rep(40, nschools)


model <- bi_model(here::here("inst", "bi", "model_noalpha.bi"))

obsdata <- bi_generate_dataset(model %>% fix(beta = 7.0), end_time=1, noutputs=1,
                               input = list(
                                 sample = data.frame(s = 0:(nschools-1), value = sample_size),
                                 prev = data.frame(s = 0:(nschools-1), value = prev),
                                 prev_past = data.frame(s = 0:(nschools-1), value = prev_past),
                                 school_size = data.frame(s = 0:(nschools-1),value = school_size)))
dataset <- bi_read(obsdata)


posterior <- sample(proposal="prior", model, nsamples=1000, end_time=1, nparticles=16, input=list(
  sample = data.frame(s = 0:(nschools-1), value = sample_size),
  prev = data.frame(s = 0:(nschools-1), value = prev),
  prev_past = data.frame(s = 0:(nschools-1), value = prev_past),
  school_size = data.frame(s = 0:(nschools-1),value = school_size)),
  obs = dataset, seed=1404) %>%
  #adapt_particles(max = 2**20) %>%
  #adapt_proposal(min=0.05, max=0.4) %>%
  sample(nsamples=2000)

#pred <- posterior %>%
#  predict(noutputs=16, with=c("transform-obs-to-state"))


params <- get_traces(posterior)

lp <- params %>%
  mutate(id = 1:n()) %>%
  pivot_longer(-id, names_to = "param")

dataset$alpha[[1]] = 0.8

dataset$beta[[1]] = 7.

true_values <- tibble(param = c("alpha", "beta"),
                      value = c(dataset$alpha[[1]], dataset$beta[[1]]))

p1 <- ggplot(lp, aes(x = value)) +
  geom_histogram(bins = 20) +
  geom_vline(data = true_values, aes(xintercept = value)) +
  facet_wrap(~ param) +
  theme_minimal() +
  xlab("Value") +
  ylab("Count") + 
  xlim(c(0,15))



## pairs plot
p2 <- ggplot(params, aes(x = alpha, y = beta)) +
  geom_jitter() +
  theme_minimal() +
  geom_hline(yintercept = dataset$beta[[1]]) +
  geom_vline(xintercept = dataset$alpha[[1]])+
  xlim(c(0,3))+
  ylim(c(0,15))

p1/p2


traces <- mcmc(get_traces(posterior))
plot(traces)

