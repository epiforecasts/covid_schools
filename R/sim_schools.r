library("rbi")
library("rbi.helpers")
library("here")
library("data.table")
library("ggplot2")


alpha = 0.8                                                     # rate of imporation (scaled by prevalence)
beta = 7.0                                                   # SAR of infected pupil at school


prev = abs(rnorm(100,mean = 0.03, sd=0.01))
prev_past = abs(rnorm(100,mean = 0.007, sd=0.01))
school_size = round(abs(rnorm(100,mean = 300, sd=50)))
sample_size = rep(40, 100)

case_func = function(alpha, beta, prev, prev_past, school_size, sample){
  lambda_import = alpha*prev 
  lambda_import_past = alpha*prev_past 
  imports = mapply(FUN = function(lambda_import, school_size){rpois(1, lambda = lambda_import * school_size)}, lambda_import=lambda_import, school_size=school_size)        # simulated number of imports
  imports_past = mapply(FUN = function(lambda_import, school_size){rpois(1, lambda = lambda_import * school_size)}, lambda_import=lambda_import_past, school_size=school_size)        # simulated number of imports
  
  cases = imports #+ sapply(1:length(prev), function(X){sum(rpois(imports_past[X], beta))})
  cases
}

cases = case_func(alpha, beta, prev, prev_past, school_size, sample)
rates = cases/school_size

observations = sapply(X = 1:length(rates), function(X){rbinom(n=1, size=sample_size[X], prob=rates[X])})




model <- bi_model(here::here("inst", "bi", "model.bi"))

bi <- libbi(model)


bi <- simulate(model, 
               nsamples = 5, 
               end_time = 1, 
               noutputs = 1, 
               input=list(
                 observations = data.frame(s = 1:100, value = observations),
                 sample = data.frame(s = 1:100, value = sample_size),
                 prev = data.frame(s = 1:100, value = prev),
                 school_size = data.frame(s = 1:100,value = school_size)
               ))


obsdata <- bi_generate_dataset(model, end_time=1, noutputs=2)

bi_prior <- sample(bi, target="prior", nsamples=100, end_time=1, noutputs=1,
                   input=list(
                     sample = data.frame(s = 1:100, value = sample_size),
                     prev = data.frame(s = 1:100, value = prev),
                     prev_past = data.frame(s = 1:100, value = prev_past),
                     school_size = data.frame(s = 1:100,value = school_size)))

bi <- sample(bi_prior, target="posterior", nparticles=32, obs=list(detected=data.frame(s = 1:100, time=rep(1,100),value=observations)),
             input=list(
               sample = data.frame(s = 1:100, value = sample_size),
               prev = data.frame(s = 1:100, value = prev),
               prev_past = data.frame(s = 1:100, value = prev_past),
               school_size = data.frame(s = 1:100,value = school_size)))


posterior <- bi_read(bi)
summary(bi)

library('coda')
traces <- mcmc(get_traces(bi))
plot(traces)
obs_bi <- sample_obs(bi)


detected <- bi_read(bi)$detected

ggplot(data.table(detected)[time==1], aes(x = np, y = value, colour = np, group = np)) +
  theme_minimal() +
  geom_point()

model

