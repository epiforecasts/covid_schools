library("rbi")
library("rbi.helpers")
library("here")
library("ggplot2")

model <- bi_model(here::here("inst", "bi", "model.bi"))

bi <- simulate(model, nsamples = 5, end_time = 20, noutputs = 20)

detected <- bi_read(bi)$detected

ggplot(detected, aes(x = time, y = value, colour = np, group = np)) +
  theme_minimal() +
  geom_line()
