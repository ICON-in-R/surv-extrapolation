
library(survHE)

data(bc)

# Fits the same model using the 3 inference methods
mle <- fit.models(formula = Surv(recyrs, censrec) ~ 1,
                  data = bc,
                  distr = "exp",
                  method = "mle")

# the print and summary methods for a survHE object both return medians
mle

summary(mle)


# Kaplan-Meier
min(mle$misc$km$time[mle$misc$km$surv < 0.5])


## mean S

xx <- make.surv(mle)

purrr::map_dbl(xx$S, ~ min(.x$t[.x$S < 0.5]))

rate <- mle$models$Exponential$coefficients

# closed form
meantime <- -log(0.5)/exp(rate)


# multiple simulations

xx <- make.surv(mle, nsim = 100)

purrr::map_dbl(xx$S, ~ min(.x$t[.x$S < 0.5]))
purrr::map_dbl(xx$S, ~ min(.x$t[.x$low < 0.5]))
purrr::map_dbl(xx$S, ~ min(.x$t[.x$upp < 0.5]))


rtimes <- purrr::map_dbl(unlist(xx$sim), ~ -log(0.5)/.x)


plot(mle) + 
  geom_vline(xintercept = rtimes, alpha = 0.1, col = "darkgrey", size = 3) +
  geom_vline(xintercept = meantime)


# multiple groups -----------------------

# Fits the same model using the 3 inference methods
mle <- fit.models(formula = Surv(recyrs,censrec) ~ group,
                  data = bc,
                  distr = "exp",
                  method = "mle")
mle

xx <- make.surv(mle)

purrr::map_dbl(xx$S, ~ min(.x$t[.x$S < 0.5]))

rate <- mle$models$Exponential$coefficients

meantime <- -log(0.5)/rate


## plots

