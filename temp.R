
library(survHE)

data(bc)

# Fits the same model using the 3 inference methods
mle <- fit.models(formula = Surv(recyrs,censrec) ~ 1,
                  data = bc,
                  distr = "exp",
                  method = "mle")
mle

# includes median
summary(mle)


# Kaplan-Meier
min(mle$misc$km$time[mle$misc$km$surv < 0.5])


## mean S

xx <- make.surv(mle)

purrr::map(xx$S, ~ min(.x$t[.x$S < 0.5]))

rate <- mle$models$Exponential$coefficients

-log(0.5)/exp(rate)


# multiple simulations

xx <- make.surv(mle, nsim = 10)

purrr::map(xx$S, ~ min(.x$t[.x$S < 0.5]))
purrr::map(xx$S, ~ min(.x$t[.x$low < 0.5]))
purrr::map(xx$S, ~ min(.x$t[.x$upp < 0.5]))


##TODO: where are the stan data for rate?

-log(0.5)/exp(rate)



# multiple groups -----------------------


# Fits the same model using the 3 inference methods
mle <- fit.models(formula = Surv(recyrs,censrec) ~ group,
                  data = bc,
                  distr = "exp",
                  method = "mle")
mle

xx <- make.surv(mle)

purrr::map(xx$S, ~ min(.x$t[.x$S < 0.5]))

rate <- mle$models$Exponential$coefficients

-log(0.5)/rate


## plots

