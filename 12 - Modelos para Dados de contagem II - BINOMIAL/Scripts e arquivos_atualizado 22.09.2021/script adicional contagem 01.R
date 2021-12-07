pois <- rpois(1000, lambda = 2)
hist(pois, breaks = 10)

nbinom <- rnbinom(1000, size = 2, mu = 2)
hist(nbinom, breaks = 10)

###########################################

by(data = corruption$staff, INDICES = corruption$post, FUN = mean)


###########################################

modelo_poisson0 <- glm(formula = violations ~ 1,
                       data = corruption,
                       family = "poisson")
summ(modelo_poisson0)
logLik(modelo_poisson0)

summ(modelo_poisson)
logLik(modelo_poisson)


chi2 <- -2*(logLik(modelo_poisson0)-logLik(modelo_poisson))
chi2

delta_AIC <- 6899.30 - 4151.59
delta_AIC

chi2 - 2*(3)

?step
