modelo_nulo <- glm(formula = atrasado ~ 1,
                   data = Atrasado,
                   family = "binomial")
logLik(modelo_nulo)

chi2 <- -2*(logLik(modelo_nulo)-logLik(modelo_atrasos))
chi2

pchisq(chi2, df = 2, lower.tail = F)

#Akaike Info Criterion
AIC <- -2*(logLik(modelo_atrasos)) + 2*3
AIC

#Bayesian Info Criterion
BIC <- -2*(logLik(modelo_atrasos)) + 3*log(100)
BIC

pseudoR2MF <- (-2 * logLik(modelo_nulo) - (-2*logLik(modelo_atrasos)))/(-2*logLik(modelo_nulo))
pseudoR2MF

#####################

modelo_atrasos_errado <- lm(formula = atrasado ~ dist + sem,
                            data = Atrasado)
export_summs(modelo_atrasos,modelo_atrasos_errado,scale = F)

Atrasado$atrasado <- as.factor(Atrasado$atrasado)

