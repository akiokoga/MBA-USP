#Cálculo manual do fit do modelo binomial negativo:
1.9469 + 0.0400 * 23 - 4.2746 * 1 + 0.4527 * 0.5
exp(-1.18135)

#Cálculo manual do fit do modelo ZIP:
(1 - (1/(1+exp(-(-1.612 - 0.952*0.5)))))*(exp(2.489 + 0.020*23 -4.288*0 + 0.094*0.5))

###################################################

#OLS:
modelo_lm <- lm(formula = violations ~ staff + post + corruption,
                data = corruption)
summary(modelo_lm)
logLik(modelo_lm)
library(nortest)
sf.test(modelo_lm$residuals)
lrtest(modelo_zinb,modelo_lm)

#OLS com Box-Cox:
library(car)
lambda_BC <- powerTransform(corruption$violations)
summary(corruption$violations)

corruption$violations1 <- corruption$violations + 0.001
lambda_BC <- powerTransform(corruption$violations1)
lambda_BC

corruption$bc_violations <- (((corruption$violations1 ^
                                 lambda_BC$lambda) - 1) / lambda_BC$lambda)


modelo_bc <- lm(formula = bc_violations ~ staff + post + corruption,
                data = corruption)
summary(modelo_bc)
logLik(modelo_bc)
sf.test(modelo_bc$residuals)
lrtest(modelo_zinb, modelo_bc)


#Gráfico para comparação de LLs

my_plot3 <-
  data.frame(Poisson = logLik(modelo_poisson),
             ZIP = logLik(modelo_zip),
             Bneg = logLik(modelo_bneg),
             ZINB = logLik(modelo_zinb),
             OLS = logLik(modelo_lm),
             OLS_BC = logLik(modelo_bc)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)),
            color = "black",
            size = 3.5,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF",
                                           "orange", "#FDE725FF",
                                           "grey20", "grey40")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot3





