################################################################################
# LEMBRETES ####################################################################
################################################################################

# Padrão sazonal existe quando uma série é influenciada por fatores sazonais 
# (ex.: trimestre do ano) e representa um período fixo e conhecido. 
# Consequentemente, as séries temporais sazonais às vezes são chamadas de séries
# temporais periódicas.

# Padrão cíclico existe quando os dados exibem subidas e descidas que não são 
# de período fixo. A duração dessas flutuações é geralmente de pelo menos 2 anos.

# Se as flutuações não são de período fixo, são cíclicas; se o período for 
# imutável e associado a algum aspecto do calendário, o padrão é sazonal. 
# Em geral, a duração média dos ciclos é maior do que a duração de um padrão 
# sazonal.

################################################################################
# Exemplo - Ciclo e Sazonalidade ###############################################
################################################################################

# base: lynx: número de linces capturadas entre 1821 e 1934 no Canadá
# base: hsales: vendas mensais de novas casas nos EUA entre 1973-1995
# base: taylor: demanda de eletricidade por meia hora entre 06.2000 e 08.2000

# Ciclos populacionais aperiódicos de, aproximadamente, 10 anos
# Não são fixos e duram entre 8 ou 9 anos e outros 10 anos
autoplot(lynx) + xlab("Year") + ylab("Number of lynx trapped")

# Forte sazonalidade dentro de cada ano, e comportamento cíclico
# com um período entre 6 e 10 anos
autoplot(hsales) + xlab("Year") + ylab("Monthly housing sales (millions)")

#Sazonalidade, padrão diário e padrão semanal
autoplot(taylor) + xlab("Week") + ylab("Electricity demand (GW)")

################################################################################
# Exemplo - Estacionariedade ###################################################
################################################################################

# Estacionariedade: propriedades estatísticas não mudam ao longo do tempo
# Exemplo: média, variância, autocorrelação

# Ruido: tem uma média constante, uma variância constante e não há estrutura
# de autocorrelação

# Autocorrelação (ADF) é um cálculo da correlação das observações da série temporal 
# com valores da mesma série, mas em tempos anteriores. Os períodos anteriores 
# são chamados de lags.

# Autocorrelação parcial (PACF) resume a relação entre uma observação em uma série 
# de tempo com observações em etapas de tempo anteriores, mas com as relações de
# observações intermediárias removidas.

# Correlograma: no contexto de séries temporais é uma diagrama de autocorrelações
# da amostra

# Teste de Dickey-Fuller: avaliação estacionariedade da série
# Expectativa que o teste seja significativo para rejeitar hipótese 

# Teste Ljung–Box: determina se algum grupo de autocorrelações de uma série 
# temporal é diferente de zero. Em outras palavras, avaliar se as séries de 
# observações ao longo do tempo é aleatória e independente.

# Expectativa que o p valor não seja significativo para não rejeitarmos a 
# hipótese nula e concluir que os resíduos são conjuntamente não correlacionados.

# Avaliando o ruído
set.seed(14)
white.noise <- ts(rnorm(1000))
plot(white.noise)

# Usar o teste ADF para avaliar se a série é estacionária
# Quanto mais negativo for o valor do Teste de Dickey-Fuller menor o p valor
adf.test(white.noise)

# Sazonalidade e tendência # 01
data(beersales) # base disponível no pacote TSA
plot(beersales) # plotando a série

adf.test(beersales) # avaliando a correlação e aparenta ser estacionária

adf.test(beersales, k=12) # série não é estacionária

# Sazonalidade e tendência # 02
data(airpass) # base disponível no pacote TSA
plot(airpass) # plotando a série

# Avalie com uma lag longo para avaliar a sazonalidade
adf.test(airpass, k=12) 

################################################################################
###### SUAVIZAÇÃO EXPONENCIAL ##################################################
################################################################################

# Criando a base de treino e teste
# base: ações do Google do pacote fpp2
fpp2::goog
goog.train <- window(fpp2::goog, 
                     end = 900)
goog.test <- window(fpp2::goog, 
                    start = 901)

# base: AirPassengers do pacote fpp2
fpp2::qcement
qcement.train <- window(fpp2::qcement, 
                        end = c(2012, 4))
qcement.test <- window(fpp2::qcement, 
                       start = c(2013, 1))

################################################################################
### SUAVIZAÇÃO EXPONENCIAL SIMPLES (SES) #######################################
################################################################################

# EXEMPLO 1 

# Aplicando o SES na base com dados do Google
ses.goog <- ses(goog.train, 
                alpha = .2,
                h = 100)
autoplot(ses.goog)

# Com o gráfico avaliamos que não está captando a tendência atual

# EXEMPLO 2

# Removendo a tendência
goog.dif <- diff(goog.train)
autoplot(goog.dif)

# Reaplicado o SES com os dados filtrados
ses.goog.dif <- ses(goog.dif,
                    alpha = .2, 
                    h = 100)
autoplot(ses.goog.dif)

# A tendência parece satisfatória. Agora precisamos comparar nossa previsão com
# nosso conjunto de dados de validação ou teste. Como os dados de treino foram 
# diferenciados, vamos criar uma validação diferenciada parao o conjunto de teste.

# Vamos criar uma validação de conjunto diferenciada e comparar com a previsão 
# dos dados. O intervalo de alpha será entre 0.01 e -0.99. A ideia será entender
# o nível de minimização do teste de RMSE. 
# Removendo a tendência e avaliando os dados de teste

# EXEMPLO 3

goog.dif.test <- diff(goog.test)
accuracy(ses.goog.dif, goog.dif.test)

# Comparando os modelos
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(goog.dif, alpha = alpha[i],
             h = 100)
  RMSE[i] <- accuracy(fit, 
                      goog.dif.test)[2,2]
}

# Convertendo os dados e identificando o valor de Alfa
alpha.fit <- data_frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, 
                    RMSE == min(RMSE))

# Plotando o RMSE vs Alpha
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min,
             aes(alpha, RMSE), 
             size = 2, color = "red")

# Agora vamos reajustar a previsão SES com alpha = 0.05

# EXEMPLO 4

# Criando treino e validando a base do Google
goog.train <- window(fpp2::goog, 
                     end = 900)
goog.test <- window(fpp2::goog, 
                    start = 901)

# Removendo a tendência
goog.dif <- diff(goog.train)

# Reajustando o alpha = .05
ses.goog.opt <- ses(goog.dif, 
                    alpha = .05,
                    h = 100)

# Avaliando a performance;performance eval
accuracy(ses.goog.opt, goog.dif.test)

# Plotando os resultados
p1 <- autoplot(ses.goog.opt) +
  theme(legend.position = "bottom")
p2 <- autoplot(goog.dif.test) +
  autolayer(ses.goog.opt, alpha = .5) +
  ggtitle("Predicted vs. actuals for 
                 the test data set")

gridExtra::grid.arrange(p1, p2, 
                        nrow = 1)

################################################################################
### HOLT'S #####################################################################
################################################################################

# EXEMPLO 1

# Aplicando o método Holt's method on
holt.goog <- holt(goog.train,
                  h = 100)
autoplot(holt.goog) # plotando

# Não fizemos a definição do valor alfa e beta. Ao mencionarmos qualquer valor
# para alfa e beta a função holt() identificará o ideal. Assim, se valor do alfa
# for de 0,9967 irá identificar um aprendizado rápido, e se o beta for 0,0001
# indicará um aprendizado lento

# EXEMPLO 2
# Vamos configurar o alfa e beta

# Método Holt's
holt.goog$model

# Acurácia do modelo
accuracy(holt.goog, goog.test)

# O valor ideal, beta = 0.0001 será usado para remover erros do conjunto de 
# treinamento. Podemos ainda ajustar o valor beta para o ideal

# EXEMPLO 3

# Vamos tentar encontrar o valor ideal de beta por meio do loop variando 
# de 0.0001 a 0,5 para minimizar o teste RMSE. Veremos que 0,0601 será o valor
# beta que diminuirá o RMSE.

# Identificado o parâmetro alpha ideal
beta <- seq(.0001, .5, by = .001)
RMSE <- NA

for(i in seq_along(beta)) {
  fit <- holt(goog.train,
              beta = beta[i], 
              h = 100)
  RMSE[i] <- accuracy(fit, 
                      goog.test)[2,2]
}

# Convertendo os dados e identificando o menor valor de alpha
beta.fit <- data_frame(beta, RMSE)
beta.min <- filter(beta.fit, 
                   RMSE == min(RMSE))

# Plotando RMSE vs. alpha
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta.min, 
             aes(beta, RMSE), 
             size = 2, color = "red")

# Agora vamos refinar o modelo para obter o melhor valor de beta

# EXEMPLO 4

holt.goog <- holt(goog.train,
                  h = 100)

# Novo modelo para otimizar o beta
holt.goog.opt <- holt(goog.train,
                      h = 100,
                      beta = 0.0601)

# Acurácia do primeiro modelo
accuracy(holt.goog, goog.test)

# Acurácia do novo modelo ideal
accuracy(holt.goog.opt, goog.test)

# Plotando
p1 <- autoplot(holt.goog) +
  ggtitle("Original Holt's Model") +
  coord_cartesian(ylim = c(400, 1000))

p2 <- autoplot(holt.goog.opt) +
  ggtitle("Optimal Holt's Model") +
  coord_cartesian(ylim = c(400, 1000))

gridExtra::grid.arrange(p1, p2, 
                        nrow = 1)

# O modelo ideal em em comparação com o modelo original é  mais conservador. 
# Além disso, o intervalo de confiança do modelo ideal é mais extremo.

################################################################################
###### HOLT-WINTERS ############################################################
################################################################################

# Método sazonal de Holt-Winter é usado para dados com padrões e tendências sazonais. 
# Pode ser implementado usando a estrutura Aditiva ou usando a estrutura Multiplicativa
# dependendo do conjunto de dados. A estrutura ou modelo aditivo é usado quando 
# o padrão sazonal de dados tem a mesma magnitude ou é consistente em toda a extensão,
# enquanto a estrutura ou modelo Multiplicativo é usado se a magnitude do padrão sazonal
# dos dados aumenta ao longo do tempo. 
# São usados três parâmetros de suavização - alfa, beta e gama.

# Vamos usar a base qcement do pacote fpp2

# Exemplo 1

# Carregando a base de dados
qcement.train <- window(fpp2::qcement, 
                        end = c(2012, 4))
qcement.test <- window(fpp2::qcement,
                       start = c(2013, 1))

# Aplicando o Holt-Winters 
autoplot(decompose(fpp2::qcement))

# Para criarmos um modelo aditivo que lida com erro, tendência e sazonalidade 
# usaremos a função ets () para escolher o melhor modelo aditivo. 

# EXEMPLO 2

# applying ets
qcement.hw <- ets(qcement.train,
                  model = "AAA")
autoplot(forecast(qcement.hw))

# Agora vamos avaliar a acurácia e resíduos

# EXEMPLO 3

qcement.hw <- ets(qcement.train, model = "AAA")

# Avaliando o modelo
summary(qcement.hw)
checkresiduals(qcement.hw)

# Previsão para os próximos 5 quadrimestres
qcement.f1 <- forecast(qcement.hw,
                       h = 5)
# Avaliando a acurácia
accuracy(qcement.f1, qcement.test)

# Agora vamos avaliar como o modelo multiplicativo funciona usando ets ().
# Para isso, o parâmetro do modelo de ets () será "MAM".

# EXEMPLO 4

# Aplicando ETS
qcement.hw2 <- ets(qcement.train,
                   model = "MAM")
checkresiduals(qcement.hw2)

# Vamos otimizar o parâmetro gama para minimizar a taxa de erro. O valor do gama
# será 0,21, junto como isso vamos identificar a precisão e plotar os valores preditivos

# EXEMPLO 5

qcement.hw <- ets(qcement.train,
                  model = "AAA")

# Previsão para os próximos 5 quadrimestres
qcement.f1 <- forecast(qcement.hw,
                       h = 5)

# Avaliando a acurácia
accuracy(qcement.f1, qcement.test)

gamma <- seq(0.01, 0.85, 0.01) # estipulando o gamma
RMSE <- NA

for(i in seq_along(gamma)) {
  hw.expo <- ets(qcement.train, 
                 "AAA", 
                 gamma = gamma[i])
  future <- forecast(hw.expo, 
                     h = 5)
  RMSE[i] = accuracy(future, 
                     qcement.test)[2,2]
}

error <- data_frame(gamma, RMSE)
minimum <- filter(error, 
                  RMSE == min(RMSE))

ggplot(error, aes(gamma, RMSE)) +
  geom_line() +
  geom_point(data = minimum, 
             color = "blue", size = 2) +
  ggtitle("gamma's impact on 
            forecast errors",
          subtitle = "gamma = 0.21 minimizes RMSE")

# Modelos aditivos anteriores
# Erro, tendência e sazonalidade
accuracy(qcement.f1, qcement.test)


# Novo modelo com parâmetro gama ótimo
qcement.hw6 <- ets(qcement.train,
                   model = "AAA", 
                   gamma = 0.21)
qcement.f6 <- forecast(qcement.hw6, 
                       h = 5)
accuracy(qcement.f6, qcement.test)

# Valores preditos
qcement.f6
autoplot(qcement.f6)

# EXEMPLO 6

# Acessando a base do pacote ggseas
nzdata<-data.table(nzbop) 
nzdata<-nzdata[!((Account=="Capital account"&
                    Category=="Balance")|
                   (Account=="Financial account"&
                      Category=="Foreign inv. in NZ; Financial derivative liabilities")|
                   (Category=="Secondary income balance")),]
sample_ts<-nzdata[Account == "Current account" & Category=="Services; Exports total",
                  .(TimePeriod, Value)]
knitr::kable(head(sample_ts)) # visualização da base

# Para avaliar se a série é multiplicativa ou aditiva é preciso separar os componentes

# Extraindo a tendência, pode ser feito calculando a média móvel ou mediana.
# Uma mediana móvel é menos sensível a outliers do que uma média móvel
sample_ts[,trend := zoo::rollmean(Value, 8, fill=NA, align = "right")]
knitr::kable(tail(sample_ts))

# Redução da Tendência
# A redução precisa ser aditiva ou multiplcativa, como ainda não sabemos 
# faremos os dois processos.
sample_ts[,`:=`( detrended_a = Value - trend,  detrended_m = Value / trend )]
knitr::kable(tail(sample_ts))

# Sazonalidade
# É preciso avaliar os valores típicos de tendência ao longo do ciclo. 
# A título de aprendizado vamos calcular o valor médio das observações 
# em Q1, Q2, Q3 e Q4.
sample_ts[,`:=`(seasonal_a = mean(detrended_a, na.rm = TRUE),
                seasonal_m = mean(detrended_m, na.rm = TRUE)), 
          by=.(quarter(TimePeriod)) ]
knitr::kable(tail(sample_ts))

# Com a tendência e a sazonalidade é possível calcular os resíduos.
sample_ts[,`:=`( residual_a = detrended_a - seasonal_a, 
                 residual_m = detrended_m / seasonal_m )]
knitr::kable(tail(sample_ts))

# Avaliando a decomposição

# Modelo Aditivo
ggsdc(sample_ts, aes(x = TimePeriod, y = Value), method = "decompose", 
      frequency = 4, s.window = 8, type = "additive")+ geom_line()+
  ggtitle("Additive")+ theme_minimal()

# Modelo Multiplicativo
ggsdc(sample_ts, aes(x=TimePeriod, y=Value), method = "decompose", 
      frequency=4, s.window=8, type = "multiplicative")+ geom_line()+
  ggtitle("Multiplicative")+ theme_minimal()

# Após a decomposição é preciso comparar os resíduos. Vamos avaliar a quantidade
# de correlação nos resíduos.

ssacf<- function(x) sum(acf(x, na.action = na.omit)$acf^2)
compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), 
                                         "Additive", "Multiplicative") 
knitr::kable(sample_ts[,.(compare_ssacf(residual_a, residual_m ))])

################################################################################
###### HOLT-WINTERS VS ARIMA ###################################################
################################################################################

# Lembrete
# Método aditivo - sazonalidade for constante (default)
# Método multiplicativo - sazonalidade for crescente

# Criando a base de dados
values = c(92.1,  92.6,  89.5,  80.9,  95.6,  72.5,  71.2,  78.8,  73.8,  83.5,  
           97.9, 93.4,  98.0,  90.2,  96.7, 100.0, 103.6,  74.6,  78.9,  92.0,  
           83.4,  98.1, 109.9, 102.2, 102.1,  96.2, 106.9,  95.1, 113.4,  84.0, 
           88.6,  94.9,  94.7, 105.7, 108.6, 101.9,  113.9, 100.9, 100.2,  91.9,
           99.6,  87.2,  92.1, 104.9, 103.4, 103.3, 103.9, 108.5)

# Formatando como Série Temporal
time_series = ts(values, start = 2015, frequency =12)

# Decomposição
autoplot(decompose(time_series)) + ggtitle("Decomposition of the series") + 
        theme(plot.title = element_text(size=8))

# Modelando: ARIMA e Holt-Winter, auto.arima()

# ARIMA
forecast_arima = auto.arima(time_series, seasonal=TRUE, stepwise = FALSE, approximation = FALSE) 
forecast_arima = forecast(forecast_arima, h=60)
plot(forecast_arima)

# Holt-Winters
forecast_hw = hw(time_series, seasonal="multiplicative", h=60)
summary(forecast_hw) # avaliando os resultados 
plot(forecast_hw) # plotando os resultados
  
# Forecasting

# ARIMA
autoplot(time_series, series=" Historical data") +
  autolayer(forecast_arima, series=" ARIMA Forecast") +
  ggtitle(" ARIMA forecasting") +
  theme(plot.title = element_text(size=8))

# Holt-Winters
autoplot(time_series, series=" Historical data") + 
  autolayer(forecast_hw, series="Holt-Winter forecast") +
  ggtitle("HW Exponential Smoothing") +
  theme(plot.title = element_text(size=8))

# Avaliação dos modelos

# Modelo de previsão
forecast_arima['model'] #ARIMA
forecast_hw['model'] #Holt Winter

# Acurácia do Modelo
accuracy(forecast_arima) #ARIMA
accuracy(forecast_hw) #Holt Winter

# Diferenças no RMSE, porém, em MAE não é significativa. Em termos de AIC, ARIMA
# parece ser um modelo melhor. Não é recomendável comparar o AIC entre o ARIMA
# e o Holt-Winter.

# PROPHET ######################################################################
# Detalhes podem ser acessados em: https://facebook.github.io/prophet/ #########
################################################################################

# Base - quantidade de visualizações no perfil do Wikipedia do Lebron James
# Fonte: https://pageviews.toolforge.org/

# Fases: 
# 1. Explorando os dados
# 2. Predições Básicas
# 3. Inspecionando Componentes do Modelo
# 4. Personalizando feriados e eventos

# 1. Explorando os dados
colnames(lebron) <- c("ds", "y")  # renomeando as colunas
head(lebron) # visualizando a base
lebron$y <- log10(lebron$y) # aplicando logaritmo base 10 na variável y
View(summary(lebron)) # explorando os dados
plot(y ~ ds, lebron, type = "l") # gráfico da série

# 2. Predições Básicas
m <- prophet(lebron)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
plot(m, forecast) # visualização simples para compreensão

# Avaliação por valores brutos com o valor previsto por dia e intervalos de incerteza
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast) # previsão por componentes

# 3. Inspecionando Componentes do Modelo
prophet_plot_components(m, forecast)

################################################################################
###### ARIMA e PROPHET #########################################################
################################################################################

data(AirPassengers) 
AirPassengers
plot(AirPassengers, ylab="Passengers", type="o", pch =20) # visualizando a série

# Separação entre treino e teste (período de dois anos)
df_train<- window(AirPassengers, end = c(1958, 12))
df_test <- window(AirPassengers, start = c(1959, 01))

# A partir do fluxo de Box e Jenkins é possível concluir que a variância não é constante, 
# porque aumenta e muda com o tempo, portanto a transformação do log é necessária. Além disso, 
# esta série temporal não é estacionária em média, considerando a sazonalidade, portanto, a 
# diferença de sazonalidade é necessária.

ggtsdisplay(diff(log(AirPassengers), 12)) # avaliação da autocorrelação

# ACF e PACF sugerem um modelo auto regressivo de ordem 2 e um modelo MA de ordem 1. 
# Assim, o modelo ARIMA (2,0,0) (0,1,1) é selecionado e é treinado com o conjunto de treinamento. 
# Dois parâmetros são definidos: include.constant e lambda. O primeiro adiciona ao modelo a interceptação. 
# O outro, em vez disso, define a transformação do log.

arima_1 <- Arima(df_train, c(2, 0, 0), c(0, 1, 1), include.constant = TRUE, lambda = 0)

ggtsdisplay(arima_1$residuals)

# Não há uma autocorrelação automática significativa entre as defasagens. O modelo 
# pode prever os últimos dois anos.

arima_f <- forecast(arima_1, 24)
forecast(arima_1, 24) %>% autoplot()

# Vamos avaliar o modelo com o RMSE, MAE e MAPE.

# RMSE - raiz do erro quadrático da média
## Usado para avaliar a medida das diferenças entre os valores (amostra ou população) previstos
## por mum modelo ou um estimador e os valores observados

# MAPE - erro absoluto do percentual da média
## Medida de precisão. Mede a precisão como uma porcentagem e pode ser calculado como o 
## erro percentual absoluto médio para cada período de tempo menos os valores reais
# divididos pelos valores reais.

# MAE - erro absoluto da média
## Medida de erros entre observações emparelhadas que expressam o mesmo fenômeno

err = df_test - arima_f$mean
mape <- mean(abs(err) / (arima_f$mean+ err)) * 100
rmse <- sqrt(mean(err^2, na.rm = TRUE)) 
mae <- mean(abs(err), na.rm = TRUE) 
cbind(mape, rmse, mae)

################################################################################
# PROPHET ######################################################################
################################################################################

df_train = subset(air_passengers, ds < "1959-01-01")
df_test = subset(air_passengers, ds >= "1959-01-01")

# Como foi analisado antes, a sazonalidade não é constante no tempo, mas aumenta 
# com a tendência. Os modelos aditivos não são os melhores para lidar com essas 
# séries temporais. Mas com o Prophet podemos passar da sazonalidade aditiva para 
# a sazonalidade multiplicativa por meio do parâmetro "seasonality_mode".

m <- prophet(df_train,seasonality.mode = 'multiplicative')

# Vamos definir o período da previsão e a frequência (m, s, a)
future <- make_future_dataframe(m, 24, freq = 'm', include_history = F)
forecast <- predict(m, future)
plot(m, forecast)

# Para efeito de comparação, vamos avaliar o modelo com o RMSE, MAE e MAPE.

pred = forecast$yhat
err = df_test$y - forecast$yhat
mape <- mean(abs(err) / (pred+ err)) * 100
rmse <- sqrt(mean(err^2, na.rm = TRUE)) 
mae <- mean(abs(err), na.rm = TRUE) 
cbind(mape, rmse, mae)

# Para facilitar, vamos comparar os dois

## Modelo Arima
# mape     rmse      mae
# 2.356519 14.12564 10.62677

## Modelo Prophet
# mape     rmse      mae
# 5.463905 31.08188 25.89196

################################################################################
###### ARIMAX E SARIMAX ########################################################
################################################################################

# base: uschange: mudanças percentuais nas despesas de consumo pessoal, renda,
# produção, poupança, e taxa de desemprego nos EUA entre 1960 e 2016

str(uschange) # formato dos dados
head(uschange) # avaliação das 5 primeiras linhas
summary(uschange) # análise das variáveis

uschange_df <- uschange[,1:2] # subconjunto da base
autoplot(uschange_df, facets=TRUE) + # plotando os dados
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumptionand personal income")

# Plotando a autocorrelação, aparentemente a variável "income" é mais estacionária
ggAcf(uschange_df) # plotando o ACF
ggPacf(uschange_df) # plotando o PCF

# Decomposição
# variável "consumption"
uschange_freq_4 = uschange_df[,"Consumption"] %>% ts(., frequency=4)
uschange_stl = stl(uschange_freq_4, s.window = "periodic")
autoplot(uschange_stl)

# variável "income"
uschange_income_freq_4 = uschange_df[,"Income"] %>% ts(., frequency=4) 
uschange_income_stl = stl(uschange_income_freq_4, s.window = "periodic")
autoplot(uschange_income_stl)

# Forecast pelo modelo ARIMAX, argumento xreg

# Arimax - Autoregressive Integrated Moving Average with Explanatory Variable
# Modelo de regressão múltipla com um ou mais termos autorregressivos (AR) e um 
# ou um ou mais termos de média móvel (MA). Adequado para prever quando os dados
# são estacionários/não estacionários e multivariados com qualquer tipo de 
# padrão de dados, ou seja, tendência/sazonalidade/ciclicidade.

uschange_arimax = auto.arima(uschange_df[,"Consumption"], # especificando a tendência
                             xreg = uschange_df[,"Income"], # variável exógena
                             trace = TRUE, 
                             seasonal= FALSE,
                             stepwise=FALSE,
                             approximation=FALSE)

summary(uschange_arimax) # avaliação do modelo
checkresiduals(uschange_arimax) # avaliação dos resíduos do modelo  
test(resid(uschange_arimax)) # testes dos resíduos do modelo

# Os resíduos não são claros sobre a estacionariedade. Seria o SARIMAX melhor?

# Forecast pelo modelo SARIMAX, inclusão da sazonalidade
uschange_sarimax = auto.arima(uschange_df[,"Consumption"], # especificação do modelo
                              xreg = uschange_df[,"Income"], # variável exógena
                              trace = TRUE, 
                              seasonal= TRUE, # altera o argumento
                              stepwise=FALSE,
                              approximation=FALSE)

summary(uschange_sarimax) # avaliação do modelo
checkresiduals(uschange_sarimax) # p valor não significativo
# os resíduos são conjuntamente não correlacionados.
test(resid(uschange_sarimax)) # testes dos resíduos do modelo

# Especificando a série 
uschange_ts <- ts(uschange_df, frequency = 4, start = 1970, end= 2016 )

# Separando a base em treino e teste
uschange_train <- window(uschange_ts, end=2010)
uschange_test <- window(uschange_ts, start=2011)

# Treinando o modelo apenas com dados de treino
uschange_arimax2 = auto.arima(uschange_train[,"Consumption"], # especificando a tendência
                              xreg = uschange_train[,"Income"], # variável exógena
                              trace = FALSE, # não apresentar modelos modificados
                              seasonal= FALSE, # não permitir modelo SARIMAX
                              stepwise= FALSE,
                              approximation=FALSE)

# Elaborando as previsões
myforecasts <- forecast::forecast(uschange_arimax2, xreg=rep(uschange_test[,"Income"],20))

# Plotando as previsões
autoplot(myforecasts) + autolayer(uschange_ts[,"Consumption"]) + xlim(1995, 2017)

############## Previsão usando a média dos preditores para uma imagem mais clara

# Elaborando as previsões
myforecasts <- forecast::forecast(uschange_arimax2, xreg= rep(mean(uschange_test[,"Income"]), 25) )

# Plotando as previsões
autoplot(myforecasts) + autolayer(uschange_ts[,"Consumption"]) + xlim(1995, 2017)

################################################################################
###### MODELTIME ###############################################################
################################################################################

# Pacote Modeltime: Tidy Time Series Forecasting using Tidymodels
# Framework: clássicos (ARIMA), novos métodos (Prophet), machine learning (Tidymodels)

# Tidymodels: estrutura dos dados

# O foco será avaliar as séries de maneira automática (Prophet, ARIMA), uso de 
# Machine Learning (ElasticNet, Random Forest) e Híbrido (Prophet-XGBoost)

bike_transactions_tbl <- bike_sharing_daily %>% # carregando a base 
  select(dteday, cnt) %>%
  set_names(c("date", "value")) 

bike_transactions_tbl # avaliando a estrutura dos dados

bike_transactions_tbl %>% # plotando os dados
  plot_time_series(date, value, .interactive = FALSE)

# Vamos separar a base em treino e teste, time_series_split()
splits <- bike_transactions_tbl %>% # período de 3 meses
  time_series_split(assess = "3 months", cumulative = TRUE)

# Convertendo a base em treino e teste
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)

# Vamos a modelagem

# 1. Modelos automáticos, integração do forecast e prophet
# AutoArima
model_fit_arima <- arima_reg() %>% # algoritmo e parâmetro
  set_engine("auto_arima") %>% # seleção da função do pacote
  fit(value ~ date, training(splits)) # inserção da coluna data como regressor

model_fit_arima # sumário do modelo

#Prophet
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%  # inserção da sazonalidade
  set_engine("prophet") %>% # algoritmo prophet
  fit(value ~ date, training(splits))

model_fit_prophet # sumário do modelo

# 2. Modelos de Machine Learning
# Pipeline: Pré-processamento, especificação do modelo, fluxo para combinar o 
# pré-processamento, especificação do modelo e modelo de ajuste.

# Adição de etapas de série temporal
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

# ElasticNet: regressão regularizada que combina linearmente os métodos Lasso e Ridge
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

# Vamos aplicar o workflox
workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>% # especificação do modelo
  add_recipe(recipe_spec %>% step_rm(date)) %>% # adicionando o pré-processamento
  fit(training(splits)) # ajustando o fluxo

# Random Forest: método para classificação, regressão e outras por meio da 
# construção de árvores de decisão no treinamento
model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>% # especificando o modelo
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>% # aplicando o fluxo
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

# 3. Modelos Híbridos de Machine Learning

# Iniciando o fluxo
model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>% # aplicando o fluxo
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost # ajustando o fluxo

# Workflow do pacote Modeltime
# 1. Criando a tabela
# 2. Calibrando, testando a previsão e a acurácia
# 3. Reajustando e fazendo a previsão

# Criando a tabela 
model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost
) 

model_table # avaliando tabela

# Calibrando
calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table # avaliando a tabela 

# Testando a previsão
calibration_table %>% # gerando os dados de previsão para os testes da tabela
  modeltime_forecast(actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(.interactive = FALSE) # plotando

# Avaliando a acurácia
calibration_table %>%
  modeltime_accuracy() %>% # criando métricas de acurácia
  table_modeltime_accuracy(.interactive = FALSE) # criando tabela de comparação

# LEMBRETE #####################################################################
# MAPE - erro absoluto do percentual da média
## Medida de precisão. Mede a precisão como uma porcentagem e pode ser calculado como o 
## erro percentual absoluto médio para cada périodo de tempo menos os valores reais
# divididos pelos valores reais.

# RMSE - raiz do erro quadrático da média
## Usado para avaliar a medida das diferenças entre os valores (amostra ou população) previstos
## por mum modelo ou um estimador e os valores observados

# MAE - erro absoluto da média
## Medida de erros entre observações emparelhadas que expressam o mesmo fenômeno

# Analizando os resultados

calibration_table %>% # Removendo o modelo ARIMA pela baixa acurácia
  filter(.model_id != 1) %>%
  
# Refinando a previsão futura
  modeltime_refit(bike_transactions_tbl) %>%
  modeltime_forecast(h = "12 months", actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(.interactive = FALSE)

################################################################################
###### AUTOMATIC FORECASTING ###################################################
################################################################################

# Base: Vendas Semanais do Walmart disponível no pacote timetk

data_tbl <- walmart_sales_weekly %>% # carregando a base
  select(id, Date, Weekly_Sales)

# Plotando a série
data_tbl %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var    = Date,
    .value       = Weekly_Sales,
    .facet_ncol  = 2,
    .smooth      = F,
    .interactive = F
  )

# Gerando uma base de treino e teste

splits <- time_series_split(data_tbl, assess = "3 month", cumulative = TRUE) # período de 3 meses

recipe_spec <- recipe(Weekly_Sales ~ ., data = training(splits)) %>%
  step_timeseries_signature(Date) 

train_tbl <- training(splits) %>% bake(prep(recipe_spec), .) # base de treino
test_tbl  <- testing(splits) %>% bake(prep(recipe_spec), .) # base de teste

h2o.init(
  nthreads = -1,
  ip       = 'localhost',
  port     = 54321
)

model_spec <- automl_reg(mode = 'regression') %>% # uso do algoritmo de ML
  set_engine(
    engine                     = 'h2o',
    max_runtime_secs           = 5, 
    max_runtime_secs_per_model = 3,
    max_models                 = 3,
    nfolds                     = 5,
    exclude_algos              = c("DeepLearning"),
    verbosity                  = NULL,
    seed                       = 786
  ) 

model_spec # avaliando a especificação do modelo

# Treinando o modelo
model_fitted <- model_spec %>%
  fit(Weekly_Sales ~ ., data = train_tbl)

# Checando o modelo
model_fitted

# Prevendo o modelo de teste
predict(model_fitted, test_tbl)

# Como o modelo preparado voltamos para o fluxo do Modeltime
# 1. Criando a tabela
# 2. Calibrando, testando a previsão e a acurácia
# 3. Reajustando e fazendo a previsão

# Criando a tabela modelo
modeltime_tbl <- modeltime_table(
  model_fitted
) 

# Avaliando a tabela
modeltime_tbl

# Calibrando, testando e configurando a acurácia do modelo
modeltime_tbl %>%
  modeltime_calibrate(test_tbl) %>%
  modeltime_forecast(
    new_data    = test_tbl,
    actual_data = data_tbl,
    keep_data   = TRUE
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2, 
    .interactive = FALSE
  )

# Fazendo a previsão futura
data_prepared_tbl <- bind_rows(train_tbl, test_tbl) # preparando os dados

future_tbl <- data_prepared_tbl %>% # criando o dataset
  group_by(id) %>%
  future_frame(.length_out = "1 year") %>%
  ungroup()

future_prepared_tbl <- bake(prep(recipe_spec), future_tbl)

# Retreinando todo a base de dados. Prática para melhorar o resultado das previsões
refit_tbl <- modeltime_tbl %>%
  modeltime_refit(data_prepared_tbl)

# Visualiação da previsão
refit_tbl %>%
  modeltime_forecast(
    new_data    = future_prepared_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = FALSE
  )
