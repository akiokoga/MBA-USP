# Instalação e Carregamento de Todos os Pacotes ---------------------------

################################## ATENÇÃO #####################################
# Como discutido na aula estaremos usando pacotes que se sobrepõem, por isso,
# para facilitar os estudos de vocês separei dois grupos de pacotes para rodarem. 
# Entrei até em contato com o criador do pacote "forecast" para ter alternativas.
# Assim, não se esqueçam que caso o pacote "forecast" dê erro como apresentado 
# na aula será preciso reinstalar o pacote. 
################################################################################

################################################################################
# Rode o primeiro grupo de pacotes para executarem as linhas 1 a 534
################################################################################

pacotes <- c("TSA", "dplyr", "datasets","forecast","fpp2","tseries","patchwork", 
             "DataCombine", "TTR", "scales", "tidyverse", "tidyquant", "gridExtra",
             "tibbletime", "itsmr", "here", "ggplot2")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
  
################################################################################
# Rode o seegundo grupo de pacotes para executarem as linhas 534
################################################################################

pacotesb <- c("TSA", "dplyr", "datasets","prophet", "forecast","fpp2","tseries","patchwork", 
             "DataCombine", "TTR", "scales", "tidyverse", "tidyquant", "gridExtra",
             "tibbletime", "itsmr", "here", "ggplot2", "ggseas", "data.table",
             "tidymodels", "timetk", "lubridate", "modeltime.h2o", "modeltime")

if(sum(as.numeric(!pacotesb %in% installed.packages())) != 0){
  instalador <- pacotesb[!pacotesb %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotesb, require, character = T) 
} else {
  sapply(pacotesb, require, character = T) 
}