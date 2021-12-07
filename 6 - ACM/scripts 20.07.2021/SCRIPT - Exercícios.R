# Pacotes a serem instalados e carregados ---------------------------------
pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","reshape2","FactoMineR",
             "cabootcrs","knitr","kableExtra","gifski","gganimate","factoextra",
             "plot3D","viridis")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Utilizando a ANACOR para demonstrar comportamentos temporais de forma animada
load("covid_america_weekly.RData")

# Apresentando os dados
covid_america_weekly %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# O primeiro passo é o de estabelecer, uma ANACOR para cada período de tempo.
# Aqui, estamos estabelecendo uma ANACOR para a semana 78 desde o primeiro
# caso de COVID-19 reportado no continente americano.

# Criando uma tabela de contingências
tab <- table(covid_america_weekly$country, 
             covid_america_weekly$lethality_Q5)

tab

# Teste Qui-Quadrado
qui2_covid <- chisq.test(tab)
qui2_covid

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_covid$stdres) %>%
  rename(country = 1,
         let_q5 = 2) %>% 
  ggplot(aes(x = country, y = let_q5, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3, angle = 90) +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# Elaborando a ANACOR:
anacor <- CA(tab)

# Plotando o mapa perceptual de maneira mais elegante:

# Capturando todas as coordenadas num só objeto
ca_coordenadas <- rbind(anacor$row$coord, anacor$col$coord)
ca_coordenadas

# Capturando a quantidade de categorias por variável
id_var <- apply(covid_america_weekly[,c(1,9)],
                MARGIN =  2,
                FUN = function(x) nlevels(as.factor(x)))
id_var

# Juntando as coordenadas e as categorias capturadas anteriormente
ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))

ca_coordenadas_final

# Mapa perceptual bidimensional

# Mapa perceptual elegante:
ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Category, 
             color = Variable, 
             shape = Variable)) +
  geom_point(size = 2) +
  geom_text_repel(max.overlaps = 100,
                  size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor$eig[2,2], digits = 2), "%"))) +
  scale_color_viridis_d(option = "viridis") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")


# Elaborando a animação em razão do transcorrer temporal ------------------

# A base de dados a ser carregada a seguir contém as coordenadas de todas as
# ANACOR feitas, desde a 2ª até a 78ª semana na América. Foram consideradas
# duas dimensões de análise. 
load("coords_covid_america_byweek.RData")

# Apresentando os dados
coords_covid_america_byweek %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#Sobrepondo as coordenadas dos mapas perceptuais em um só plano
coords_covid_america_byweek %>%
  ggplot() +
  geom_point(aes(x = Dim.1, y = Dim.2, 
                 color = country %in% c("L1","L2","L3","L4","L5"), size = 3,
                 shape = country %in% c("L1","L2","L3","L4","L5"))) +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, 
                      label = country),
                  max.overlaps = 3000) +
  scale_color_viridis_d() +
  labs(x = "Dimensão 1",
       y = "Dimensão 2") +
  theme(legend.position = "none") -> mapas_perceptuais  

#Definindo que a interação entre os mapas perceptuais se dará em razão do passar
#das semanas
mapa_animado <- mapas_perceptuais + transition_time(week) +
  enter_fade() +
  labs(title = "Week: {frame_time}") +
  exit_fade()

#Estabelecendo um fundo branco para os gráficos
theme_set(theme_bw())

#Resultado final
animate(mapa_animado, renderer = gifski_renderer(), fps = 1)


# Combinando técnicas -----------------------------------------------------

# Para fins didáticos, vamos utilizar duas bases de dados já visitadas. 
# Primeiramente, estabeleceremos uma ACM e, depois, uma PCA. Por fim, faremos
# uma clusterização.

load("notasfatorial.RData")

# Apresentando os dados da base 'notasfatorial'
notasfatorial %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

load(file = "perfil_investidor_aplicacao.RData")

# Apresentando os dados da base 'perfil_investidor_aplicacao'
perfil_investidor_aplicacao %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Juntado as duas bases
base_dados <- notasfatorial %>% 
  left_join(perfil_investidor_aplicacao, by = "estudante")

# Apresentando a base de dados a ser utilizada
base_dados %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Vamos começar pela ACM:


# Estabelecendo a ACM -----------------------------------------------------

# 1. Verificando o teste Qui-Quadrado entre o cruzamentos de variáveis a serem
# considerados

# A) Perfil x Aplicação
tab_perfil_aplicacao <- table(perfil_investidor_aplicacao$perfil,
                              perfil_investidor_aplicacao$aplicacao)

qui2_perfil_aplicacao <- chisq.test(tab_perfil_aplicacao)
qui2_perfil_aplicacao

# B) Perfil x Estado Civil
tab_perfil_estadocivil <- table(perfil_investidor_aplicacao$perfil,
                                perfil_investidor_aplicacao$estado_civil)

tab_perfil_estadocivil

qui2_perfil_estadocivil <- chisq.test(tab_perfil_estadocivil)
qui2_perfil_estadocivil

# C) Aplicação x Estado Civil
tab_aplicacao_estadocivil <- table(perfil_investidor_aplicacao$aplicacao,
                                   perfil_investidor_aplicacao$estado_civil)

tab_aplicacao_estadocivil

# 2. A ACM
ACM <- MCA(base_dados[, 6:8], method = "Indicador")

# 3. Capiturando as coordenadas das observações em nossa base de dados
base_dados[c("D1","D2","D3","D4","D5")] <- data.frame(ACM$ind$coord)

# 4. Para facilitar o transcorrer do exercício, removeremos as variáveis
# categóricas originais, visto que suas coordenadas já as representam.
base_dados <- base_dados[,-c(6:8)]


# Estabelecendo uma PCA ---------------------------------------------------

# 1. Para a utilização do algoritmo prcomp(), o R exige a padronização dos
# dados. Não utilizaremos as coordenadas da ACM, mas já as estamos padronizando
# porque a subsequente clusterização a exigirá.
base_dados_std <- base_dados %>% 
  column_to_rownames("estudante") %>% 
  scale() %>% 
  data.frame()

# 2. A PCA
AFCP <- prcomp(base_dados_std[,1:4])

AFCP

# 3. Vamos considerar os fatores cujos eigenvalues se mostraram maiores do que
# 1. Assim, para salvá-los em nossa base de dados, podemos:

scores_fatoriais <- t(AFCP$rotation)/AFCP$sdev 

#Assumindo-se apenas o F1 e F2 como indicadores, calculam-se os scores 
#fatorias
score_D1 <- scores_fatoriais[1,]
score_D1

score_D2 <- scores_fatoriais[2,]
score_D2

F1 <- t(apply(base_dados_std[,1:4], 1, function(x) x * score_D1))
F2 <- t(apply(base_dados_std[,1:4], 1, function(x) x * score_D2))

F1
F2

F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * 1)

F1

F2 <- data.frame(F1) %>%
  mutate(fator2 = rowSums(.) * 1)

F2

base_dados_std[c("F1","F2")] <- cbind(F1$fator1, F2$fator2)

# 4. Por razões didáticas, excluiremos as variáveis métricas originais da base
# de dados:
base_dados_std <- base_dados_std[,-c(1:4)]


# Estabelecendo a Clusterização -------------------------------------------

# 1. Clustering
cluster_estudantes <- kmeans(base_dados_std, centers = 2)

# 2. Observando os resultados
fviz_cluster(cluster_estudantes, data = base_dados_std)

# 3. Uma outra maneira de enxergar os dados

# Vamos capturar as coordenadas do eixo Z:
plot <- fviz_cluster(cluster_estudantes, data = base_dados_std)

View(plot)

# Note que só as coordenadas dos eixos X e Y. Vamos "adaptar" o algoritmo
# fviz_cluster() para que ele nos retorne os valores do eixo Z:
fviz_cluster

fviz_cluster_adaptado(object = cluster_estudantes,
                      data = base_dados_std)

# Aparentemente, nada mudou, certo?
coordenadas <- fviz_cluster_adaptado(object = cluster_estudantes,
                                     data = base_dados_std)

View(coordenadas)

scatter3D(x = coordenadas$data$x, 
          y = coordenadas$data$y, 
          z = coordenadas$data$Dim.3, 
          zlim = c(-3,3),
          ylim = c(-3,3),
          xlim = c(-3,3),
          pch = 19,
          bty = "b2",
          colvar = as.numeric(coordenadas[["data"]][["cluster"]]),
          col = viridis(200))

# Fim ---------------------------------------------------------------------