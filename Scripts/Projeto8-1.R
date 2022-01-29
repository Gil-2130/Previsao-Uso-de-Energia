#### Formação Cientista de dados
#
#
# Projeto com feedback 8
#
#### Modelagem Preditiva em IoT- Previsão de Uso de Energia
#
# Este projeto de IoT tem como objetivo a criação de modelos preditivos para
# a previsão de consumo de energia de eletrodomésticos. Os dados utilizados
# incluem medições de sensores de temperatura e umidade de uma rede sem fio,
# previsão do tempo de uma estação de um aeroporto e uso de energia utilizada porluminárias.
#
# Nesse projeto de aprendizado de máquina você deve realizar a filtragem de
# dados para remover parâmetros não-preditivos e selecionar os melhores recursos
# (melhores features) para previsão. O conjunto de dados foi coletado por um
# período de 10 minutos por cerca de 5 meses. As condições de temperatura e
# umidade da casa foram monitoradas com uma rede de sensores sem fio ZigBee.
#
# Cada nó sem fio transmitia as condições de temperatura e umidade em torno de 3 min.
# Em seguida, a média dos dados foi calculada para períodos de 10 minutos.
# Os dados de energia foram registrados a cada 10 minutos com medidores deenergia de barramento m.
# O tempo da estação meteorológica mais próxima do aeroporto (Aeroporto de Chievres, Bélgica)
# foi baixado de um conjunto de dados públicos do Reliable Prognosis (rp5.ru) e mesclado com os conjuntos
# de dados experimentais usando a coluna de data e hora. Duas variáveis aleatórias foram
# incluídas no conjunto de dados para testar os modelos de regressão e filtrar os atributos não preditivos (parâmetros).
#
# Seu trabalho agora é construir um modelo preditivo que possa prever o consumo de energia com base nos dados
# de sensores IoT coletados.
# Recomendamos usar RandomForest para a seleção de atributos e SVM, Regressão
# Logística Multilinear ou Gradient Boosting para o modelo preditivo.
# Recomendamos ainda o uso da linguagem R.

# Dicionário de Dados
# date = data da coleta dos dados
# Appliances = (variavel Target) Uso de energia consumida
# lights = quantidade de lampadas/ aparelhos ligados
# TX = Temperatura ambiente
# RH_x = Umidade Relativa ambiente
# WindSpeed = Velocidade do vento
# Visibility = Visibilidade (medição padrão em KM)
# Tdewpoint = Pesquisei e parece estar relacionado á temperatura de condensação (Dew Point)
# rv1 e rv2 = Variaveis aleatórias incluidas no dataset.
#Elas contém a mesma informação. Veriicaremos se são úteis para o modelo
# WeekStatus = Verifica se são dias comuns da semana ou finais de semana
# Day_of_week = Dia da semana
# NSM = Medição do tempo (segundos)

#===============================================================================
#
#========================== carregando bibliotecas==============================
#
#===============================================================================

library(dplyr) # Leitura dos datasets
library(readr) #manipulação de dados
library(RColorBrewer) # Alterar cores dos gráficos
library(e1071) # biblioteca com multiplas funções para análise e machine learning
library(ggplot2) # Biblioteca para plot dos gráficos
library(caret) # Biblioteca de machine learning
library(ROCR) # Para construção de métricas de avaliação
library(randomForest) # Algoritmo de machine Learning
library(lattice) # Recurso para visualização de dados
library(knitr) # Pacote para relatórios dinâmicos
library(car) # Função para aplicação de comparações entre modelos de regressão
library(MASS) # Pacote estatístico para análise de multicolinearidade som stepAIC
library(gains) # Pacote auxiliar na avaliação de modelos como o SVM
library(pROC) # Apresenta análises de Curva ROC
library(multiROC) # Pacote auxiliar de análise de curva ROC
library(clustertend) # pacote estatístico para análise de tendência de clusterização de um conjunto de dados
library(NbClust) # Identifica o numero ideal de cluster nos dados analisados
library(ggfortify) # Pacote auxiliar na plotagem de clusters
library(cluster) # Método para análise de clusters


# Dataframe para guardar os resultados das análises
dfResults <- data.frame(Model = character(),
                        RMSE = numeric(),
                        Accuracy = numeric())

#===============================================================================
#
#========================= Carregando os Dataset's =============================
#
# ==============================================================================
df_train <- read.csv('projeto8-training.csv') # Carregando o dataset de treino

# Resumo
str(df_train)

#Visualizando o dataset
View(df_train)

# Verificando valores nulos no dataset de treino
sum(is.na(df_train))

# Dataset de teste
df_test <- read.csv('projeto8-testing.csv')

# Resumo
str(df_test)

# Visualizando o dataset de teste
View(df_test)

# Verificando se há valores nulos
sum(is.na(df_test))


#===============================================================================
#
#==== A etapa abaixo pode ser feita se desejar, porém será necessário ajustar
# todo código posterior.Irei fazer uma segunda versão do modelo com essa etapa!
#
#===============================================================================

# Conforme verificado nos dois datasets, o número de variaveis/atributos é o mesmo
# Faremos a união de datasets para melhorar o processo de análise
#df_unico <- rbind(df_train, df_test)
#str(df_unico)
#View(df_unico)
#sum(is.na(df_unico))

# vamos olhar a variavel target (Appliances)
#summary(df_unico$Appliances)

#===============================================================================
#
#============================ PErguntas de negócio==============================
#
#===============================================================================
#
#====01- Em qual momento há maior consumo de energia?
analysis <- df_train %>%
  count(WeekStatus) %>%
  as.data.frame()

# Pie chart
ggplot() +
  geom_bar(data = analysis,
           aes(x = "",
               y = n,
               fill = WeekStatus),
           stat = 'identity') +
  coord_polar('y',
              start = 0) +
  scale_fill_brewer(palette = 'Paired') +
  theme_void() +
  theme(axis.text.x = element_blank()) +
  ggtitle('Consumo em dias da semana e Semana')

# Salvando o trabalho
ggsave('1-Consumo-de-energia.png', width = 10, height = 5)

# R: Os dias da semana são os dias de maior consumo de energia

#====02 Qual é o consumo por kWh e WeekStatus( dia da semana ou final de semana)
analysis <- df_train %>%
  group_by(Appliances, WeekStatus) %>%
  count(Appliances) %>%
  filter(Appliances <= 150) %>%
  as.data.frame()

# Bar plot
ggplot() +
    geom_bar(data = analysis,
             aes(x = Appliances,
                 y = n,
                 fill = WeekStatus),
             stat = 'identity',
             position = 'dodge') +
    ggtitle('Consumo em kWh')
# Salvando o trabalho
ggsave('2-consumo-de-energia-em-kwh.png', width = 10, height = 5)

# R:Existe um consumo de 50kwh e o consumo se concentra durante os dias da semana.

#====03- Há  relação entre o consumo médio de energia com a temperatura da cozinha? 
analysis <- df_train %>%
    count(T_kitchen = round(T1), Appliances) %>%
    as.data.frame()
# Plot
ggplot() +
    geom_point(data = analysis,
               aes(x = T_kitchen,
                   y = Appliances),
               stat = 'identity') +
    geom_vline(xintercept = mean(df_train$T1), 
               linetype = 1,
               color = 'red',
               size = 2) +
    theme_bw() +
    scale_fill_brewer(palette = 'Paired') +
    ggtitle('Consumo de Energia x Temperatura(Cozinha)')
# Salvando o trabalho
ggsave('3-consumo de energia cozinha.png', width = 10, height = 5)

# R:É posivel identificar que não há relação entre a temperatura da cozinha e o consumo médio de energia.
#
#====04- Há relação entre o consumo de energia e a temperatura média da sala de estar?
analysis <- df_train %>%
    count(T2, Appliances) %>%
    as.data.frame()

#plot
ggplot() +
    geom_point(data = analysis,
               aes(x = T2,
                   y = Appliances),
               stat = 'identity') +
    theme_bw() +
    scale_fill_brewer(palette = 'Paired') +
    ggtitle('Consumo de Energia x Temperatura(Sala de Estar)')
# Salvando o trabalho
ggsave('4-Consumo de energia sala de estar.png', width = 10, height = 5)

# plot da temperatura média da sala de estar
ggplot() +
    geom_point(data = analysis,
               aes(x = round(T2),
                   y = Appliances),
               stat = 'identity') +
    geom_vline(xintercept = mean(df_train$T2),
               linetype = 1,
               color = 'red',
               size = 2) +
    theme_bw() +
    scale_fill_brewer(palette = 'Paired') +
    ggtitle('Consumo de Energia x Temperatura(Sala de Estar)')

# salvando o trabalho
ggsave('4-1 Temperatura média da sala de estar.png', width = 10, height = 5)

# R: Novamente vemos que não relação da temperatura da sala de estar com o consumo de energia
# porém existe um consumo maior quando a temperatura está próximo da média
#

#====05- A temperatura externa (T_out) pode afetar o consumo de energia?
analysis <- df_train %>%
    count(T_extern = round(T_out, digits = 0), Appliances) %>%
    as.data.frame()

# plot
ggplot() +
    geom_point(data = analysis,
               aes(x = T_extern,
                   y = Appliances),
               stat = 'identity') +
    geom_vline(xintercept = 1.5,
               linetype = 1,
               color = 'red',
               size = 2) +
    geom_vline(xintercept = 15.5,
               linetype = 1,
               color = 'red',
               size = 2) +
    theme_bw() +
    scale_fill_brewer(palette = 'Paired') +
    ggtitle('Energia consumida x Temperatura Externa')
# Salvando o trabalho
ggsave('5-consumo de energia pela Temperatura externa.png', width = 10, height = 5)

# R: No gráfico observamos que o consumo de energia ocorre quando a temperatura
# esterna está entre 1 e 16 graus. o que faz sentido, pois está meio frio para nós,
# logo buscamos fontes de calor como aquecedores por exemplo.

#===============================================================================
#
#===================== Engenharia de Atributos =================================
#
#===============================================================================
#
# Removendo variavies não relevantes para criação dos modelos preditivos
train_subset <- subset(df_train, select = -c(date, NSM, WeekStatus, Day_of_week))
test_subset <- subset(df_test, select = -c(date, NSM, WeekStatus, Day_of_week))
str(train_subset)

# checando por valores nulos
sum(is.na(train_subset))
sum(is.na(test_subset))

# outra forma de olhar se há valores nulos por coluna do dataset
# sapply(train_subset,function(x)sum(is.na(x)))

# Tabela de frequencia para a variavel target
prop.table(table(train_subset$Appliances)) *100

# também pode ser feito com os seguintes comandos;
# as.data.frame(table(train_subset$Appliances))
# table(train_subset$Appliances)

# Análise Visual - Box Plots e Histogramas

# Salvando o Box Plot
png('6-boxplot.png', width = 800, height = 1000, res = 100)
boxplot(train_subset$Appliances)
dev.off()

# Salvando o Histograma
png('7-Histograma.png', width = 1200, height = 900, res = 100)
hist(train_subset$Appliances)
dev.off()

# Com esses plots percebemos que existem alguns outliers que fogem da média 
# de consumo de energia. 
# talvez os dados tenham algum erro ou algumas pessoas usando acima da média?
# O consumo aparece com maior frequencia entre 0 e 150 kWh

# Colocando as variaveis na mesma escala
# Modelos de regressão linear esperam receber os dados padronizados
str(train_subset)

# conforme observado não temos variaveis categóricas em nosso dataset
# Usaremos a função (scale.features) paa normalização de variaveis numéricas.
# Ambos regressão linear e regressão logistica esperam receber os dados dessa forma.

# Criando função para colocar os dados na mesma escala
scale.features <- function(df, variables) {
    for(variable in variables){
      df[[variable]] <- scale(df[[variable]], center = T, scale = T)
    }
  return(df)
}

# Extraindo todas as colunas do dataset exceto a variavel target (Appliances)
cols_data <- names(train_subset[-1])

# Chamando a função criada acima e aplicando ás colunas extraídas anteriormente
train_scaled <- scale.features(train_subset, cols_data)
test_scaled <- scale.features(test_subset, cols_data)


# Verificando os dados
View(train_scaled)
View(test_scaled)

# Poderiamos ter feito da seguinteforma também;
# train_subset_scaled <- scale(train_subset, center = T, scale = T)
# test_subset_scaled <- scale(test_subset, center = T, scale = T)


#===============================================================================
#
#=========================== Correlação=========================================
#
#===============================================================================

# Definindo as colunas para análise de correlação
cols <- names(train_scaled)
str(cols)

# Métodos de correlação; Pearson, Spearman, Kendall
# Pearson https://pt.wikipedia.org/wiki/Coeficiente_de_correla%C3%A7%C3%A3o_de_Pearson
# Speraman https://pt.wikipedia.org/wiki/Coeficiente_de_correla%C3%A7%C3%A3o_de_postos_de_Spearman
# Kendall https://pt.wikipedia.org/wiki/Coeficiente_de_correla%C3%A7%C3%A3o_tau_de_Kendall

# Criando um vetor com os métodos de correlação
meth <- c("pearson", "spearman")

# Aplicando os métodos de correlação juntamente com os métodos de cores

cors <- lapply(meth, function(method)(cor(train_scaled[,cols], method = method)))
head(cors) # Visualmente ficou ruim para identificar a correlação, iremos preparar o plot para facilitar.

# Preparando o plot de correlação

# Você pode entrar em https://mycolor.space/ escolher uma cor, 
# clicar em generate e escolhas as cores que você quiser!
col.l <- colorRampPalette(c('#1E90C0', '#00ADCA', '#4EDDA5', '#EC9929', '#002B53',
                                 '#374955', '#DF587B', '#619438', '#004F7A'))(90)


# Adicionando zeros para a diagonal

# Criando função para plotagem
plot.cors <- function(x, labs){
  diag(x) <- 0.0
  plot(levelplot(x,
                 main = paste("Plot de Correlação Usando Método", labs),
                 scales = list(x = list(rot = 90), cex = 1.0),
                 col.regions=col.l))
}

# mapa de correlação
png('8-correlacao_map.png', width = 900, height = 900, res = 100)
Map(plot.cors, cors, meth)
dev.off()

# Obs: o plot do mapa de correlação não foi renderizado na minha maquina,
# porém o mesmo foi salvo e contém algumas observações;
# Existem variaveis altamente correlacionadas e precisam removidas do nosso dataset.
# Para isso usaremos machine learning para extrair as features mais importantes

#===============================================================================
#
#========================== Machine Learning ===================================
#
#===============================================================================

#=== Processo 1 (variaveis Importantes) Multiple Linear Regression
model_v1 <- lm(Appliances ~ ., data = train_scaled)
summary(model_v1)

# R-Squared muito baixo = 0.1694, onde é de 0 a 1(quanto maior, melhor)
# RSE altíssimo = 93.79
# Porém o p-value é bem baixo (p-value < 0.05) indicando que as vaiaveis preditoras
# ajudam a explicar a variavel target


#==== Veremos agora a multicolinearidade e usaremos o stepAIC
"https://www.rdocumentation.org/packages/MASS/versions/7.3-54/topics/stepAIC"
steps <- stepAIC(model_v1, direction = 'both', trace = FALSE)
summary(steps)
summary(steps)$coeff
summary(steps)$r.squared

# R2 muito baixo = 0.1691 (referencia é 0 a 1, onde quanto mais próximo de 1 é melhor)
# Encontramos as melhores variaveis para nosso modelo:
# lights + RH_1 + T2 + RH_2 + T3 + RH_3 + T4 + T6 + RH_6 + RH_7 + T8 + RH_8 +
# T9 + T_out + RH_out + Windspeed + Tdewpoint

#==== Processo 2 - Regressão Linar Multipla (model_v2)
# Este
model_v2 <- lm(Appliances ~ lights + RH_1 + T2 + RH_2 + T3 + RH_3 + T4 + T6 + RH_6 +
                 RH_7 + T8 + RH_8 + T9 + T_out + RH_out + Windspeed + Tdewpoint, data = train_scaled)
summary(model_v2)

# O R2 teve leve alteração, mas ainda muito longe do ideal
# Continuaremos o trabalho para detectar multicolinearidade e excluir esse problema do nosso modelo
# Utilizaremos o método VIF https://www.analyticsvidhya.com/blog/2020/03/what-is-multicollinearity/
?kable
?vif
kable(vif(model_v2), align = 'c')

# Interpretando o resultado:
# Valores para VIF maiores que 10 são considerados extremos, devemos considerar aqueles,
# que ficam entre 5 e 10, ou seja,indicam alta correlação e será problemático.
# Valores acima disso devemos assumir queos coeficientes de regressão estão mal-estimados,
# devido a multicolinearidade.

# farei experimentos removendo algumas variaveis do modelo e realizar novos testes.
# opcionalmente removerei RH_3, RH_6 e RH_7 pois os valores estão próximos de 10
model_v3 <- lm(Appliances ~ lights + RH_1 + T2 + RH_2 + T3 + T4 + T6 +
                  RH_7 + T8 + RH_8 + T9 + T_out + RH_out + Windspeed + Tdewpoint, data = train_scaled)
summary(model_v3) 
kable(vif(model_v3), align = 'c')

# novo modelo sem as variaveis RH_1, T3, Tdewpoint, RH_7, RH_8
model_v4 <- lm(Appliances ~ lights + RH_1 + T3  + RH_2 + RH_3  + T6 + T8 + RH_8
                + Windspeed, data = train_scaled)
summary(model_v4)
kable(vif(model_v4), align = 'c')

#=== O modelo 4 apresentou menor colinearidade,executaremos mais testes com ele


# ===== Trabalharemos agora com os dados de teste
test_scaled_pred <-
  test_scaled[,c('Appliances', 'lights', 'RH_1', 'T3', 'RH_2', 'RH_3', 'T6',
                 'T8', 'RH_8', 'Windspeed')]
View(test_scaled_pred)

# Realizando previsões
previsao <- round(predict(model_v4, test_scaled_pred, type = 'response'))

esperado <- as.numeric(test_scaled$Appliances)

# Score RSME
rmse_model <- RMSE(previsao, esperado)
str(rmse_model)

# RMSE = 95.4

# Score AUC do modelo
curve_roc <- multiclass.roc(response = test_scaled$Appliances, predictor = previsao)
class(test_scaled$Appliances)
class(previsao)

accuracy <- curve_roc$auc
print(accuracy)

# multi-class AUc 0.7053 ( iso significa 70.53% de acurácia)

#=== Plotando AUC
png('9-AUC(lm).png', width = 1000, height = 1000, res = 100)
plot(roc(test_scaled$Appliances, previsao))
dev.off()

# Conforme visto no gráfico, tivemos pésimos resultados com os algoritmos LM
# Então faremos testes com RandomForest

# combinando os resultados acima
modelo <- 'lm'
results_1 <- data.frame(modelo, rmse_model, accuracy)
colnames(results_1) <- c('Model', 'RMSE', 'Accuracy')
dfResults <- rbind(dfResults, results_1)
View(dfResults)


#===============================================================================
#              |                |
#= Processo 2  | Random Forest  |
#              |                |
#===============================
# criando um modelo com random forest e extraindo as variaveis mais importantes
rf_model <- randomForest(Appliances ~ .,
                         data = train_scaled,
                         ntree = 90,
                         nodesize = 10,
                         importance = TRUE)
varImpPlot(rf_model)

# Features mais importantes: lights + RH_1 + T2 + RH_2 + T3 + RH_3 + 
# T6 + RH_7 + T8 + RH_8 + T9 + RH_out + Windspeed + Press_mm_hg + RH_6 + RH_5 +
# RH_9 + RH_4 + T5 + T1 + Tdewpoint + Windspeed + T4 + T_out

test_scaled_pred <-
    test_scaled[,c('Appliances', 'lights', 'RH_1', 'T2', 'RH_2', 'T3', 'RH_3', 'T6',
                   'RH_7', 'T8', 'RH_8', 'T9', 'RH_out', 'Windspeed', 'Press_mm_hg',
                   'RH_6', 'RH_5', 'RH_9', 'RH_4', 'T5', 'T1', 'Tdewpoint', 'T4','T_out')]
View(test_scaled_pred)

# novo modelo com as features mais importantes analisadas pelo randomForest
model_V5_rf <- function(n){
  model_v5 <- randomForest(Appliances ~ lights + RH_1 + T2 + RH_2 + T3 + RH_3 + 
                          T6 + RH_7 + T8 + RH_8 + T9 + RH_out + Windspeed + Press_mm_hg + RH_6 + RH_5 +
                          RH_9 + RH_4 + T5 + T1 + Tdewpoint + Windspeed + T4 + T_out,
                          
                          data = train_scaled,
                          ntree = n,
                          nodesize = 10)
    previsao_rf <- round(predict(model_v5, newdata = test_scaled_pred), digits = 0)
    esperado_rf <- test_scaled$Appliances
    
    return(RMSE(previsao_rf, esperado_rf))


}

# Construindo tabela para guardar os valores RMSE para análise
table_rmse <- data.frame(ntree = seq(5, 100, 5))
Result <- c()

# Função para controle
for(i in table_rmse$ntree){
    Result <- append(Result, model_V5_rf(i))
}

# Realizando o merging da tabla com o resultado da análise
table_rmse <- cbind(table_rmse, Result)

# Análise gráfica
colnames(table_rmse) <- c('ntree', 'ResultRMSE')

png('10-RMSE Analysis.png', width = 2000, height = 1000, res = 100)
ggplot(table_rmse, aes(x = ntree, y = ResultRMSE)) +
    geom_point() +
    stat_smooth(method = 'lm', formula = y ~ poly(x, 13), se = FALSE) +
    labs(title = 'Análise RMSE', x = 'ntre', y = 'Values') +
    guides(color = 'none') +
    theme_bw()
dev.off()


# numero de arvores
ntree = 80

model_v5 <- randomForest(Appliances ~lights + RH_1 + T2 + RH_2 + T3 + RH_3 + T6 + RH_7 +
                           T8 + RH_8 + T9 + RH_out + Windspeed + Press_mm_hg + RH_6 + RH_5 +
                           RH_9 + RH_4 + T5 + T1 + Tdewpoint + Windspeed + T4 + T_out,
                         
                         data = train_scaled,
                         ntree = ntree,
                         nodesize = 10)
print(model_v5)

# Realizando Previsões
previsao_v5 <- round(predict(model_v5, newdata = test_scaled_pred), digits = 0)
esperado_v5 <- as.numeric(test_scaled$Appliances)

# Avaliando o RMSE do modelo
rmse_v5 <- RMSE(previsao_v5, esperado_v5)
print(rmse_v5)  # RMSE de 72.56

# Avaliando o AUC do modelo
curve_roc_v5 <- multiclass.roc(response = test_scaled$Appliances, predictor = previsao_v5)
class(test_scaled$Appliances)
class(previsao_v5)

accuracy_v5 <- curve_roc_v5$auc
print(accuracy_v5) #Multi-class area under the curve: 0.7908 ou seja; 79.08% de acurácia

# Plotando a AUC
png('11-AUC(RandomForest).png', width = 1000, height = 1000, res = 100)
plot(roc(test_scaled$Appliances, previsao_v5))
dev.off()

# Combinando e salvando os resultados
model_rf <- 'Random Forest'
results_2 <- data.frame(model_rf, rmse_v5, accuracy_v5)
colnames(results_2) <- c('Model', 'RMSE', 'Accuracy')
dfResults <- rbind(dfResults, results_2)
View(dfResults)

#==== Random forest obteve ótimos resultados com o RSME bem abaixo se comparado ao LM
# Agora usaremos SVM (Support Vector Machines)
#
#
#===============================================================================
#              |                |
#= Processo 3  |      SVM       |
#              |                |
#===============================
?svm
#Criando o modelo SVM
model_v6_svm <- svm(Appliances ~ lights + RH_1 + T2 + RH_2 + T3 + RH_3 + T6 + RH_7 +
                      T8 + RH_8 + T9 + RH_out + Windspeed + Press_mm_hg + RH_6 + RH_5 +
                      RH_9 + RH_4 + T5 + T1 + Tdewpoint + Windspeed + T4 + T_out,
                    dat = train_scaled,
                    na.action = na.omit,
                    scale = TRUE)
summary(model_v6_svm)

# Realizando previsões com o modelo
test_scaled_pred <-
    test_scaled[, c('Appliances', 'lights', 'RH_1', 'T2', 'RH_2', 'T3', 'RH_3', 'T6',
                    'RH_7', 'T8', 'RH_8', 'T9', 'RH_out', 'Windspeed', 'Press_mm_hg',
                    'RH_6', 'RH_5', 'RH_9', 'RH_4', 'T5', 'T1', 'Tdewpoint', 'T4','T_out')]
#View(test_scaled_pred)

# Realizando previsões
previsao_v6 <- round(predict(model_v6_svm,
                             newdata = test_scaled_pred),
                     digits = 0)
esperado_v6 <- as.numeric(test_scaled$Appliances)

# Avaliando RMSE do modelo
rmse_v6 <- RMSE(previsao_v6, esperado_v6)
print(rmse_v6) #RMSE = 89.19


# Avaliando o AUC do modelo
curve_roc_v6 <- multiclass.roc(response = test_scaled$Appliances,
                               predictor = previsao_v6)
class(test_scaled$Appliances)
class(previsao_v6)

accuracy_v6 <- curve_roc_v6$auc
print(accuracy_v6) # Multi-class area under the curve: 0.7427 ou seja; 74,27% de acurácia

# plotando a AUC
png('12-AUC(SVM).png', width = 1000, height = 1000, res = 100 )
plot(roc(test_scaled$Appliances, previsao_v6))
dev.off()

# Combinando e guardando os resultados
model_svm_1 <- 'SVM-1'
results_3 <- data.frame(model_svm_1, rmse_v6, accuracy_v6)
colnames(results_3) <- c('Model', 'RMSE', 'Accuracy')
dfResults <- rbind(dfResults, results_3)
View(dfResults)


# -----> Criaremos uma segunda versão do modelo SVM utilizando Linear Kernel e GridSearch
model_v7_grid1 <- tune(svm,
                       Appliances ~ lights + RH_1 + T2 + RH_2 + T3 + RH_3 + T6 + RH_7 +
                         T8 + RH_8 + T9 + RH_out + Windspeed + Press_mm_hg + RH_6 + RH_5 +
                         RH_9 + RH_4 + T5 + T1 + Tdewpoint + Windspeed + T4 + T_out,
                       dat = train_scaled,
                       
                       data = train_scaled,
                       kernel = 'linear',
                       ranges =list(cost = c(0.05, 0.1, 0.5, 1, 2)))
summary(model_v7_grid1)

# melhores parâmetros do modelo
model_v7_grid1$best.parameters # cost = 2


# Extraindo o melhor modelo
model_v7_grid1$best.model
model_v7_grid <- model_v7_grid1$best.model
summary(model_v7_grid)


# Realizando previsões
test_scaled_pred <-
    test_scaled[,c('Appliances', 'lights', 'RH_1', 'T2', 'RH_2', 'T3', 'RH_3', 'T6',
                   'RH_7', 'T8', 'RH_8', 'T9', 'RH_out', 'Windspeed', 'Press_mm_hg',
                   'RH_6', 'RH_5', 'RH_9', 'RH_4', 'T5', 'T1', 'Tdewpoint', 'T4','T_out')]


previsao_v7 <- round(predict(model_v7_grid, newdata = test_scaled_pred), digits = 0)
esperado_v7 <-as.numeric(test_scaled$Appliances)

# Avaliando o RMSE do modelo
rmse_v7 <- RMSE(previsao_v7, esperado_v7)
print(rmse_v7)  # RMSE = 99.22453


# Avaliando o AUC do modelo
curve_roc_v7 <- multiclass.roc(response = test_scaled$Appliances, predictor = previsao_v7)
class(test_scaled$Appliances)
class(previsao_v7)

accuracy_v7 <- curve_roc_v7$auc
print(accuracy_v7) # Multi-class area under the curve: 0.724 ou seja; 72,4% de acurácia


# plot AUC
png('13-AUC(SVM-1).png', width = 1000, height = 1000, res = 100)
plot(roc(test_scaled$Appliances, previsao_v7))
dev.off()

# Combinando e salvando os resultados
model_svm_2 <- 'SVM-2'
results_4 <- data.frame(model_svm_2, rmse_v7, accuracy_v7)
colnames(results_4) <- c('Model', 'RMSE', 'Accuracy')
dfResults <- rbind(dfResults, results_4)
View(dfResults)

#-----> Terceira versão do modelo SVM - Polynomial Kernel e GridSearch
model_v7_grid2 <- tune(svm,
                       Appliances ~ lights + RH_1 + T2 + RH_2 + T3 + RH_3 + T6 + RH_7 +
                         T8 + RH_8 + T9 + RH_out + Windspeed + Press_mm_hg + RH_6 + RH_5 +
                         RH_9 + RH_4 + T5 + T1 + Tdewpoint + Windspeed + T4 + T_out,
                       dat = train_scaled,
                       
                       data = train_scaled,
                       kernel = 'polynomial',
                       ranges = list(cost = c(1,2), 
                                     degree = c(2,3,4)))
summary(model_v7_grid2)

# Melhores parãmetros do modelo
model_v7_grid2$best.parameters
# cost = 2
# degree = 4


# Extraindo o melhor modelo
model_v7_grid2$best.model
model_v8_grid <- model_v7_grid2$best.model
summary(model_v8_grid)


# Realizando previsões
test_scaled_pred <-
    test_scaled[,c('Appliances', 'lights', 'RH_1', 'T2', 'RH_2', 'T3', 'RH_3', 'T6',
                   'RH_7', 'T8', 'RH_8', 'T9', 'RH_out', 'Windspeed', 'Press_mm_hg',
                   'RH_6', 'RH_5', 'RH_9', 'RH_4', 'T5', 'T1', 'Tdewpoint', 'T4','T_out')]

previsao_v8 <- round(predict(model_v8_grid, newdata =  test_scaled_pred), digits = 0)

esperado_v8 <- as.numeric(test_scaled$Appliances)


# Avaliando o RMSE do modelo
rmse_v8 <- RMSE(previsao_v8, esperado_v8)
print(rmse_v8) # RMSE = 87,85


# Avaliando o AUC do modelo
curve_roc_v8 <- multiclass.roc(response = test_scaled$Appliances, predictor = previsao_v8)
class(test_scaled$Appliances)
class((previsao_v8))


accuracy_v8 <- curve_roc_v8$auc
print(accuracy_v8) # Multi-class area under the curve: 0.7486, ou seja; 74,86% de acurácia


# Plotando AUC
png('14-AUC(SVM-2.png', width = 1000, height = 1000, res =100)
plot(roc(test_scaled$Appliances, previsao_v8))
dev.off()


# combinando os resultados e salvando em um dataframe
model_svm_3 <- 'SVM-3'
results_5 <- data.frame(model_svm_3, rmse_v8, accuracy_v8)
colnames(results_5) <- c('Model', 'RMSE', 'Accuracy')
dfResults <- rbind(dfResults, results_5)
View(dfResults)

# Guardando os resultados finais
write.csv(dfResults, file = 'Final_results.csv')


# plot dos modelos criados anteriormente
png('15-Final_Results(Accuracy).png', width = 900, height = 600, res = 100)
ggplot() +
    geom_bar(data = dfResults,
             aes(x = Model,
                 y = Accuracy),
             stat =  'identity') +
    coord_flip() +
    ggtitle('Modelos x Acurácia')
dev.off()


# Clustering
#----> modelo escolhido : Random Forest - Model_v5
# Realizando Previsões

previsao_RF <- round(predict(model_v5, newdata = test_scaled_pred), digits = 0)
esperado_RF <- as.numeric(test_scaled$Appliances)


# determinando k como objeto de estudo e unindo as previsões criadas pelo random forest model_v5
# para o dataframe

k <- as.data.frame(cbind(previsao_RF, esperado_RF))
colnames(k) <- c('Previsão', 'Real')
View(k)


# Real vs Previsto
ggplot() +
    geom_point(data = k,
               aes(x = Previsão,
                   y = Real)) +
    theme_bw()

ggsave('16-Previsto x Real.png', width = 13, height = 8)


# Criando modelo k para Machine Learning a calculas os clusters
model_k <- kmeans(k$Previsão, 9)
print(model_k)

# verificando o tamanho dos clusters
model_k$size


# Checando o centro dos clusters
model_k$centers

# Map do cluster
k$Cluster <- model_k$cluster

# Map center
lin = 0

for(i in k$Cluster){
    lin = lin + 1
    
    if (i == 1) {k$Center[lin] <- model_k$centers[1]}
    if (i == 2) {k$Center[lin] <- model_k$centers[2]}
    if (i == 3) {k$Center[lin] <- model_k$centers[3]}
    if (i == 4) {k$Center[lin] <- model_k$centers[4]}
    if (i == 5) {k$Center[lin] <- model_k$centers[5]}
    if (i == 6) {k$Center[lin] <- model_k$centers[6]}
    if (i == 7) {k$Center[lin] <- model_k$centers[7]}
    if (i == 8) {k$Center[lin] <- model_k$centers[8]}
    if (i == 9) {k$Center[lin] <- model_k$centers[9]}

}
View(k)

# Range de cada cluster e as distancias entre o centro

ggplot() +
  geom_point(dat = k,
             aes(x = Center,
                 y = previsao_RF,
                 color = as.factor(Cluster)),
             stat = 'identity') +
  theme_bw()

ggsave('17-Cluster Center x Cluster Range.png', width = 12, height = 6)


# Evolução do cluster
ggplot() +
    geom_point(data = k,
               aes(x = previsao_RF,
                   y = previsao_RF,
                   color = as.factor(Cluster)),
               stat = 'identity') + 
  theme_bw()


# Esperado vs Real + Centro dos Clusters
ggplot() +
    geom_point(data = k,
               aes(x = previsao_RF,
                   y = Real)) +
    geom_point(aes(x = model_k$centers[1],
                   y = model_k$centers[1]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
                 
    geom_point(aes(x = model_k$centers[2],
                   y = model_k$centers[2]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
                 
    geom_point(aes(x = model_k$centers[3],
                   y = model_k$centers[3]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
                 
    geom_point(aes(x = model_k$centers[4],
                   y = model_k$centers[4]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
                 
    geom_point(aes(x = model_k$centers[5],
                   y = model_k$centers[5]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
    
    geom_point(aes(x = model_k$centers[6],
                   y = model_k$centers[6]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
    
    geom_point(aes(x = model_k$centers[7],
                   y = model_k$centers[7]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
    
    geom_point(aes(x = model_k$centers[8],
                   y = model_k$centers[8]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
    
    geom_point(aes(x = model_k$centers[9],
                   y = model_k$centers[9]),
                   shape = 18,
                   size = 3.7,
                   colour = "firebrick1") +
    theme_bw()

# Salvando o Plot
ggsave('18-PrevisãoxEsperado(com o centro dos Clusters).png', width = 13, height = 6)



# Cluster de cada grupo de consumo
ggplot() +
    geom_point(data = k, 
               aes(x = previsao_RF,
                   y = Real,
                   color = as.factor(Cluster)),
               stat = 'identity') +
    theme_bw()

ggsave('19-PrevisaoXcluster esperado(centro d cluster).png', width = 13, height = 6)


# Cluster de cada grupo de consumo + Cluster Center
ggplot() +
    geom_point(data = k,
               aes(x = previsao_RF,
                   y = Real,
                   color = as.factor(Cluster)),
               stat = 'identity') +
    
    geom_point(aes(x = model_k$centers[1],
                   y = model_k$centers[1]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
                 
    geom_point(aes(x = model_k$centers[2],
                   y = model_k$centers[2]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
                 
    geom_point(aes(x = model_k$centers[3],
                   y = model_k$centers[3]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
                 
    geom_point(aes(x = model_k$centers[4],
                   y = model_k$centers[4]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
                 
    geom_point(aes(x = model_k$centers[5],
                   y = model_k$centers[5]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
                 
    geom_point(aes(x = model_k$centers[6],
                   y = model_k$centers[6]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
    geom_point(aes(x = model_k$centers[7],
                   y = model_k$centers[7]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
    geom_point(aes(x = model_k$centers[8],
                   y = model_k$centers[8]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
    geom_point(aes(x = model_k$centers[9],
                   y = model_k$centers[9]),
                   shape = 18,
                   size = 3.7,
                   colour = 'firebrick1') +
  theme_bw()


ggsave('20-Previsao x Esperado cluster(Centro do Cluster).png', width = 13, height = 6)


#Clusters Reduzidos
png('21-Cluters reduzidos.png', width = 700, height = 500, res = 100)
autoplot(pam(k[-c(3,4)],4)) +
  theme_bw()
dev.off()


# Considerações finais : Escolhemos o Random Forest pois ele foi o que obteve a melhor
# acurácia, porém o mesmo leva um tempoconsiderável para ser executado, mas iso compensou no final.
