![fundo-projeto08](https://user-images.githubusercontent.com/83449614/151662757-c440d585-a619-42cd-8523-1a8e96414ece.png)

# Previsao-Uso-de-Energia

## Modelagem Preditiva em IoT - Previsão de Uso de Energia

### Formação Cientista de Dados - Projeto com Feedback 8 - www.datascienceacademy.com.br

Este projeto de IoT tem como objetivo a criação de modelos preditivos para
a previsão de consumo de energia de eletrodomésticos. Os dados utilizados
incluem medições de sensores de temperatura e umidade de uma rede sem fio,
previsão do tempo de uma estação de um aeroporto e uso de energia utilizada por
luminárias.

Nesse projeto de aprendizado de máquina você deve realizar a filtragem de
dados para remover parâmetros não-preditivos e selecionar os melhores recursos
(melhores features) para previsão.

O conjunto de dados foi coletado por um período de 10 minutos por cerca de 5 meses.
As condições de temperatura e
umidade da casa foram monitoradas com uma rede de sensores sem fio ZigBee.
Cada nó sem fio transmitia as condições de temperatura e umidade em torno
de 3 min. 

Em seguida, a média dos dados foi calculada para períodos de 10 minutos.
Os dados de energia foram registrados a cada 10 minutos com medidores de
energia de barramento m. 

O tempo da estação meteorológica mais próxima do
aeroporto (Aeroporto de Chievres, Bélgica) foi baixado de um conjunto de dados
públicos do Reliable Prognosis (rp5.ru) e mesclado com os conjuntos de dados
experimentais usando a coluna de data e hora.

Duas variáveis aleatórias foram
incluídas no conjunto de dados para testar os modelos de regressão e filtrar os
atributos não preditivos (parâmetros).

Seu trabalho agora é construir um modelo preditivo que possa prever o
consumo de energia com base nos dados de sensores IoT coletados.
Recomendamos usar RandomForest para a seleção de atributos e SVM, Regressão
Logística Multilinear ou Gradient Boosting para o modelo preditivo.
