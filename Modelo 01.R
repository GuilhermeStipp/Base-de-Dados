## Título: Inflação e Choque de Expectativas (Modelo 01 - Repasse de IPCA para a Inflação Implícita) ------------------------------------------
## Autor: Guilherme Micota Stipp
## Data: 31/01/2022

## Pacotes Utilizados -------------------------------------------------------------------------------------------------------------------------

library(mise)
library(tidyverse)
library(readxl)
library(zoo)

## Diretório de Trabalho ----------------------------------------------------------------------------------------------------------------------

mise(vars = TRUE, figs = TRUE, console = FALSE)
setwd('D:/OneDrive/4UM Investimentos/Estudos Econômicos/Análises Econômicas/001 - Inflação e Choque de Expectativas')

## Manipulação da Base de Dados ---------------------------------------------------------------------------------------------------------------

base_dados <- as.ts(x = read.zoo(file = tibble(read_xlsx(path = '001 - Inflação e Choque de Expectativas.xlsx', 
                                                         sheet = 'Inflação Implícita')) |>
                        mutate(data = as.Date(x = data)), format = '%Y-%m-%d', FUN = as.yearmon))

dif_base_dados <- diff(x = base_dados)

## Teste de Estacionariedade da Primeira Diferença da Taxa de Inflação ------------------------------------------------------------------------



## Modelos de Regressão Linear de Estimativa de Repasse de IPCA para a Inflação Implícita -----------------------------------------------------

vertice_1_ipca <- lm(formula = lag(x = `1`, n = 1) ~ ipca, data = dif_base_dados)
vertice_2_ipca <- lm(formula = lag(x = `2`, n = 1) ~ ipca, data = dif_base_dados)
vertice_3_ipca <- lm(formula = lag(x = `3`, n = 1) ~ ipca, data = dif_base_dados)
vertice_4_ipca <- lm(formula = lag(x = `4`, n = 1) ~ ipca, data = dif_base_dados)
vertice_5_ipca <- lm(formula = lag(x = `5`, n = 1) ~ ipca, data = dif_base_dados)
vertice_6_ipca <- lm(formula = lag(x = `6`, n = 1) ~ ipca, data = dif_base_dados)
vertice_7_ipca <- lm(formula = lag(x = `7`, n = 1) ~ ipca, data = dif_base_dados)
vertice_8_ipca <- lm(formula = lag(x = `8`, n = 1) ~ ipca, data = dif_base_dados)
vertice_9_ipca <- lm(formula = lag(x = `9`, n = 1) ~ ipca, data = dif_base_dados)
vertice_10_ipca <- lm(formula = lag(x = `10`, n = 1) ~ ipca, data = dif_base_dados)
vertice_11_ipca <- lm(formula = lag(x = `11`, n = 1) ~ ipca, data = dif_base_dados)
vertice_12_ipca <- lm(formula = lag(x = `12`, n = 1) ~ ipca, data = dif_base_dados)
vertice_13_ipca <- lm(formula = lag(x = `13`, n = 1) ~ ipca, data = dif_base_dados)
vertice_14_ipca <- lm(formula = lag(x = `14`, n = 1) ~ ipca, data = dif_base_dados)
vertice_15_ipca <- lm(formula = lag(x = `15`, n = 1) ~ ipca, data = dif_base_dados)

## Salvar Estatísticas dos Modelos de Regressão Linear ----------------------------------------------------------------------------------------

vetor_coeficientes_ipca <- tibble(vertice = seq(from = 1, to = 15, by = 1),
                           coeficiente = c(vertice_1_ipca$coefficients[2], vertice_2_ipca$coefficients[2], vertice_3_ipca$coefficients[2], 
                                          vertice_4_ipca$coefficients[2], vertice_5_ipca$coefficients[2], vertice_6_ipca$coefficients[2], 
                                          vertice_7_ipca$coefficients[2], vertice_8_ipca$coefficients[2], vertice_9_ipca$coefficients[2], 
                                          vertice_10_ipca$coefficients[2], vertice_11_ipca$coefficients[2], vertice_12_ipca$coefficients[2], 
                                          vertice_13_ipca$coefficients[2], vertice_14_ipca$coefficients[2], vertice_15_ipca$coefficients[2]),
                           erro_padrao = c(sqrt(diag(vcov(vertice_1_ipca)))[2], sqrt(diag(vcov(vertice_2_ipca)))[2],
                                           sqrt(diag(vcov(vertice_3_ipca)))[2], sqrt(diag(vcov(vertice_4_ipca)))[2],
                                           sqrt(diag(vcov(vertice_5_ipca)))[2], sqrt(diag(vcov(vertice_6_ipca)))[2],
                                           sqrt(diag(vcov(vertice_7_ipca)))[2], sqrt(diag(vcov(vertice_8_ipca)))[2],
                                           sqrt(diag(vcov(vertice_9_ipca)))[2], sqrt(diag(vcov(vertice_10_ipca)))[2],
                                           sqrt(diag(vcov(vertice_11_ipca)))[2], sqrt(diag(vcov(vertice_12_ipca)))[2],
                                           sqrt(diag(vcov(vertice_13_ipca)))[2], sqrt(diag(vcov(vertice_14_ipca)))[2],
                                           sqrt(diag(vcov(vertice_15_ipca)))[2]))

