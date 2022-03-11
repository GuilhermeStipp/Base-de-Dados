## Título: Inflação e Choque de Expectativas (Modelo 02 - Estimativas de Componentes da Inflação Implícita e Significância Econométrica) -----
## Autor: Guilherme Micota Stipp
## Data: 22/02/2022

## Pacotes Utilizados ------------------------------------------------------------------------------------------------------------------------

library(mise)
library(tidyverse)
library(readxl)
library(mFilter)

## Diretório de Trabalho ---------------------------------------------------------------------------------------------------------------------

mise(vars = TRUE, figs = TRUE, console = FALSE)
setwd(dir = 'D:/OneDrive/4UM Investimentos/Estudos Econômicos/Análises Econômicas/001 - Inflação e Choque de Expectativas')

## Setup de Informações ----------------------------------------------------------------------------------------------------------------------

data_inicial <- as.Date('2005-02-01')
data_final <- as.Date('2022-02-01')
num_obs <- as.numeric(length(seq(from = data_inicial, to = data_final, by = 'months')))
num_euler <- (1 + 1 / 1E+10) ^ 1E+10
lambda <- 0.8
beta1_inflacao <- as.numeric()
beta2_inflacao <- as.numeric()
beta3_inflacao <- as.numeric()
i <- 0
j <- 0
k <- 0

## Manipulação da Base de Dados --------------------------------------------------------------------------------------------------------------

inflacao_efetiva <- tibble(read_xlsx(path = '001 - Inflação e Choque de Expectativas.xlsx', sheet = 'Inflação Implícita')) |> 
  select(!ipca) |> 
  pivot_longer(cols = !c(data), names_to = 'vertice', values_to = 'taxas') |> 
  mutate(data = as.Date(data),
         vertice = as.numeric(vertice), 
         nivel = 1,
         inclinacao = (1 - (num_euler ^ (- lambda * vertice))) / (lambda * vertice),
         curvatura = (1 - (num_euler ^ (- lambda * vertice))) / (lambda * vertice) - (num_euler ^ (- lambda * vertice)))

## Extração dos Três Componentes da Inflação Implícita (Nível, Inclinação e Curvatura) -------------------------------------------------------

for (x in seq(from = data_inicial, to = data_final, by = 'month')) {
  reg = lm(formula = taxas ~ inclinacao + curvatura, data = inflacao_efetiva |> filter(data == x))
  i = i + 1
  beta1_inflacao[i] = reg$coefficients[1]
}

for (x in seq(from = data_inicial, to = data_final, by = 'month')) {
  reg = lm(formula = taxas ~ inclinacao + curvatura, data = inflacao_efetiva |> filter(data == x))
  j = j + 1
  beta2_inflacao[j] = reg$coefficients[2]
}

for (x in seq(from = data_inicial, to = data_final, by = 'month')) {
  reg = lm(formula = taxas ~ inclinacao + curvatura, data = inflacao_efetiva |> filter(data == x))
  k = k + 1
  beta3_inflacao[k] = reg$coefficients[3]
}

componentes_inflacao <- tibble(data = seq(from = data_inicial, to = data_final, by = 'months')) |> 
  mutate(beta1 = beta1_inflacao, beta2 = beta2_inflacao, beta3 = beta3_inflacao)

componentes_inflacao_dif <- componentes_inflacao |>
  mutate(beta1 = beta1 - lag(x = beta1, n = 1),
         beta2 = beta2 - lag(x = beta2, n = 1),
         beta3 = beta3 - lag(x = beta3, n = 1))

## Estimação da Inflação Implícita a Partir dos Coeficientes Estimados -----------------------------------------------------------------------

inflacao_ajustada <- full_join(x = inflacao_efetiva, y = componentes_inflacao) |> 
  mutate(taxas = ((nivel * beta1) + (inclinacao * beta2) + (curvatura * beta3))) |> 
  select(data, vertice, taxas)

## Agrupar os Dados Efetivos e Ajustados das Curvas Nominais, Reais de Juros e Inflação Implícita --------------------------------------------

inflacao_agrupada <- tibble(data = inflacao_efetiva$data,
                            vertice = inflacao_efetiva$vertice,
                            efetiva = inflacao_efetiva$taxas,
                            ajustada = inflacao_ajustada$taxas)

## Cálculo do Erro Médio do Modelo de Ajuste da Curva Nominal e Real de Juros ----------------------------------------------------------------

erro_medio_inflacao <- inflacao_agrupada |> 
  mutate(erro = abs(ajustada - efetiva)) |> 
  select(data, vertice, erro) |> 
  group_by(vertice) |> 
  summarise(total = sum(erro) / num_obs)

## Excluir Variáveis Secundárias -------------------------------------------------------------------------------------------------------------

rm(x, num_obs, num_euler, lambda, i, j, k, data_inicial, data_final, beta1_inflacao, beta2_inflacao, beta3_inflacao, reg, inflacao_ajustada,
   inflacao_efetiva)

## Relação entre o IPCA e os Coeficientes Estimados da Inflação Implícita --------------------------------------------------------------------

componentes_inflacao_2 <- componentes_inflacao |> 
  mutate(ipca = tibble(read_xlsx(path = '001 - Inflação e Choque de Expectativas.xlsx', sheet = 'Inflação Implícita')) |> 
                select(ipca))

componentes_inflacao_2_diff <- componentes_inflacao_2 |> 
  mutate(beta1 = beta1 - lag(x = beta1, n = 1),
         beta2 = beta2 - lag(x = beta2, n = 1),
         beta3 = beta3 - lag(x = beta3, n = 1),
         ipca = ipca - lag(x = ipca, n = 1))

modelo_nivel <- lm(formula = ts(data = componentes_inflacao_2_diff$beta1, start = c(2005, 02), frequency = 12) ~ 
                             ts(data = componentes_inflacao_2_diff$ipca, start = c(2005, 02), frequency = 12), 
                   data = componentes_inflacao_2_diff)

modelo_inclinacao <- lm(formula = ts(data = componentes_inflacao_2_diff$beta2, start = c(2005, 02), frequency = 12) ~ 
                                  ts(data = componentes_inflacao_2_diff$ipca, start = c(2005, 02), frequency = 12), 
                        data = componentes_inflacao_2_diff)

modelo_curvatura <- lm(formula = ts(data = componentes_inflacao_2_diff$beta3, start = c(2005, 02), frequency = 12) ~ 
                                 ts(data = componentes_inflacao_2_diff$ipca, start = c(2005, 02), frequency = 12), 
                       data = componentes_inflacao_2_diff)



## Decomposição dos Componentes Estimados da Inflação implícita ------------------------------------------------------------------------------







