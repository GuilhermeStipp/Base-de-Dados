## Título: Estrutura a Termo da Taxa de Juros (EUA) ------------------------------------------------------------------------------------------
## Autor: Guilherme Micota Stipp
## Data: 21/02/2022

## Pacotes Utilizados ------------------------------------------------------------------------------------------------------------------------

library(mise)
library(tidyverse)
library(fredr)
library(extrafont)

## Diretório de Trabalho ---------------------------------------------------------------------------------------------------------------------

mise(vars = TRUE, figs = TRUE, console = FALSE)
setwd(dir = 'D:/OneDrive/Economia/Stipp Group/Banco de Dados/Dados Internacionais/Estrutura a Termo da Taxa de Juros (EUA)')

## Setup de Informações ----------------------------------------------------------------------------------------------------------------------

fredr_set_key('3f0cd0cde1ba1d586622f2a3b2c801cb')
data_inicial <- as.Date('2011-01-01')
vetor_id_nominal <- c('DGS1MO', 'DGS3MO', 'DGS6MO', 'DGS1', 'DGS2', 'DGS3', 'DGS5', 'DGS7', 'DGS10', 'DGS20', 'DGS30')
vetor_id_real <- c('DFII5', 'DFII7', 'DFII10', 'DFII20', 'DFII30')
j <- 2
k <- 2

## Manipulação da Base de Dados --------------------------------------------------------------------------------------------------------------

juro_nominal <- tibble(data = fredr(series_id = 'DGS1MO', observation_start = data_inicial)$date)
juro_real <- tibble(data = fredr(series_id = 'DFII5', observation_start = data_inicial)$date)

for (i in vetor_id_nominal) {
  x <- fredr(series_id = i, observation_start = data_inicial)$value
  juro_nominal[,j] <- x
  j <- j + 1
}

for (i in vetor_id_real) {
  y <- fredr(series_id = i, observation_start = data_inicial)$value
  juro_real[,k] <- y
  k <- k + 1
}

juro_nominal <- juro_nominal |> 
  rename(`1` = ...2,
         `3` = ...3,
         `6` = ...4,
         `12` = ...5,
         `24` = ...6,
         `36` = ...7,
         `60` = ...8,
         `84` = ...9,
         `120` = ...10,
         `240` = ...11,
         `360` = ...12) |> 
  na.omit()

juro_real <- juro_real |> 
  rename(`60` = ...2,
         `84` = ...3,
         `120` = ...4,
         `240` = ...5,
         `360` = ...6) |> 
  na.omit()

inflacao_implicita <- tibble(data = juro_nominal$data) |> 
  mutate(`60` = ((juro_nominal$`60` / 100 + 1) / (juro_real$`60` / 100 + 1) - 1) * 100,
         `84` = ((juro_nominal$`84` / 100 + 1) / (juro_real$`84` / 100 + 1) - 1) * 100,
         `120` = ((juro_nominal$`120` / 100 + 1) / (juro_real$`120` / 100 + 1) - 1) * 100,
         `240` = ((juro_nominal$`240` / 100 + 1) / (juro_real$`240` / 100 + 1) - 1) * 100,
         `360` = ((juro_nominal$`360` / 100 + 1) / (juro_real$`360` / 100 + 1) - 1) * 100) 

juro_nominal <- juro_nominal |> 
  pivot_longer(cols = !c(data), names_to = 'vertice', values_to = 'valor') |> 
  mutate(variavel = 'Juro Nominal')

juro_real <- juro_real |> 
  pivot_longer(cols = !c(data), names_to = 'vertice', values_to = 'valor') |> 
  mutate(variavel = 'Juro Real')

inflacao_implicita <- inflacao_implicita |> 
  pivot_longer(cols = !c(data), names_to = 'vertice', values_to = 'valor') |> 
  mutate(variavel = 'Inflação Implícita')

ettj <- inflacao_implicita |> 
  full_join(x = juro_real) |> 
  full_join(x = juro_nominal) |> 
  mutate(vertice = as.numeric(vertice))

rm(data_inicial, i, j, vetor_id_nominal, x, vetor_id_real, y, k, juro_nominal, juro_real, inflacao_implicita)

## Visualização da Base de Dados -------------------------------------------------------------------------------------------------------------

ggplot(data = ettj |> filter(data %in% c(as.Date(x = '2021-12-31'), as.Date(x = '2022-02-28'), as.Date(x = '2022-03-31')))) +
  ggthemes::theme_par(base_size = 12, base_family = 'Times New Roman') + facet_wrap(facets = ~ variavel) +
  geom_line(mapping = aes(x = vertice / 12, y = valor, color = as.character(data)), size = 1.2) +
  scale_y_continuous(name = 'Taxa Anual (%)', n.breaks = 10) +
  scale_x_continuous(name = 'Anos', breaks = c(1, 2, 3, 5, 7, 10, 20, 30)) +
  labs(title = 'Estrutura da Curva de Juros Nominal dos EUA', color = NULL) +
  theme(legend.position = 'bottom')
