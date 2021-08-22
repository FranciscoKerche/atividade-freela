#### Atividade Freela ####

# Autor: Francisco W. Kerche
# Data: 18 de Agosto de 2021

#### 0. Abrir pacotes necessários ---------------------------------------------

pacman::p_load(tidyverse, #Para trabalhar com bases de dados
               bigrquery, #Para importação do big query
               DBI, #Para conectar com big query
               httr, #pegar dados da api
               jsonlite, #Para lidar com o formato da API
               rio) #Para importar arquivos


#### Exercício 1 - Importação de dados da câmara ---------------------------------------------

#1. Importação e limpeza
#1.1 - apresentar o url da api que vai ser extraída

url_deputados <- "https://dadosabertos.camara.leg.br/api/v2/deputados"

#1.2 - Utilizar a função GET para pegar os valores da API.
  #Como queremos todos os dados disponíveis, apenas selecionei que os
  #resultados sejam devolvidos em ordem alfabética

raw_dep <- GET(url_deputados,
               query = list(ordenarPor = 'nome',
                            ordem = 'asc'))

#1.3 - Utilizar o pacote jsonlite para ler os dados

json_dep <- content(raw_dep, as = 'text') %>%
  fromJSON()

#1.4 - Transformar os dados sobre os deputados em uma tibble

deputados <- tibble(json_dep$dados)

#1.5 - Exportar como excel

write_excel_csv(deputados, 'resposta-exercicio-1.csv')

#2. Análise:
#2.1 - conte o número de deputados por partidos

deputados %>%
  count(siglaPartido, sort = T) %>%
  select(partido = siglaPartido, `numero de deputados` = n)

#2.2 - conte o número de deputados por Estado

deputados %>%
  count(siglaUf, sort = T) %>%
  select(Estado = siglaUf, `numero de deputados` = n)

#### Exercício 2 - Importação de dados com Big Query --------------------------

#1 - Importar base de dados

#1.1 - Conectar com meu e-mail
bigrquery::bq_auth("chicowkn@gmail.com")

#1.2 - Separar qual base de dados que vai ser importada
exercicio_2 <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "google_political_ads",
  billing = 'testecanais'
)

dbListTables(exercicio_2)

#1.3 - Como vamos usar todas as colunas, vou importar diretamente
g_ads <- dbGetQuery(exercicio_2, 'SELECT * FROM top_keywords_history')


#1.4 - Fazer o pivot para pegar apenas duas colunas, uma para as keywords outras para o valor gasto
final_ads <- g_ads %>%
  pivot_longer(keyword_1:spend_usd_6, #Pivotar todas as colunas de keyword 1 até spend usd 6
               names_to = '.value',
               names_pattern = '(.+).+') %>% #Dessa forma ele compreende que pelo padrão ser similar, deve considerar duas colunas
  rename(keyword = keyword_, spend_usd = spend_usd_) #Colocar o nome desejado

write_excel_csv(final_ads, 'resposta-exercicio-2.csv') #Transformar em csv


# OBS: (enviei um e-mail a propósito) A tabela está diferente da esperada na ficha da atividade.
# O data-set modelo, que é apresentado com 2340 linhas não deveria poder ter repetições do report_date para a mesma keyword.
# Considerando que o report_date é o valor chave da tabela, a relação entre keyword e spend_usd tem que ser única
# para cada report_date. Isso acontece, pois os valores estão repetidos, entre as primeiras linhas,
# Elizabeth Warren tem, de fato, 1.491.100 no dia 02 de Dezembro. Porém quem tem 1.403.700 é Pete Buttigieg e
# 964.900, é Kamala Harris, assim por diante.
# Ou seja, uma vez que o data-set original tem apenas de 65 linhas a expansão vai multiplicá-lo por seis
# (seis keywords para cada dia), será encontrado um data-set final de apenas 390 linhas, não 2340.



#EXTRA: importar base diretamente do CSV para não ser necessário raspar novamente:

#retire o # e baixe diretamente:
#final_ads <- import('resposta-exercicio-2.csv', setclass = 'tibble')

# 1.5 - Checar quem teve o maior gasto médio

final_ads %>%
  group_by(keyword) %>% #Pegar o valor para cada keyword
  summarise(gasto_medio = mean(spend_usd)) %>% #retirar a média de gasto
  arrange(-gasto_medio) #ordenar do maior gasto médio para o menor

#Quem teve o maior gasto médio foi Pete Buttigieg

#Quem teve o maior gasto no período foi elizabeth warren, uma vez que a tabela é cumulativa

