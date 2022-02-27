####################################################################
###################### PROJETO DE DATA SCIENCE #####################
######## UMA MÉTRICA POSSÍVEL PARA A TAXA DE JUROS NATURAL #########
############# BRASILEIRA E A POSTURA DO BANCO CENTRAL ##############
####################################################################


## Carregando os pacotes básicos
library(tidyverse)
library(dplyr)
library(lubridate)

###################### OBTENDO DADOS DO FED (FED Funds) #####################

library(httr)
endpoint = "series/observations"
params = list (
  api_key = "ca4c6990bb4ed3ccc2a5f133fc08b82e", ## utilizei minha key
  file_type = "json",
  series_id = "FEDFUNDS"
)


fred =
  httr::GET(
    url = "https://api.stlouisfed.org/", ## Base URL
    path = paste0("fred/", endpoint), ## O API endpoint
    query = params ## Nossa lista de parâmetros
  )


fred =
  fred %>%
  httr::content("text") %>% ## Extract the reponse content (i.e. text )
  jsonlite::fromJSON() ## Convert from JSON to R object


fred =
  fred %>%
  purrr::pluck("observations") %>% ## Extract the "$ observations " list element
  # .$ observations %>% ## I could also have used this
  # magrittr :: extract (" observations ") %>% ## Or this
  as_tibble() ## Just for nice formatting

fred

## covertendo as colunas chr para date e numeric
library(lubridate)
fred$date = ymd(fred$date)
class(fred$date)

fred$value = as.numeric(fred$value) 
class(fred$value)
fred

## excluindo colunas desnecessárias
library(dplyr)
fred = fred %>%
  select(date, value)
fred

## coletando somente as observações a partir de jan-2005
fred = fred %>%
  slice(607:n()) ## a 607ª linha corresponde a jan-2005 no dataframe

fred = rename(fred, Data = date)
fred

############### OBTENDO DADOS DO IPEADATA (EMBI+) ########################

## instalação do pacote
install.packages("ipeadatar")

## extrair tabela com todas as séries e códigos disponíveis
series_ipeadata = ipeadatar::available_series()

## filtrar séries com o termo "embi"
dplyr::filter(
  series_ipeadata,
  stringr::str_detect(source, stringr::regex("embi", ignore_case = TRUE))
)

## coletar a série de interesse usando o código
dados_ipeadata = ipeadatar::ipeadata("JPM366_EMBI366")

head(dados_ipeadata)
tail(dados_ipeadata)
class(dados_ipeadata$value)
class(dados_ipeadata$date)

## Coletando somente as observações a partir de jan-2005
dados_ipeadata = dados_ipeadata %>%
  slice(2714:n())
head(dados_ipeadata)

## Calculando a mediana mensal do EMBI+

dados_ipeadata = dados_ipeadata %>%
  mutate(mes = month(date), ano = year(date))
head(dados_ipeadata)

embi_mensal = dados_ipeadata %>%
  group_by(ano, mes) %>%
  summarise(mediana_embi = median(value))
embi_mensal


################## OBTENDO DADOS DO BCB ##########################

## Instalação do pacote para obtenção dos dados
install.packages("GetBCBData")

## Dados da meta Selic
selic = GetBCBData::gbcbd_get_series(
  id = 432,                     # código da série SGS/BCB
  first.date = "2005-01-01",    # filtro de período
  last.date = Sys.Date()
)

tail(selic)

## Cálculo da média da meta anual da Selic (periodicidade mensal)
class(selic$ref.date)

selic = selic %>%
  mutate(mes = month(ref.date), ano = year(ref.date))
head(selic)

selic_mensal = selic %>%
  group_by(ano, mes) %>%
  summarise(selic_media = mean(value))
selic_mensal

## Dados das expectativas de inflação para os 12 meses seguintes
library(rbcb)
inflacao_esper = get_market_expectations("inflation-12-months",
                                         "IPCA", 
                                         start_date = "2005-01-01", 
                                         end_date = "2022-02-25")
tail(inflacao_esper)
head(inflacao_esper)
class(inflacao_esper$Data)
class(inflacao_esper$Mediana)

## Excluir linhas do dataframe (expect. não-suavizadas)
inflacao_esper = inflacao_esper[!grepl("N", inflacao_esper$Suavizada),]

## Cálculo das medianas mensais das expectativas de inflação para os 12
## meses seguintes
inflacao_esper = inflacao_esper %>%
  mutate(mes = month(Data), ano = year(Data))

expec_infl = inflacao_esper %>%
  group_by(ano, mes) %>%
  summarise(exp_inf = median(Mediana))
expec_infl


################## Obtendo dados da B3 ########################
## CUPOM CAMBIAL LIMPO mensal - valor no 15º dia do mês (ou dia
## útil mais próximo) para o 360º dia corrido (ou dia mais
## próximo disponível).
## FONTE: TAXAS REFERENCIAIS BM&FBOVESPA
library(readxl)
url <- "https://docs.google.com/spreadsheets/d/16twmYdjNRu2YeOQt1v65FoCIfOQSLipm/edit#gid=93104422"
destfile <- "edit_gid_93104422.xls"
curl::curl_download(url, destfile)
edit_gid_93104422 <- read_excel(destfile)
View(edit_gid_93104422)

## Mudando as classes de character para data e numérica
class(cupom_cambial_limpo$Data)
cupom_cambial_limpo$Data = lubridate::ym(cupom_cambial_limpo$Data)
is.Date(cupom_cambial_limpo$Data)

class(cupom_cambial_limpo$Cupom)
cupom_cambial_limpo$Cupom = as.numeric(cupom_cambial_limpo$Cupom)
is.numeric(cupom_cambial_limpo$Cupom)

################### Construindo o dataframe ##########################
library(tidyverse)
embi_mensal = embi_mensal %>% select(mediana_embi, ano, mes) %>%
  mutate(Data = make_date(ano, mes))
embi_mensal

expec_infl = expec_infl %>% select(exp_inf, ano, mes) %>%
  mutate(Data = make_date(ano, mes))
expec_infl

selic_mensal = selic_mensal %>% select(selic_media, ano, mes) %>%
  mutate(Data = make_date(ano, mes))
selic_mensal

## Formatando as datas
fred$Data = format(as.Date(fred$Data), "%Y-%m")
selic_mensal$Data = format(as.Date(selic_mensal$Data), "%Y-%m")
expec_infl$Data = format(as.Date(expec_infl$Data), "%Y-%m")
embi_mensal$Data = format(as.Date(embi_mensal$Data), "%Y-%m")
cupom_cambial_limpo$Data = format(as.Date(cupom_cambial_limpo$Data), "%Y-%m")

## Unindo os dataframes criados a partir da coluna de datas
dados1 = merge(embi_mensal, 
               cupom_cambial_limpo, by = "Data")
dados2 = merge(fred, selic_mensal, by = "Data")
dados3 = merge(dados1, dados2, by = "Data")
dados = merge(expec_infl, dados3, by = "Data")


################### Plotando o gráfico #######################
## Vamos definir algumas operações entre colunas

## A soma dos valores da taxa efetiva do FED, dos pontos-base EMBI+ e do
## cupom cambial limpo consistem na taxa natural de juros para o Brasil,
## pela metodologia de economia aberta. Tratamos essa soma com um Filtro HP
## para extrair a tendência de longo prazo para a taxa de juros
dados = dados %>%
  group_by("Data") %>%
  mutate(juro_nat = value + Cupom + (mediana_embi/100)) %>%
  glimpse()

## A meta da taxa de juros real consiste na diferença entre a meta para a
## Selic e as expectativas de inflação para os próximos 12 meses
dados = dados %>%
  group_by("Data") %>%
  mutate(juro_real = selic_media - exp_inf)

is.Date(dados$Data)
dados$Data = lubridate::ym(dados$Data)
is.Date(dados$Data)

## Extração da tendência de longo prazo da taxa de juros 
install.packages("mFilter")
library(mFilter)

## Definimos o valor de lambda em 14400 por ser mais adequado à
## periodicidade mensal
hpf = hpfilter(dados$juro_nat, freq = 14400, type = "lambda")


#################### Plotando o gráfico 1 ##########################
library(ggplot2)
ggplot(dados, aes(x = Data, y = juro_real)) +
  geom_line(aes(col = "Taxa real de juros"), lwd = 1.2) +
  geom_line(aes(y = hpf$trend, col = "Taxa natural de juros"), lwd = 1.2) +
  theme_bw() +
  labs(x = "Ano", y = "Taxa (%)", colour = NULL) +
  theme(legend.position = "top")


###################################################################
## Podemos definir um diferencial de juros entre a meta de juros real
## do BCB e a taxa natural definida pela tendência extraída pelo
## Filtro HP
dados = dados %>%
  group_by("Data") %>%
  mutate(diff_juros = juro_real - hpf$trend)

## Definindo uma nova variável
dados = dados %>%
  mutate(diferencial_juros = ifelse(diff_juros>0,
                                    "Positivo", "Negativo"))


################### Plotando o gráfico 2 ###########################
ggplot(dados, aes(x = Data, y = diff_juros)) +
  geom_segment(aes(x = Data, xend = Data, y = 0, yend = diff_juros, 
                   color = diferencial_juros), size = 0.7, alpha = 1) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Data") +
  ylab("Diferencial de juros")


###################################################################
## Finalmente, vamos organizar o dataframe

dados = dados[,-c(3,4,6,7,11:13, 17)]
head(dados)
write.csv(dados, "projeto_joao_paulo.csv")

####################################################################
####################################################################
####################################################################