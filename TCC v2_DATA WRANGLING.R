#################################################################################
#                                                                               #
#               Modelagem de Séries Temporais para Previsão de                  #
#                  Sinistralidade em uma Operadora de Saúde                     #
#                                                                               #
#################################################################################

# INSTALAÇÃO E CARREGAMENTO DOS PACOTES QUE SERÃO UTILIZADOS

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate", 
             "openxlsx", "writexl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}


######################################
## PRIMEIRA PARTE - DATA WRANGLING ###
######################################


##### CARREGAMENTO E LIMPEZA DE DADOS DOS CUSTOS #####


# Para agilizar o processo de carga e limpeza dos dados, pois trata-se de mais de 40 milhões de linhas, 
# foram excluídas as colunas conforme abaixo:
# 2017: Coluna 'Data da Carga'
# 2022 a 2023: Coluna 'VL_SALDO_INICIAL'

# Caminho dos dados
caminho_diretorio <- "C:/Users/hanrs/iCloudDrive/TCC/_TCC v2/Dados_ANS/Dados"

# Lista todos os arquivos .csv no diretório
lista_arquivos <- list.files(path = caminho_diretorio, pattern = "\\.csv$", full.names = TRUE)

# Lista vazia para armazenar os dados
lista_dados <- list()

# Lê o primeiro arquivo para obter a estrutura de colunas
primeiro_arquivo <- read.csv(lista_arquivos[1], header = TRUE, sep = ";")
num_colunas <- ncol(primeiro_arquivo)

# Loop sobre cada arquivo na lista de arquivos
for (arquivo in lista_arquivos) {
  dados <- read.csv(arquivo, header = TRUE, sep = ";")
  # Verifica se o número de colunas é consistente para tratamento no excel
  if (ncol(dados) != num_colunas) {
    stop("Os arquivos têm números diferentes de colunas.")
  }
  
# Adiciona os dados à lista
  lista_dados[[arquivo]] <- dados
}

# Combina todos os dados em um único conjunto de dados
dados_combinados <- do.call(rbind, lista_dados)

# Remove o caminho do diretório dos nomes das colunas
nomes_colunas_sem_caminho <- basename(colnames(dados_combinados))

# Atribui os novos nomes de colunas ao dataframe combinado
colnames(dados_combinados) <- nomes_colunas_sem_caminho


##### FILTRO DA PRESTADORA DE SÁUDE E CONTA CONTÁBIL DE SINISTRALIDADE #####	

# Filtros utilizados:
# 335592 - Prestadora de saúde analisada
# 41 - Conta contábil (EVENTOS INDENIZÁVEIS LÍQUIDOS - SINISTROS RETIDOS)

# Combinação de filtro
filtro <- dados_combinados$REG_ANS == 335592 & dados_combinados$CD_CONTA_CONTABIL == 41

df_prestadora <- dados_combinados[filtro, ]

df_prestadora <- na.omit(df_prestadora)

# Filtro das colunas que serão utilizadas

df_prestadora <- df_prestadora[, c("DATA", "VL_SALDO_FINAL")]

# Convertendo as colunas que estão no formato chacarter
df_prestadora$DATA <- as.Date(df_prestadora$DATA, format="%d/%m/%Y")
df_prestadora$VL_SALDO_FINAL <- as.numeric(gsub(",", ".", df_prestadora$VL_SALDO_FINAL))

# Os dados recolhidos são os acumulados nos trimestres. O código abaixo vai dimuir um mês do outro para 
# identificar qual o valor do trimestre

# Ordenar o dataframe por data
df_prestadora <- df_prestadora %>% arrange(DATA)

# Coluna que identifica o início de um novo ano com TRUE ou FALSE
df_prestadora <- df_prestadora %>% mutate(INICIO_ANO = month(DATA) == 1 & day(DATA) == 1)

# Calcular a diferença de saldo para cada trimestre, exceto para o primeiro de cada ano
df_prestadora <- df_prestadora %>%
  mutate(VL_SALDO_ANTERIOR = ifelse(INICIO_ANO, VL_SALDO_FINAL, lag(VL_SALDO_FINAL))) %>%
  mutate(SALDO_TRIMESTRAL = VL_SALDO_FINAL - VL_SALDO_ANTERIOR)

# Para o primeiro trimestre de cada ano, ajustar a diferença para o valor do trimestre
df_prestadora <- df_prestadora %>%
  mutate(SALDO_TRIMESTRAL = ifelse(INICIO_ANO, VL_SALDO_FINAL, SALDO_TRIMESTRAL))

# Salvando dataframe em excel para agilizar a utilização futura

write_xlsx(df_prestadora, "df_prestadora.xlsx")

