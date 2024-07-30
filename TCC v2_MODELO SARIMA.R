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

############################################
###             MODELO SARIMA            ###
############################################

# Transformando os dados em séries temporais
custoassistencial_ts <- ts(df_final[5], start = c(2007, 1), end = c(2023, 3), frequency = 4)
plot(custoassistencial_ts)

# Divisão para os treinos e testes

custo_treino = window(custoassistencial_ts, start = c(2007, 1), end = c(2022, 4))
custo_teste = window(custoassistencial_ts, start = c(2023, 1), end = c(2023, 3))
length(custo_teste)

# Plotagem das duas séries para análise

autoplot(custoassistencial_ts) +
  autolayer(custo_treino, series="Treino") +
  autolayer(custo_teste, series="Teste") +
  scale_color_viridis_d() +
  theme_bw()

# Análise do treino

ggtsdisplay(custo_treino)
acf(custo_treino, main="")
pacf(custo_treino, main="")

ggsave("acf.jpg", plot = acf, device = "jpeg", dpi = 600)


# Teste de estacionaridade

testecusto=ur.df(custo_treino)
summary(testecusto)

# A série não é estacionária, portanto deve ser feita a diferenciação

ndiffs(custo_treino)

diff_custo_treino=diff(custo_treino)
ggtsdisplay(diff_custo_treino)

# Teste após a diferenciação
teste_custo_treinodif = ur.df(diff_custo_treino)
summary(teste_custo_treinodif)

# Aplicar o auto ARIMA

arima_custo<-auto.arima(custo_treino, trace = T)
summary(arima_producao)

# Verificação dos resíduos - Teste de Ljung-Box

checkresiduals(arima_custo)

# Normalidade dos resíduos - Teste KS

ks.test(arima_custo$residuals, "pnorm", mean(arima_custo$residuals),
        sd(arima_custo$residuals))

# Previsão dos custos assistenciais

previsao_custo=forecast::forecast(arima_custo, h=9)

forecast::accuracy(previsao_custo, custo_teste)

autoplot(previsao_custo) +
  theme_bw()

