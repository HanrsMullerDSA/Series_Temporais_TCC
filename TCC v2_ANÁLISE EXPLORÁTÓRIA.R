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

###########################################
## SEGUNDA PARTE - ANÁLISE EXPLORÁTÓRIA ###
###########################################

# Carga do dataframe
df_final <- read.xlsx("df_prestadora.xlsx")

# Análise exploratória
summary(df_final$SALDO_TRIMESTRAL)
sd(df_final$SALDO_TRIMESTRAL)

# Gráfico para analisar as tendências
grafico01 <- ggplot(df_final, aes(x = DATA, y = SALDO_TRIMESTRAL / 1e6)) + 
  geom_line() +
  labs(x = "Data", y = "Sinistralidade (em milhões)") + 
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(
    axis.line = element_line(color = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank()  
  )

plot(grafico01)

ggsave("custos_ao_longo_do_tempo.jpg", plot = grafico01, device = "jpeg", dpi = 600)

