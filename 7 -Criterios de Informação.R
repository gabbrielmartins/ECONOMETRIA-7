                       #Aula 7 - Critérios de Informação

library(readxl)                                                        #Carrega os pacotes
variacao_PIB <- read.table("c:/Econometria/Variacao.xls", header = T)  #Carrega os arquivos
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Cria a serie temporal var_PIB
View(var_PIB)
plot(var_PIB, main="Variacao do PIB Brasileiro", col="Blue", ylab="%PIB", xlab="Ano")

acf(var_PIB)         #Cria a FAC -Função de Autocorrelação (ACF)
pacf(var_PIB)        #cria a FACP - Funçao de Autocorrelação Parcial (PACF)

AR1 <- arima(var_PIB, order = c(1,0,0))   #Estima um modelo autoreressivo de ordem p=1 ,AR(1)
MA1 <- arima(var_PIB, order = c(0,0,1))   #Estima um modelo de médias móveis ordem q=1 , MA(1)
ARMA11 <- arima(var_PIB, order = c(1,0,1))#Estima um modelo autoregressivo de médias móveis ordem p=1 e q=1 ARMA(1,1)

AIC(AR1) #Extrai a estatística AIC do modelo AR1
BIC(AR1) #Extrai a estatística BIC Ddo modelo AR1


AR2 <- arima(var_PIB, order = c(2,0,0))

MA2 <- arima(var_PIB, order = c(0,0,2))
MA3 <- arima(var_PIB, order = c(0,0,3))
MA4
MA5
MA6
MA7
MA8
MA9

ARMA12
ARMA13
ARMA14
ARMA15
ARMA16
ARMA17
ARMA18
ARMA19

ARMA21
ARMA22
ARMA23
ARMA24
ARMA25
ARMA26
ARMA27
ARMA28
ARMA29

#Exemplo aplicação múltipla - Extra (Deve-se completar as estimações antes de executar esse código)

estimacoes <- list(AR1, AR2, MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8, MA9, 
                   ARMA11,ARMA12, ARMA13, ARMA14,ARMA15, ARMA16,ARMA17,ARMA18,ARMA19,
                   ARMA21,ARMA22,ARMA23,ARMA24,ARMA25,ARMA26,ARMA27,ARMA28,ARMA29)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

#Exemplo de criação de tabela com resultados - Extra
AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("AR1", "AR2", "MA1", "MA2", "MA3", "MA4", "MA5", "MA6", "MA7", "MA8", "MA9", "ARMA11","ARMA12", "ARMA13", "ARMA14","ARMA15", "ARMA16","ARMA17","ARMA18","ARMA19","ARMA21","ARMA22","ARMA23","ARMA24","ARMA25","ARMA26","ARMA27","ARMA28","ARMA29")

Resultados <- data.frame(Modelo, AIC, BIC)
View(Resultados)
