          #Critérios de Informação - Tabela dos Resultados das Estimações Por Meio de Loop

library(readxl)                                                        #Carrega os pacotes
variacao_PIB <- read.table("c:/Econometria/variacao.xls", header = T)  #Carrega os arquivos
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Cria a serie temporal var_PIB

#Estimando Regressões e Tabelando Resultados

est1 <- data.frame()
      for (i in 1:2) {                      #Loop para os AR: ARIMA(i,0,0)
        est1[i,1] <- paste("AR",i)      #Coluna com os nomes do Modelo
        est1[i,2] <- AIC(arima(var_PIB,  order = c(i,0,0)))  #Coluna com valores AIC
        est1[i,3] <- BIC(arima(var_PIB,  order = c(i,0,0)))  #Coluna com valores BIC
  }

est2 <- data.frame()                        #Loop para os MA: ARIMA(0,0,i)
        for (i in 1:9) {
          est2[i,1] <- paste("MA",i) 
          est2[i,2] <- AIC(arima(var_PIB,  order = c(0,0,i)))
          est2[i,3] <- BIC(arima(var_PIB,  order = c(0,0,i)))
      
    }

est3 <- data.frame()                        #Loop para OS ARMA p=1: ARIMA(1,0,i)
        for (i in 1:9) {
          est3[i,1] <- paste("ARMA",1,0,i) 
          est3[i,2] <- AIC(arima(var_PIB,  order = c(1,0,i)))
          est3[i,3] <- BIC(arima(var_PIB,  order = c(1,0,i)))
  
}

est4 <- data.frame()                       #Loop para OS ARMA p=2: ARIMA:(2,0,i)
         for (i in 1:9) {
             est4[i,1] <- paste("ARMA",2,0,i) 
             est4[i,2] <- AIC(arima(var_PIB,  order = c(2,0,i)))
             est4[i,3] <- BIC(arima(var_PIB,  order = c(2,0,i)))
         
       }
  
Resultados <- data.frame(rbind(est1,est2,est3,est4))  
colnames(Resultados) <- c("Modelo","AIC","BIC")

View(Resultados)

write.table(Resultados, "c:/Econometria/ResultadosAICeBIC.csv") #Grava arquivo com resultados em formato csv