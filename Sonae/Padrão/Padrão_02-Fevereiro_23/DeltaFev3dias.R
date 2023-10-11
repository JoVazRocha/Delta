library(tidyverse)
library(readxl)

# Ler ficheiros
fileNinjas1 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brand and Ninjas/Fevereiro/Resultado Sonae_fevereiro 23.xlsx")
fileStocks <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brand and Ninjas/Fevereiro/Stocks_fev.xlsx", sheet = "Sheet1")
fileSellouts1 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brand and Ninjas/Fevereiro/SellOut_fev.xlsx")



#Renomear coisas e pôr em datetime o que não está

fileNinjas <- fileNinjas1 %>%
  rename('DATA' = 'Data de Resposta', "STORE"="Código da loja")
fileNinjas$DATA <- as.POSIXct(fileNinjas$DATA, format = "%d-%m-%Y", tx = "GMT")

fileStocks$DATA <- as.POSIXct(fileStocks$DATA, format = "%d-%m-%Y", tx = "GMT")

fileSellouts <- fileSellouts1 %>%
  rename("DATA" = "Data", "STORE" = "LojaEntreposto")
fileSellouts$DATA <- as.POSIXct(fileSellouts$DATA, format = "%d/%m/%Y", tx = "GMT")


class(fileNinjas$DATA)
class(fileStocks$DATA)
class(fileSellouts$DATA)

#Mergir stocks

ficheiros2 <- fileNinjas

for (i in 1:5) {               
  
  coluna <- names(fileNinjas)[5 + i - 1]   
  
  dfStocks <- fileStocks %>%
    filter(DESC_ARTIGO == coluna) %>%
    select(DATA, STORE, SOH, PRES_STOCK, INTRANSIT, EXPECTED)   
  
  ficheiros2 <- merge(ficheiros2, dfStocks, by = c("DATA", "STORE"), all.x = TRUE)   
  
  colnames(ficheiros2) <- gsub("SOH", paste("STOCK", coluna, sep = " "), colnames(ficheiros2))     
  colnames(ficheiros2) <- gsub("PRES_STOCK", paste("Preslinear", coluna, sep = " "), colnames(ficheiros2))
  colnames(ficheiros2) <- gsub("INTRANSIT", paste("Transito", coluna, sep = " "), colnames(ficheiros2))
  colnames(ficheiros2) <- gsub("EXPECTED", paste("Esperado", coluna, sep = " "), colnames(ficheiros2))
}         

# Mergir sellouts

fileSellouts$STORE <- gsub("^L0*","", fileSellouts$STORE) #Tirar L's e 0's
fileSellouts$STORE <- as.numeric(fileSellouts$STORE)

ficheiros3 <- ficheiros2

for (i in 1:5) {               
  
  coluna <- names(fileNinjas)[5 + i - 1]   
  
  dfSellouts <- fileSellouts %>%
    filter(DscArtigo == coluna) %>%
    select(DATA, STORE, Quant, Valor)   
  
  ficheiros3 <- merge(ficheiros3, dfSellouts, by = c("DATA", "STORE"), all.x = TRUE)   
  colnames(ficheiros3) <- gsub("Quant", paste("Número de", coluna, sep = " "), colnames(ficheiros3))     
  colnames(ficheiros3) <- gsub("Valor", paste("Preço de", coluna, sep = " "), colnames(ficheiros3))
  
}         


#write.table(ficheiros3, "?.csv",row.names = FALSE, sep=";")
