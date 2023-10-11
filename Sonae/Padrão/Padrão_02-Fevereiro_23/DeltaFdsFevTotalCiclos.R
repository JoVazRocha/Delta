####Stocks SONAE fim de semana 24,25,26###

library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)

# Ler ficheiros
fileNinjas1 <- read_excel("Resultado Sonae_fevereiro 23.xlsx")
fileStocks <- read_excel("Stocks_fev.xlsx", sheet = "Sheet1")
fileSellouts1 <- read_excel("Sellout_fev.xlsx")

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
    dplyr::select(DATA, STORE, SOH, PRES_STOCK, INTRANSIT, EXPECTED)
  
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
    dplyr::select(DATA, STORE, Quant, Valor)
  
  ficheiros3 <- merge(ficheiros3, dfSellouts, by = c("DATA", "STORE"), all.x = TRUE)   
  colnames(ficheiros3) <- gsub("Quant", paste("Número de", coluna, sep = " "), colnames(ficheiros3))     
  colnames(ficheiros3) <- gsub("Valor", paste("Preço de", coluna, sep = " "), colnames(ficheiros3))
  
}

ficheiros3$`Número de produtos Delta presentes`<-ficheiros3$`Número de PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAPidade de produtos Delta presentes`
write.table(ficheiros3, "Delta Fevereiro.csv",row.names = FALSE, sep=";")




                    #GRÁFICOS


                    #GRÁFICO_CLASSICO

library(grid)

fileLong <- gather(ficheiros3, Produto, Presença, "PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP":"CAFÉ SOLÚVEL DELTA FRASCO 200G", factor_key=TRUE)
PresençaPerc <- fileLong %>%
  group_by(Produto) %>%
  summarise(PresençaPerc = sum(Presença) / nrow(ficheiros3)) %>%
  arrange(desc(PresençaPerc))
Classic <- ggplot(PresençaPerc, aes(x=PresençaPerc*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#17706E")+
  geom_text(aes(label = paste0(round(PresençaPerc * 100), "%")), hjust = -0.2)+
  labs(title= "Presença de Produto em Linear verificado pelo Ninja")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

Classic<-Classic +theme(panel.background = element_blank())
#Classic<- Classic+ annotation_custom(rasterGrob(img), 
# xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 14) +
# scale_y_continuous(limits = c(0, 14))





                    #GRÁFICO POR FIM DE SEMANA



attach(ficheiros3)
perc1 <- aggregate(`PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` ~ DATA,ficheiros3, mean)
G1<-ggplot(perc1, aes(x = DATA, y = `PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)) + 
  geom_col(fill="#704f78") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G1<-G1 +theme(panel.background = element_blank())

perc2 <- aggregate(`PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` ~ DATA,ficheiros3, mean)
G2<-ggplot(perc2, aes(x = DATA, y = `PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)) + 
  geom_col(fill="#735b4d") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G2<-G2 +theme(panel.background = element_blank())

perc3 <- aggregate(`PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` ~ DATA,ficheiros3, mean)
G3<-ggplot(perc3, aes(x = DATA, y = `PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)) + 
  geom_col(fill="#9d5c52") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G3<-G3 +theme(panel.background = element_blank())

perc4 <- aggregate(`CAFÉ DELTA PORTUGAL MU 220G` ~ DATA,ficheiros3, mean)
G4<-ggplot(perc4, aes(x = DATA, y = `CAFÉ DELTA PORTUGAL MU 220G`)) + 
  geom_col(fill="#4b625a") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G4<-G4 +theme(panel.background = element_blank())

perc5 <- aggregate(`CAFÉ SOLÚVEL DELTA FRASCO 200G` ~ DATA,ficheiros3, mean)
G5<-ggplot(perc5, aes(x = DATA, y = `CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_col(fill="#e73b37") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G5<-G5 +theme(panel.background = element_blank())

ficheiros3$`Número de produtos Delta presentes`<-ficheiros3$`Número de PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAPidade de produtos Delta presentes`
FINAL<-ggplot(data = ficheiros3, aes(x = Loja, y = `Número de produtos Delta presentes`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Número de produtos Delta no ponto de venda") + theme(axis.text.x = element_blank(), panel.background = element_blank())


#Overview Ninja
#subplot(Classic, G5, G4, G3, G2, G1,FINAL, nrows = 7, widths = 0.3, heights = NULL, margin = 0.009)
#subplot(Classic, G5, G4, G3, G2, G1, FINAL, nrows = 7, widths = c(1, 0.5, 0.5, 0.5, 1), 
#        heights = c(1, 0.5, 0.5, 0.5, 1), margin = 0.009)

#Solução 1x3x2x1
P1<-subplot(G5,G4,G3)
P2<-subplot(G2,G1)

#subplot(Classic, P1, P2, FINAL, nrows = 4, widths = 0.9, heights = NULL, margin = 0.009)

#Solução 1x5x1

P1<-subplot(G5,G4,G3,G2,G1)
#subplot(Classic, P1, FINAL, nrows = 3, widths = 0.9, heights = NULL, margin = 0.009)





                    ###   SIGNAL DETECTION - NINJA/STOCK    ###


DATAFUSION<-ficheiros3

#ESTADO 1
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<-NA
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` <= 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` > 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` > 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` <= 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE1 <- DATAFUSION %>%
  dplyr::select(`ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
DATAE1<-na.omit(DATAE1)

freq1 <- table(DATAE1$`ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
dfEsPerc1<-data.frame(prop.table(freq1))

E1 <- ggplot(dfEsPerc1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#704f78", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ESTADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") + 
  theme(panel.background = element_blank())



#ESTADO 2
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<-NA
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` <= 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` > 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` > 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` <= 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE2 <- DATAFUSION %>%
  dplyr::select(`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
DATAE2<-na.omit(DATAE2)

freq2 <- table(DATAE2$`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
dfEsPerc2<-data.frame(prop.table(freq2))

E2 <- ggplot(dfEsPerc2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#735b4d", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())


#ESTADO 3
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-NA
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` <= 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` > 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` > 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` <= 0 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE3 <- DATAFUSION %>%
  dplyr::select(`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
DATAE3<-na.omit(DATAE3)

freq3 <- table(DATAE3$`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
dfEsPerc3<-data.frame(prop.table(freq3))

E3 <- ggplot(dfEsPerc3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9d5c52", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

`CAFÉ DELTA PORTUGAL MU 220G`

#ESTADO 4
DATAFUSION$`ESTADO CAFÉ DELTA PORTUGAL MU 220G`<-NA
DATAFUSION$`ESTADO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`CAFÉ DELTA PORTUGAL MU 220G` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`CAFÉ DELTA PORTUGAL MU 220G` > 0 & DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`CAFÉ DELTA PORTUGAL MU 220G` > 0 & DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`CAFÉ DELTA PORTUGAL MU 220G` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE4 <- DATAFUSION %>%
  dplyr::select(`ESTADO CAFÉ DELTA PORTUGAL MU 220G`)
DATAE4<-na.omit(DATAE4)

freq4 <- table(DATAE4$`ESTADO CAFÉ DELTA PORTUGAL MU 220G`)
dfEsPerc4<-data.frame(prop.table(freq4))

E4<- ggplot(dfEsPerc4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#4b625a", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO CAFÉ DELTA PORTUGAL MU 220G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())


#ESTADO 5
DATAFUSION$`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`<-NA
DATAFUSION$`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`CAFÉ SOLÚVEL DELTA FRASCO 200G` <= 0 & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`CAFÉ SOLÚVEL DELTA FRASCO 200G` > 0 & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`CAFÉ SOLÚVEL DELTA FRASCO 200G` > 0 & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`CAFÉ SOLÚVEL DELTA FRASCO 200G` <= 0 & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE5 <- DATAFUSION %>%
  dplyr::select(`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
DATAE5<-na.omit(DATAE5)

freq5 <- table(DATAE5$`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfEsPerc5<-data.frame(prop.table(freq5))

E5<- ggplot(dfEsPerc5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e73b37", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

Ep1<-subplot(E1, E2, E3)
Ep2<-subplot(E4, E5)

#subplot(Ep1, Ep2, nrows=2, widths = 1, heights = NULL, margin = 0.02)








                    ### CICLOS ASSEGURADOS? ###




#cafe1
DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<-NA
DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`
#DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` >1.1] <- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`
#DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP` <=1.1] <- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`

ciclo1 <-DATAFUSION %>%
  dplyr::select(`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
ciclo1<-na.omit(ciclo1)
C1 <- ggplot(ciclo1, aes(x = `CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)) +
  geom_bar(fill = "#704f78", position = "stack") +
  labs(title = " ASSEGURADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe2  
DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<-NA
DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`
#DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[!is.na(DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`) & !is.na(DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`) & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` >1.1] <- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`
#DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[!is.na(DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`) & !is.na(DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`) & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`  <=1.1] <- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`

  
ciclo2 <-DATAFUSION %>%
  dplyr::select(`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
ciclo2<-na.omit(ciclo2)

C2 <- ggplot(ciclo2, aes(x = `CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)) +
  geom_bar(fill = "#735b4d", position = "stack") +
  labs(title = "`CICLO ASSEGURADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())



#cafe3
DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-NA
DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`
#DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` >1.1] <- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`
#DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` <=1.1] <- DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`

ciclo3 <-DATAFUSION %>%
  dplyr::select(`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
ciclo3<-na.omit(ciclo3)

C3 <- ggplot(ciclo3, aes(x = `CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)) +
  geom_bar(fill = "#9d5c52", position = "stack") +
  labs(title = "`CICLO ASSEGURADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
DATAFUSION$`CICLO CAFÉ DELTA PORTUGAL MU 220G`<-NA
DATAFUSION$`CICLO CAFÉ DELTA PORTUGAL MU 220G`<-DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`/DATAFUSION$`Preslinear CAFÉ DELTA PORTUGAL MU 220G`
#DATAFUSION$`CICLO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`/DATAFUSION$`Preslinear CAFÉ DELTA PORTUGAL MU 220G` >1.1] <- DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`/DATAFUSION$`Preslinear CAFÉ DELTA PORTUGAL MU 220G`
#DATAFUSION$`CICLO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`/DATAFUSION$`Preslinear CAFÉ DELTA PORTUGAL MU 220G` <=1.1] <- DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`/DATAFUSION$`Preslinear CAFÉ DELTA PORTUGAL MU 220G`

ciclo4 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA PORTUGAL MU 220G`)
ciclo4<-na.omit(ciclo4)

C4<- ggplot(ciclo4, aes(x = `CICLO CAFÉ DELTA PORTUGAL MU 220G`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA PORTUGAL MU 220G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
DATAFUSION$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`<-NA
DATAFUSION$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`<-DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`/DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`
#DATAFUSION$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`[!is.na(DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`) & !is.na(DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`) & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`/DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G` >1.1] <- DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`/DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`
#DATAFUSION$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`[!is.na(DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`) & !is.na(DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`) & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`/DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G` <=1.1] <- DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`/DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`

ciclo5 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
ciclo5<-na.omit(ciclo5)

C5<- ggplot(ciclo5, aes(x = `CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#gráficos
Cp1<-subplot(C1, C2, C3)
Cp2<-subplot(C4, C5)

#subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)







                    ###   HÁ STOCK DISPONÍVEL?  ###




#cafe1
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<-NA
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<=0] <- "ROTURA"
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`>0] <- "EM STOCK"

rotura1 <-DATAFUSION %>%
  dplyr::select(`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
rotura1<-na.omit(rotura1)
R1 <- ggplot(rotura1, aes(x = `ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)) +
  geom_bar(fill = "#704f78", position = "stack") +
  labs(title = "ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe2  
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<-NA
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<=0] <- "ROTURA"
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`>0] <- "EM STOCK"

rotura2 <-DATAFUSION %>%
  dplyr::select(`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
rotura2<-na.omit(rotura2)

R2 <- ggplot(rotura2, aes(x = `ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)) +
  geom_bar(fill = "#735b4d", position = "stack") +
  labs(title = "`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe3
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-NA
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<=0] <- "ROTURA"
DATAFUSION$`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`>0] <- "EM STOCK"

rotura3 <-DATAFUSION %>%
  dplyr::select(`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
rotura3<-na.omit(rotura3)

R3 <- ggplot(rotura3, aes(x = `ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)) +
  geom_bar(fill = "#9d5c52", position = "stack") +
  labs(title = "`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
DATAFUSION$`ROTURA CAFÉ DELTA PORTUGAL MU 220G`<-NA
DATAFUSION$`ROTURA CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`<=0] <- "ROTURA"
DATAFUSION$`ROTURA CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`>0] <- "EM STOCK"

rotura4 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA PORTUGAL MU 220G`)
rotura4<-na.omit(rotura4)

R4<- ggplot(rotura4, aes(x = `ROTURA CAFÉ DELTA PORTUGAL MU 220G`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`ROTURA CAFÉ DELTA PORTUGAL MU 220G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
DATAFUSION$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`<-NA
DATAFUSION$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`<=0] <- "ROTURA"
DATAFUSION$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`>0] <- "EM STOCK"

rotura5 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)
rotura5<-na.omit(rotura5)

R5<- ggplot(rotura5, aes(x = `ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#gráficos
Rp1<-subplot(R1, R2, R3)
Rp2<-subplot(R4, R5)

#subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)






                    ### ADEQUAÇÃO DE STOCK ###




#cafe1
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<-NA
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<=1.1 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+DATAFUSION$`Transito PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+DATAFUSION$`Esperado PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`>= DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<=1.1 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+DATAFUSION$`Transito PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+DATAFUSION$`Esperado PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`< DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa1 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
adequa1<-na.omit(adequa1)

freq1 <- table(adequa1$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
dfAdPerc1<-data.frame(prop.table(freq1))

A1<-ggplot(dfAdPerc1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#704f78", stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe2  
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<-NA
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<=1.1 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+DATAFUSION$`Transito PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+DATAFUSION$`Esperado PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`>= DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<=1.1 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+DATAFUSION$`Transito PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+DATAFUSION$`Esperado PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`< DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa2 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
adequa2<-na.omit(adequa2)

freq2 <- table(adequa2$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
perce2<- freq2/sum(freq2)*100
dfAdPerc2<-data.frame(prop.table(freq2))


A2 <- ggplot(dfAdPerc2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#735b4d", stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe3
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-NA
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<=1.1 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+DATAFUSION$`Transito PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+DATAFUSION$`Esperado PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`>= DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[DATAFUSION$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<=1.1 & DATAFUSION$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+DATAFUSION$`Transito PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+DATAFUSION$`Esperado PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`< DATAFUSION$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa3 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
adequa3<-na.omit(adequa3)

freq3 <- table(adequa3$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
dfAdPerc3<-data.frame(prop.table(freq3))

A3 <- ggplot(dfAdPerc3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9d5c52", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe4
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`<-NA
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`CICLO CAFÉ DELTA PORTUGAL MU 220G`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`CICLO CAFÉ DELTA PORTUGAL MU 220G`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`+DATAFUSION$`Transito CAFÉ DELTA PORTUGAL MU 220G`+DATAFUSION$`Esperado CAFÉ DELTA PORTUGAL MU 220G`>= DATAFUSION$`Preslinear CAFÉ DELTA PORTUGAL MU 220G`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`[DATAFUSION$`CICLO CAFÉ DELTA PORTUGAL MU 220G`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA PORTUGAL MU 220G`+DATAFUSION$`Transito CAFÉ DELTA PORTUGAL MU 220G`+DATAFUSION$`Esperado CAFÉ DELTA PORTUGAL MU 220G`< DATAFUSION$`Preslinear CAFÉ DELTA PORTUGAL MU 220G`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa4 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`)
adequa4<-na.omit(adequa4)

freq4 <- table(adequa4$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`)
dfAdPerc4<-data.frame(prop.table(freq4))

A4<- ggplot(dfAdPerc4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#4b625a", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe5
DATAFUSION$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`<-NA
DATAFUSION$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`<=1.1 & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`+DATAFUSION$`Transito CAFÉ SOLÚVEL DELTA FRASCO 200G`+DATAFUSION$`Esperado CAFÉ SOLÚVEL DELTA FRASCO 200G`>= DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`[DATAFUSION$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`<=1.1 & DATAFUSION$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`+DATAFUSION$`Transito CAFÉ SOLÚVEL DELTA FRASCO 200G`+DATAFUSION$`Esperado CAFÉ SOLÚVEL DELTA FRASCO 200G`< DATAFUSION$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa5 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
adequa5<-na.omit(adequa5)

freq5 <- table(adequa5$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfAdPerc5<-data.frame(prop.table(freq5))

A5<- ggplot(dfAdPerc5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e73b37", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#gráficos
Ap1<-subplot(A1, A2)
Ap2<-plot(A3)
Ap3<-subplot(A4, A5)

#subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)



                          ### GRÁFICOS ###

#Presença de produto   Solução 1x5x1 
P1<-subplot(G5,G4,G3,G2,G1)
subplot(Classic, P1, FINAL, nrows = 3, widths = 0.9, heights = NULL, margin = 0.009)

#Estado do café (Stock e ninjas)
subplot(Ep1, Ep2, nrows=2, widths = 1, heights = NULL, margin = 0.02)

#Ciclos de reposição assegurados
subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Rotura de stock
subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Adequação de Stock
subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)


                        ### Exportar ficheiro CSV###
write.csv(DATAFUSION, "DataFusionFinal.csv",row.names = FALSE, sep=";")
