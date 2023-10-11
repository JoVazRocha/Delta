####Stocks SONAE fim de semana 24,25,26###

library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)

# Ler ficheiros
fileNinjas <- read_excel("D:\\Brands and Ninjas\\Delta\\Mar-abril23\\DadosNinjas.xlsx")
fileStocks <- read_excel("D:\\Brands and Ninjas\\Delta\\Mar-abril23\\Stocks Sonae BQ.xlsx", sheet = "Sheet1")
#fileSellouts1 <- read_excel("Sellout_fev.xlsx")


#Renomear coisas e pôr em datetime o que não está

fileNinjas$DATA <- as.POSIXct(fileNinjas$DATA, format = "%d-%m-%Y", tx = "GMT")

fileStocks$DATA <- as.POSIXct(fileStocks$DATA, format = "%d-%m-%Y", tx = "GMT")

fileSellouts <- fileSellouts1 %>%
  rename("DATA" = "Data", "STORE" = "LojaEntreposto")
fileSellouts$DATA <- as.POSIXct(fileSellouts$DATA, format = "%d/%m/%Y", tx = "GMT")


class(fileNinjas$DATA)
class(fileStocks$DATA)
class(fileSellouts$DATA)



ficheiros3 <- read_excel("D:\\Brands and Ninjas\\Delta\\Mar-abril23\\DadosNinjaStocks.xlsx")
#ficheiros2$DATA <- as.POSIXct(fileSellouts$DATA, format = "%d-%m-%Y", tx = "GMT")
ficheiros2 <- read_excel("D:\\Brands and Ninjas\\Delta\\Mar-abril23\\ficheiro_NinStoSell.xlsx")


#GRÁFICOS


#GRÁFICO_CLASSICO

library(grid)

fileLong <- gather(ficheiros2, Produto, Presença, "Promoção":"CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL", factor_key=TRUE)
PresençaPerc <- fileLong %>%
  group_by(Produto) %>%
  summarise(PresençaPerc = sum(Presença) / nrow(ficheiros2)) %>%
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



attach(ficheiros2)
perc1 <- aggregate(`Promoção` ~ DATA,ficheiros2, mean)
G1<-ggplot(perc1, aes(x = DATA, y = `Promoção`)) + 
  geom_col(fill="#704f78") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G1<-G1 +theme(panel.background = element_blank())

F1<-ggplot(data = ficheiros2, aes(x = Loja, y = `Promoção`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

perc2 <- aggregate(`CAFÉ DELTA Q QALIDUS 10CAP` ~ DATA,ficheiros2, mean)
G2<-ggplot(perc2, aes(x = DATA, y = `CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_col(fill="#735b4d") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G2<-G2 +theme(panel.background = element_blank())

F2<-ggplot(data = ficheiros2, aes(x = Loja, y = `CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


perc3 <- aggregate(`CAFÉ DELTA Q AQTIVUS 10CAP` ~ DATA,ficheiros2, mean)
G3<-ggplot(perc3, aes(x = DATA, y = `CAFÉ DELTA Q AQTIVUS 10CAP`)) + 
  geom_col(fill="#9d5c52") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G3<-G3 +theme(panel.background = element_blank())

F3<-ggplot(data = ficheiros2, aes(x = Loja, y = `CAFÉ DELTA Q AQTIVUS 10CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


perc4 <- aggregate(`CAFÉ DELTA Q DEQAFEINATUS 10CAP` ~ DATA,ficheiros2, mean)
G4<-ggplot(perc4, aes(x = DATA, y = `CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) + 
  geom_col(fill="#4b625a") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G4<-G4 +theme(panel.background = element_blank())

F4<-ggplot(data = ficheiros2, aes(x = Loja, y = `CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


perc5 <- aggregate(`CERV.C/ALC.T/P CORONA 35,5CL` ~ DATA,ficheiros2, mean)
G5<-ggplot(perc5, aes(x = DATA, y = `CERV.C/ALC.T/P CORONA 35,5CL`)) + 
  geom_col(fill="#e73b37") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G5<-G5 +theme(panel.background = element_blank())

F5<-ggplot(data = ficheiros2, aes(x = Loja, y = `CERV.C/ALC.T/P CORONA 35,5CL`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


perc6 <- aggregate(`CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` ~ DATA,ficheiros2, mean)
G6<-ggplot(perc6, aes(x = DATA, y = `CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) + 
  geom_col(fill="#e73b37") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")
G6<-G6 +theme(panel.background = element_blank())

F6<-ggplot(data = ficheiros2, aes(x = Loja, y = `CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())



ficheiros2$`Número de produtos Delta presentes`<-ficheiros2$`Número de Promoçãoidade de produtos Delta presentes`
FINAL<-ggplot(data = ficheiros2, aes(x = Loja, y = `Número de produtos Delta presentes`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Número de produtos Delta no ponto de venda") + theme(axis.text.x = element_blank(), panel.background = element_blank())

ggplotly(FINAL)



ggplotly(G1)

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


DATAFUSION<- read_excel("D:\\Brands and Ninjas\\Delta\\Mar-abril23\\DataFusion22-23.xlsx")

#ESTADO 1
DATAFUSION$`ESTADO Promoção`<-NA
DATAFUSION$`ESTADO Promoção`[DATAFUSION$`Promoção` <= 0 ] <- "PRESENTE"
DATAFUSION$`ESTADO Promoção`[DATAFUSION$`Promoção` > 0 ] <- "AUSENTE"

DATAE1 <- DATAFUSION %>%
  dplyr::select(`ESTADO Promoção`)
DATAE1<-na.omit(DATAE1)

freq1 <- table(DATAE1$`ESTADO Promoção`)
dfEsPerc1<-data.frame(prop.table(freq1))

E1 <- ggplot(dfEsPerc1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#704f78", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ESTADO Promoção",
       x = "Classes",
       y = "Frequência") + 
  theme(panel.background = element_blank())



#ESTADO 2
DATAFUSION$`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`<-NA
DATAFUSION$`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q QALIDUS 10CAP` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q QALIDUS 10CAP` > 0 & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q QALIDUS 10CAP` > 0 & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q QALIDUS 10CAP` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE2 <- DATAFUSION %>%
  dplyr::select(`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`)
DATAE2<-na.omit(DATAE2)

freq2 <- table(DATAE2$`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`)
dfEsPerc2<-data.frame(prop.table(freq2))

E2 <- ggplot(dfEsPerc2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#735b4d", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO CAFÉ DELTA Q QALIDUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())


#ESTADO 3
DATAFUSION$`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`<-NA
DATAFUSION$`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q AQTIVUS 10CAP` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q AQTIVUS 10CAP` > 0 & DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q AQTIVUS 10CAP` > 0 & DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q AQTIVUS 10CAP` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE3 <- DATAFUSION %>%
  dplyr::select(`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`)
DATAE3<-na.omit(DATAE3)

freq3 <- table(DATAE3$`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`)
dfEsPerc3<-data.frame(prop.table(freq3))

E3 <- ggplot(dfEsPerc3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9d5c52", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO CAFÉ DELTA Q AQTIVUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())



#ESTADO 4
DATAFUSION$`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<-NA
DATAFUSION$`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q DEQAFEINATUS 10CAP` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q DEQAFEINATUS 10CAP` > 0 & DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q DEQAFEINATUS 10CAP` > 0 & DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`CAFÉ DELTA Q DEQAFEINATUS 10CAP` <= 0 & DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE4 <- DATAFUSION %>%
  dplyr::select(`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
DATAE4<-na.omit(DATAE4)

freq4 <- table(DATAE4$`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
dfEsPerc4<-data.frame(prop.table(freq4))

E4<- ggplot(dfEsPerc4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#4b625a", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())


#ESTADO 5
DATAFUSION$`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`<-NA
DATAFUSION$`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`CERV.C/ALC.T/P CORONA 35,5CL` <= 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`CERV.C/ALC.T/P CORONA 35,5CL` > 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`CERV.C/ALC.T/P CORONA 35,5CL` > 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`CERV.C/ALC.T/P CORONA 35,5CL` <= 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE5 <- DATAFUSION %>%
  dplyr::select(`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`)
DATAE5<-na.omit(DATAE5)

freq5 <- table(DATAE5$`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`)
dfEsPerc5<-data.frame(prop.table(freq5))

E5<- ggplot(dfEsPerc5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e73b37", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO CERV.C/ALC.T/P CORONA 35,5CL`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#ESTADO 6
DATAFUSION$`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<-NA
DATAFUSION$`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` <= 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` <= 0] <- "PRODUTO AUSENTE - SEM STOCK"
DATAFUSION$`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` > 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` <= 0] <- "PRODUTO PRESENTE - SEM STOCK"
DATAFUSION$`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` > 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` > 0] <- "PRODUTO PRESENTE - COM STOCK"
DATAFUSION$`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` <= 0 & DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` > 0] <- "PRODUTO AUSENTE - COM STOCK"

DATAE6 <- DATAFUSION %>%
  dplyr::select(`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
DATAE6<-na.omit(DATAE6)

freq6 <- table(DATAE6$`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
dfEsPerc6<-data.frame(prop.table(freq6))

E6<- ggplot(dfEsPerc6, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e73b37", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "`ESTADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

Ep1<-subplot(E1, E2, E3)
Ep2<-subplot(E4, E5, E6)

subplot(Ep1, Ep2, nrows=2, widths = 1, heights = NULL, margin = 0.02)








### CICLOS ASSEGURADOS? ###






#cafe2  
DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`<-NA
DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`<- DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`[!is.na(DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`) & !is.na(DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`) & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP` >1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`[!is.na(DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`) & !is.na(DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`) & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`  <=1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`


ciclo2 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q QALIDUS 10CAP`)
ciclo2<-na.omit(ciclo2)

C2 <- ggplot(ciclo2, aes(x = `CICLO CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(fill = "#735b4d", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q QALIDUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())



#cafe3
DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`<-NA
DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`<-DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP` >1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP` <=1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`

ciclo3 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)
ciclo3<-na.omit(ciclo3)

C3 <- ggplot(ciclo3, aes(x = `CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)) +
  geom_bar(fill = "#9d5c52", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q AQTIVUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<-NA
DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<-DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP` >1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP` <=1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`

ciclo4 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
ciclo4<-na.omit(ciclo4)

C4<- ggplot(ciclo4, aes(x = `CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`<-NA
DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`<-DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`
#DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`[!is.na(DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`) & !is.na(DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`) & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL` >1.1] <- DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`
#DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`[!is.na(DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`) & !is.na(DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`) & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL` <=1.1] <- DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`

ciclo5 <-DATAFUSION %>%
  dplyr::select(`CICLO CERV.C/ALC.T/P CORONA 35,5CL`)
ciclo5<-na.omit(ciclo5)

C5<- ggplot(ciclo5, aes(x = `CICLO CERV.C/ALC.T/P CORONA 35,5CL`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CERV.C/ALC.T/P CORONA 35,5CL`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe1
DATAFUSION$`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<-NA
DATAFUSION$`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<- DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`
#DATAFUSION$`CICLO Promoção`[DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção` >1.1] <- DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção`
#DATAFUSION$`CICLO Promoção`[DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção` <=1.1] <- DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção`

ciclo1 <-DATAFUSION %>%
  dplyr::select(`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
ciclo1<-na.omit(ciclo1)
C1 <- ggplot(ciclo1, aes(x = `CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) +
  geom_bar(fill = "#704f78", position = "stack") +
  labs(title = " ASSEGURADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#gráficos
Cp1<-subplot(C1, C2, C3)
Cp2<-subplot(C4, C5)

#subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)







###   HÁ STOCK DISPONÍVEL?  ###





#cafe2  
DATAFUSION$`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`<-NA
DATAFUSION$`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`<=0] <- "ROTURA"
DATAFUSION$`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`>0] <- "EM STOCK"

rotura2 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`)
rotura2<-na.omit(rotura2)

R2 <- ggplot(rotura2, aes(x = `ROTURA CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(fill = "#735b4d", position = "stack") +
  labs(title = "`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe3
DATAFUSION$`ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`<-NA
DATAFUSION$`ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`<=0] <- "ROTURA"
DATAFUSION$`ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`>0] <- "EM STOCK"

rotura3 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`)
rotura3<-na.omit(rotura3)

R3 <- ggplot(rotura3, aes(x = `ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`)) +
  geom_bar(fill = "#9d5c52", position = "stack") +
  labs(title = "`ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
DATAFUSION$`ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`<-NA
DATAFUSION$`ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`<=0] <- "ROTURA"
DATAFUSION$`ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`>0] <- "EM STOCK"

rotura4 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
rotura4<-na.omit(rotura4)

R4<- ggplot(rotura4, aes(x = `ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
DATAFUSION$`ROTURA CERV.C/ALC.T/P CORONA 35,5CL`<-NA
DATAFUSION$`ROTURA CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`<=0] <- "ROTURA"
DATAFUSION$`ROTURA CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`>0] <- "EM STOCK"

rotura5 <-DATAFUSION %>%
  dplyr::select(`ROTURA CERV.C/ALC.T/P CORONA 35,5CL`)
rotura5<-na.omit(rotura5)

R5<- ggplot(rotura5, aes(x = `ROTURA CERV.C/ALC.T/P CORONA 35,5CL`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`ROTURA CERV.C/ALC.T/P CORONA 35,5CL`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe1
DATAFUSION$`ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<-NA
DATAFUSION$`ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<=0] <- "ROTURA"
DATAFUSION$`ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`>0] <- "EM STOCK"

rotura1 <-DATAFUSION %>%
  dplyr::select(`ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
rotura1<-na.omit(rotura1)
R1 <- ggplot(rotura1, aes(x = `ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) +
  geom_bar(fill = "#704f78", position = "stack") +
  labs(title = "ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())


#gráficos
Rp1<-subplot(R1, R2, R3)
Rp2<-subplot(R4, R5)

#subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)






### ADEQUAÇÃO DE STOCK ###






#cafe2  
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`<-NA
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`+DATAFUSION$`INTRANSIT CAFÉ DELTA Q QALIDUS 10CAP`+DATAFUSION$`EXPECTED CAFÉ DELTA Q QALIDUS 10CAP`>= DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`+DATAFUSION$`INTRANSIT CAFÉ DELTA Q QALIDUS 10CAP`+DATAFUSION$`EXPECTED CAFÉ DELTA Q QALIDUS 10CAP`< DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa2 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`)
adequa2<-na.omit(adequa2)

freq2 <- table(adequa2$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`)
perce2<- freq2/sum(freq2)*100
dfAdPerc2<-data.frame(prop.table(freq2))


A2 <- ggplot(dfAdPerc2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#735b4d", stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe3
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`<-NA
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`+DATAFUSION$`INTRANSIT CAFÉ DELTA Q AQTIVUS 10CAP`+DATAFUSION$`EXPECTED CAFÉ DELTA Q AQTIVUS 10CAP`>= DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`+DATAFUSION$`INTRANSIT CAFÉ DELTA Q AQTIVUS 10CAP`+DATAFUSION$`EXPECTED CAFÉ DELTA Q AQTIVUS 10CAP`< DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa3 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`)
adequa3<-na.omit(adequa3)

freq3 <- table(adequa3$`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`)
dfAdPerc3<-data.frame(prop.table(freq3))

A3 <- ggplot(dfAdPerc3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9d5c52", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe4
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<-NA
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`+DATAFUSION$`INTRANSIT CAFÉ DELTA Q DEQAFEINATUS 10CAP`+DATAFUSION$`EXPECTED CAFÉ DELTA Q DEQAFEINATUS 10CAP`>= DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<=1.1 & DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`+DATAFUSION$`INTRANSIT CAFÉ DELTA Q DEQAFEINATUS 10CAP`+DATAFUSION$`EXPECTED CAFÉ DELTA Q DEQAFEINATUS 10CAP`< DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa4 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
adequa4<-na.omit(adequa4)

freq4 <- table(adequa4$`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
dfAdPerc4<-data.frame(prop.table(freq4))

A4<- ggplot(dfAdPerc4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#4b625a", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe5
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`<-NA
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`<=1.1 & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`+DATAFUSION$`INTRANSIT CERV.C/ALC.T/P CORONA 35,5CL`+DATAFUSION$`EXPECTED CERV.C/ALC.T/P CORONA 35,5CL`>= DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`[DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`<=1.1 & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`+DATAFUSION$`INTRANSIT CERV.C/ALC.T/P CORONA 35,5CL`+DATAFUSION$`EXPECTED CERV.C/ALC.T/P CORONA 35,5CL`< DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa5 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`)
adequa5<-na.omit(adequa5)

freq5 <- table(adequa5$`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`)
dfAdPerc5<-data.frame(prop.table(freq5))

A5<- ggplot(dfAdPerc5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e73b37", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#cafe1
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<-NA
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`>1.1] <- "STOCK SUF."
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<=1.1 & DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`+DATAFUSION$`INTRANSIT CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`+DATAFUSION$`EXPECTED CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`>= DATAFUSION$`Preslinear CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`] <- "STOCK INSUF. C FORN. ADEQUADO"
DATAFUSION$`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`[DATAFUSION$`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<=1.1 & DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`+DATAFUSION$`INTRANSIT CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`+DATAFUSION$`EXPECTED CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`< DATAFUSION$`Preslinear CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`] <- "STOCK INSUF. C FORN. DESADEQUADO"

adequa1 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
adequa1<-na.omit(adequa1)

freq1 <- table(adequa1$`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
dfAdPerc1<-data.frame(prop.table(freq1))

A1<-ggplot(dfAdPerc1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#704f78", stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL",
       x = "Classes",
       y = "Frequência") +
  theme(panel.background = element_blank())

#gráficos
Ap1<-subplot(A1, A2)
Ap2<-plot(A3)
Ap3<-subplot(A4, A5)

#subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)


### Dias para a Rotura ###






#cafe2  
DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`<-NA
DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`<- DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`[!is.na(DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`) & !is.na(DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`) & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP` >1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q QALIDUS 10CAP`[!is.na(DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`) & !is.na(DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`) & DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`  <=1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q QALIDUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q QALIDUS 10CAP`


ciclo2 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q QALIDUS 10CAP`)
ciclo2<-na.omit(ciclo2)

C2 <- ggplot(ciclo2, aes(x = `CICLO CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(fill = "#735b4d", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q QALIDUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())



#cafe3
DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`<-NA
DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`<-DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP` >1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP` <=1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q AQTIVUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q AQTIVUS 10CAP`

ciclo3 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)
ciclo3<-na.omit(ciclo3)

C3 <- ggplot(ciclo3, aes(x = `CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)) +
  geom_bar(fill = "#9d5c52", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q AQTIVUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<-NA
DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`<-DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP` >1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`
#DATAFUSION$`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`[DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP` <=1.1] <- DATAFUSION$`STOCK CAFÉ DELTA Q DEQAFEINATUS 10CAP`/DATAFUSION$`Preslinear CAFÉ DELTA Q DEQAFEINATUS 10CAP`

ciclo4 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
ciclo4<-na.omit(ciclo4)

C4<- ggplot(ciclo4, aes(x = `CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`<-NA
DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`<-DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`
#DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`[!is.na(DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`) & !is.na(DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`) & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL` >1.1] <- DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`
#DATAFUSION$`CICLO CERV.C/ALC.T/P CORONA 35,5CL`[!is.na(DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`) & !is.na(DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`) & DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL` <=1.1] <- DATAFUSION$`STOCK CERV.C/ALC.T/P CORONA 35,5CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P CORONA 35,5CL`

ciclo5 <-DATAFUSION %>%
  dplyr::select(`CICLO CERV.C/ALC.T/P CORONA 35,5CL`)
ciclo5<-na.omit(ciclo5)

C5<- ggplot(ciclo5, aes(x = `CICLO CERV.C/ALC.T/P CORONA 35,5CL`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CERV.C/ALC.T/P CORONA 35,5CL`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe1
DATAFUSION$`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<-NA
DATAFUSION$`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`<- DATAFUSION$`STOCK CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`/DATAFUSION$`Preslinear CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`
#DATAFUSION$`CICLO Promoção`[DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção` >1.1] <- DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção`
#DATAFUSION$`CICLO Promoção`[DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção` <=1.1] <- DATAFUSION$`STOCK Promoção`/DATAFUSION$`Preslinear Promoção`

ciclo1 <-DATAFUSION %>%
  dplyr::select(`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
ciclo1<-na.omit(ciclo1)
C1 <- ggplot(ciclo1, aes(x = `CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) +
  geom_bar(fill = "#704f78", position = "stack") +
  labs(title = " ASSEGURADO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#gráficos
Cp1<-subplot(C1, C2, C3)
Cp2<-subplot(C4, C5)

#subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)



### GRÁFICOS ###

#Presença de produto   Solução 1x5x1 
#P1<-subplot(G5,G4,G3,G2,G1)
#subplot(Classic, P1, FINAL, nrows = 3, widths = 0.9, heights = NULL, margin = 0.009)

#Estado do café (Stock e ninjas)
subplot(Ep1, Ep2, nrows=2, widths = 1, heights = NULL, margin = 0.02)

#Ciclos de reposição assegurados
subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Rotura de stock
subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Adequação de Stock
subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)

manter <- c("Ep1", "Ep2", "Cp1", "Cp2", "Rp1", "Rp2", "Ap1", "Ap2", "Ap3")
rm(list = setdiff(ls(), manter))

### Exportar ficheiro CSV###
write.csv(DATAFUSION, "DataFusionFinal.csv",row.names = FALSE, sep=";")