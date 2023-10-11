####Stocks SONAE fim de semana 24,25,26###

library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
# Ler ficheiros


#Dados todos
Dados <- read_excel("D:\\B&N Dados\\Delta\\Piloto\\OUT2DataFusion2.xlsx")
Dados2 <- read_excel("D:\\B&N Dados\\Delta\\Piloto\\OUT2DataFusion2022.xlsx")


#Variáveis
#Produtos <- read.table("D:\\B&N Dados\\Delta\\xxxxxxxx.txt", sep='\t', header=FALSE)
#Promos<- c("Promoção")


#Gráficos

df_Classic <- read_excel("D:\\B&N Dados\\Delta\\Piloto\\OUT5med.xlsx")
df_ClassicRep <- read_excel("D:\\B&N Dados\\Delta\\Piloto\\OUT5med_reposto.xlsx")
df_ClassicSemRep <- read_excel("D:\\B&N Dados\\Delta\\Piloto\\OUT5med_naoreposto.xlsx")

df_NumProdutos <- read_excel("D:\\B&N Dados\\Delta\\Piloto\\OUT5_NumProdutos.xlsx")

df_Rotura <- read_excel("D:\\B&N Dados\\Delta\\Piloto\\OUT5_Rotura.xlsx")


DATAFUSION <- Dados
DATAFUSION2022 <- Dados2


#Ordem dos elementos
#1º Elemento: CAFÉ DELTA Q MYTHIQ 80CAP
#Último elemento: BEBIDA CEREAIS DELTA C/20%CAFE FR 200G


#Nº de Produtos: 10
#produto1 <-  "#98362b CAFÉ DELTA Q MYTHIQ 80CAP"
#produto2 <-  "#662867 CAFÉ DELTA Q QALIDUS 80CAP"
#produto3 <-  "#9e2f24 CAFÉ DELTA Q QALIDUS 10CAP"
#produto4 <-  "#4b3b3b CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G"
#produto5 <-  "#b24159 CAFÉ BELLISSIMO INTENSO 200GR"
#produto6 <-  "#222222 CAFÉ DELTA RITUAL MU 220G"
#produto7 <-  "#710302 CAFÉ SOLÚVEL DELTA FRASCO 100G"
#produto8 <-  "#710302 CAFÉ SOLÚVEL DELTA FRASCO 200G"
#produto9 <-  "#ee9b25 CEVADA SOLÚVEL DELTA FRASCO 200G"
#produto10 <- "#ad4a23 BEBIDA CEREAIS DELTA C/20%CAFE FR 200G"

#Nº de Promoções: 0



#GRÁFICOS


# CLÁSSICO -> Presença em Linear para cada elemento em análise


## tudo ##

Classic <- ggplot(df_Classic, aes(x=Frequência*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#17706E")+
  geom_text(aes(label = paste0(round(Frequência * 100), "%")), hjust = -0.2)+
  labs(title= "Presença de Produto em Linear verificado pelo Ninja")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.background = element_blank())


#Classic<- Classic+ annotation_custom(rasterGrob(img),            #O que é isto?
# xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 14) +
# scale_y_continuous(limits = c(0, 14))

Classic
#install.packages("flexdashboards")


## com reposição ##

ClassicSL <- ggplot(df_ClassicRep, aes(x=Frequência*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#FFA07A")+
  geom_text(aes(label = paste0(round(Frequência*100), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

CLASSICR<-ClassicSL+theme(panel.background = element_blank())
ggplotly(CLASSICR)

## sem reposição ##

ClassicNL <- ggplot(df_ClassicSemRep, aes(x=Frequência*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#6495ED")+
  geom_text(aes(label = paste0(round(Frequência*100.0), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

CLASSICRC<-ClassicNL+theme(panel.background = element_blank())



# Proporção de Produtos presentes


df_Reposto <- aggregate(`Num_ProdutosRep` ~ Loja,df_NumProdutos, mean)
FINALSL<-ggplot(data = df_Reposto, aes(x = `Loja`, y = `Num_ProdutosRep`)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  geom_text(aes(label = paste0(round(Num_ProdutosRep), "%")), hjust = -0.2)+       # PODE SER TIRADO
  labs(x = "Ponto de Venda", y = "") + theme(axis.text.x = element_blank(), panel.background = element_blank())


df_NReposto <- aggregate(`Num_ProdutosNRep` ~ Loja,df_NumProdutos, mean)
FINALNL<-ggplot(data = df_NReposto, aes(x = `Loja`, y = `Num_ProdutosNRep`)) +
  geom_bar(stat = "identity", fill = "#6495ED") +
  labs(x = "Ponto de Venda", y = "") + theme(axis.text.x = element_blank(), panel.background = element_blank())








#GRÁFICO POR FIM DE SEMANA

#G é gráfico de proporção de presença por produto por dia
#F é gráfico de número de vezes que foi detectada a presença do produto no linear por loja (se calhar devia fazer proporção)

#Produto1

perc1 <- aggregate(`CAFÉ DELTA Q MYTHIQ 80CAP` ~ DATA,DATAFUSION, mean)
G1<-ggplot(perc1, aes(x = DATA, y = `CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_col(fill="#98362b") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim1 <- aggregate(`CAFÉ DELTA Q MYTHIQ 80CAP` ~ Loja,DATAFUSION, mean)
F1<-ggplot(data = fim1, aes(x = Loja, y = `CAFÉ DELTA Q MYTHIQ 80CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

ggplotly(F1)

#Produto2

perc2 <- aggregate(`CAFÉ DELTA Q QALIDUS 80CAP` ~ DATA,DATAFUSION, mean)
G2<-ggplot(perc2, aes(x = DATA, y = `CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_col(fill="#662867") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim2 <- aggregate(`CAFÉ DELTA Q QALIDUS 80CAP` ~ Loja,DATAFUSION, mean)
F2<-ggplot(data = fim2, aes(x = Loja, y = `CAFÉ DELTA Q QALIDUS 80CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto3

perc3 <- aggregate(`CAFÉ DELTA Q QALIDUS 10CAP` ~ DATA,DATAFUSION, mean)
G3<-ggplot(perc3, aes(x = DATA, y = `CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_col(fill="#9e2f24") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim3 <- aggregate(`CAFÉ DELTA Q QALIDUS 10CAP` ~ Loja,DATAFUSION, mean)
F3<-ggplot(data = fim3, aes(x = Loja, y = `CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto4

perc4 <- aggregate(`CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G` ~ DATA,DATAFUSION, mean)
G4<-ggplot(perc4, aes(x = DATA, y = `CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) + 
  geom_col(fill="#4b3b3b") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim4 <- aggregate(`CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G` ~ Loja,DATAFUSION, mean)
F4<-ggplot(data = fim4, aes(x = Loja, y = `CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto5

perc5 <- aggregate(`CAFÉ BELLISSIMO INTENSO 200GR` ~ DATA,DATAFUSION, mean)
G5<-ggplot(perc5, aes(x = DATA, y = `CAFÉ BELLISSIMO INTENSO 200GR`)) + 
  geom_col(fill="#b24159") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim5 <- aggregate(`CAFÉ BELLISSIMO INTENSO 200GR` ~ Loja,DATAFUSION, mean)
F5<-ggplot(data = fim5, aes(x = Loja, y = `CAFÉ BELLISSIMO INTENSO 200GR`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto6

perc6 <- aggregate(`CAFÉ DELTA RITUAL MU 220G` ~ DATA,DATAFUSION, mean)
G6<-ggplot(perc6, aes(x = DATA, y = `CAFÉ DELTA RITUAL MU 220G`)) + 
  geom_col(fill="#222222") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim6 <- aggregate(`CAFÉ DELTA RITUAL MU 220G` ~ Loja,DATAFUSION, mean)
F6<-ggplot(data = fim6, aes(x = Loja, y = `CAFÉ DELTA RITUAL MU 220G`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto7

perc7 <- aggregate(`CAFÉ SOLÚVEL DELTA FRASCO 100G` ~ DATA,DATAFUSION, mean)
G7<-ggplot(perc7, aes(x = DATA, y = `CAFÉ SOLÚVEL DELTA FRASCO 100G`)) + 
  geom_col(fill="#710302") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim7 <- aggregate(`CAFÉ SOLÚVEL DELTA FRASCO 100G` ~ Loja,DATAFUSION, mean)
F7<-ggplot(data = fim7, aes(x = Loja, y = `CAFÉ SOLÚVEL DELTA FRASCO 100G`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto8

perc8 <- aggregate(`CAFÉ SOLÚVEL DELTA FRASCO 200G` ~ DATA,DATAFUSION, mean)
G8<-ggplot(perc8, aes(x = DATA, y = `CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_col(fill="#710302") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim8 <- aggregate(`CAFÉ SOLÚVEL DELTA FRASCO 200G` ~ Loja,DATAFUSION, mean)
F8<-ggplot(data = fim8, aes(x = Loja, y = `CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto9

perc9 <- aggregate(`CEVADA SOLÚVEL DELTA FRASCO 200G` ~ DATA,DATAFUSION, mean)
G9<-ggplot(perc9, aes(x = DATA, y = `CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_col(fill="#ee9b25") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim9 <- aggregate(`CEVADA SOLÚVEL DELTA FRASCO 200G` ~ Loja,DATAFUSION, mean)
F9<-ggplot(data = fim9, aes(x = Loja, y = `CEVADA SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto10

perc10 <- aggregate(`BEBIDA CEREAIS DELTA C/20%CAFE FR 200G` ~ DATA,DATAFUSION, mean)
G10<-ggplot(perc10, aes(x = DATA, y = `BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) + 
  geom_col(fill="#ad4a23") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

fim10 <- aggregate(`BEBIDA CEREAIS DELTA C/20%CAFE FR 200G` ~ Loja,DATAFUSION, mean)
F10<-ggplot(data = fim10, aes(x = Loja, y = `BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


#Total

fim <- aggregate(`Num_Produtos` ~ Loja,df_NumProdutos, mean)
FINAL<-ggplot(data = fim, aes(x = Loja, y = `Num_Produtos`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Percentagem de produtos Delta no ponto de venda") + theme(axis.text.x = element_blank(), panel.background = element_blank())

FINAL

#ggplotly(FINAL)





#Overview Ninja
#subplot(Classic, G5, G4, G3, G2, G1,FINAL, nrows = 7, widths = 0.3, heights = NULL, margin = 0.009)
#subplot(Classic, G5, G4, G3, G2, G1, FINAL, nrows = 7, widths = c(1, 0.5, 0.5, 0.5, 1), 
#        heights = c(1, 0.5, 0.5, 0.5, 1), margin = 0.009)

#Solução 1x3x2x1
#P1<-subplot(G5,G4,G3)
#P2<-subplot(G2,G1)

#subplot(Classic, P1, P2, FINAL, nrows = 4, widths = 0.9, heights = NULL, margin = 0.009)

#Solução 1x5x1

#P1<-subplot(G5,G4,G3,G2,G1)
#subplot(Classic, P1, FINAL, nrows = 3, widths = 0.9, heights = NULL, margin = 0.009)





###   SIGNAL DETECTION - NINJA/STOCK    ###


#PRODUTOS
padrãoSINAL<- "SINAL [nome]"

#Produto1

#SINAL1<-gsub("\\[nome\\]", Produtos[1,], padrãoSINAL)
#SINAL1
#PresençaC1 <- DATAFUSION %>%
#  dplyr::select(all_of(SINAL1))
#PresençaC1<-na.omit(PresençaC1)

#FreqC1 <- table(PresençaC1$`SINAL CAFÉ DELTA Q MYTHIQ 80CAP`)  #Também não consigo mudar aqui
#dfPercC1<-data.frame(prop.table(FreqC1))
#names(dfPercC1) <- c(Promos[1], "Freq")

#GPresença1 <- ggplot(dfPercC1, aes(x = Var1, y = Freq)) +
#  geom_bar(fill = "#98362b", stat = "identity", width = 0.7) +
#  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

PresençaC1 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ DELTA Q MYTHIQ 80CAP`)
PresençaC1<-na.omit(PresençaC1)
PresençaC1

FreqC1 <- table(PresençaC1$`SINAL CAFÉ DELTA Q MYTHIQ 80CAP`)
dfPercC1<-data.frame(prop.table(FreqC1))
dfPercC1<-na.omit(dfPercC1)


GPresença1 <- ggplot(dfPercC1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#662867", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

GPresença1
#Produto2

PresençaC2 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ DELTA Q QALIDUS 80CAP`)
PresençaC2<-na.omit(PresençaC2)

FreqC2 <- table(PresençaC2$`SINAL CAFÉ DELTA Q QALIDUS 80CAP`)
dfPercC2<-data.frame(prop.table(FreqC2))

GPresença2 <- ggplot(dfPercC2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#662867", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#Produto3

PresençaC3 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ DELTA Q QALIDUS 10CAP`)
PresençaC3<-na.omit(PresençaC3)

FreqC3 <- table(PresençaC3$`SINAL CAFÉ DELTA Q QALIDUS 10CAP`)
dfPercC3<-data.frame(prop.table(FreqC3))

GPresença3<- ggplot(dfPercC3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9e2f24", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto4

PresençaC4 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)
PresençaC4<-na.omit(PresençaC4)

FreqC4 <- table(PresençaC4$`SINAL CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)
dfPercC4<-data.frame(prop.table(FreqC4))

GPresença4<- ggplot(dfPercC4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#4b3b3b", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto5

PresençaC5 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ BELLISSIMO INTENSO 200GR`)
PresençaC5<-na.omit(PresençaC5)

FreqC5 <- table(PresençaC5$`SINAL CAFÉ BELLISSIMO INTENSO 200GR`)
dfPercC5<-data.frame(prop.table(FreqC5))

GPresença5<- ggplot(dfPercC5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#b24159", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto6

PresençaC6 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ DELTA RITUAL MU 220G`)
PresençaC6<-na.omit(PresençaC6)

FreqC6 <- table(PresençaC6$`SINAL CAFÉ DELTA RITUAL MU 220G`)
dfPercC6<-data.frame(prop.table(FreqC6))

GPresença6<- ggplot(dfPercC6, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#222222", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto7

PresençaC7 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ SOLÚVEL DELTA FRASCO 100G`)
PresençaC7<-na.omit(PresençaC7)

FreqC7 <- table(PresençaC7$`SINAL CAFÉ SOLÚVEL DELTA FRASCO 100G`)
dfPercC7<-data.frame(prop.table(FreqC7))

GPresença7<- ggplot(dfPercC7, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto8

PresençaC8 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ SOLÚVEL DELTA FRASCO 200G`)
PresençaC8<-na.omit(PresençaC8)

FreqC8 <- table(PresençaC8$`SINAL CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfPercC8<-data.frame(prop.table(FreqC8))

GPresença8<- ggplot(dfPercC8, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto9

PresençaC9 <- DATAFUSION %>%
  dplyr::select(`SINAL CEVADA SOLÚVEL DELTA FRASCO 200G`)
PresençaC9<-na.omit(PresençaC9)

FreqC9 <- table(PresençaC9$`SINAL CEVADA SOLÚVEL DELTA FRASCO 200G`)
dfPercC9<-data.frame(prop.table(FreqC9))

GPresença9<- ggplot(dfPercC9, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ee9b25", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto10

PresençaC10 <- DATAFUSION %>%
  dplyr::select(`SINAL BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)
PresençaC10<-na.omit(PresençaC10)

FreqC10 <- table(PresençaC10$`SINAL BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)
dfPercC10<-data.frame(prop.table(FreqC10))

GPresença10<- ggplot(dfPercC10, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ad4a23", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


# Junção

Ep1 <- subplot(GPresença2, GPresença3, GPresença4)
Ep2 <- subplot(GPresença5, GPresença6, GPresença7)
Ep3 <- subplot(GPresença8, GPresença9, GPresença10)

#subplot(Ep1, Ep2, Ep3, nrows = 2, widths = 1, heights = NULL, margin = 0.02)




# Tentativa de automatizar
#var_names <- c("PresençaC2", "PresençaC2", "PresençaC3", "PresençaC4", "PresençaC5")

# loop over variable names and create the variables
#for (i in 1:length(var_names)) {
  # select and clean the data
#  var_names[i] <- DATAFUSION %>%
#    dplyr::select(!!sym(paste("`", var_names[i], "`", sep=""))) %>%
#    na.omit()
  
  # calculate the Frequency and percentage
#  Freq <- table(var_names[i][[1]])
#  dfPercC <- data.frame(prop.table(Freq))
  
  # create the plot
#  assign(paste("E2", i, sep=""), ggplot(dfPercC, aes(x = Var1, y = Freq)) +
#           geom_bar(fill = "#98362b", stat = "identity", width = 0.7) +
#           scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#           labs(title = "", x = "", y = "") + 
#           theme(panel.background = element_blank()))
#}




### CICLOS ###




#Produto1 

ciclo1 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q MYTHIQ 80CAP`)
ciclo1<-na.omit(ciclo1)

C1 <- ggplot(ciclo1, aes(x = `CICLO CAFÉ DELTA Q MYTHIQ 80CAP`)) +
  geom_bar(fill = "#735b4d", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q MYTHIQ 80CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())



#Produto2

ciclo2 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q QALIDUS 80CAP`)
ciclo2<-na.omit(ciclo2)

C2 <- ggplot(ciclo2, aes(x = `CICLO CAFÉ DELTA Q QALIDUS 80CAP`)) +
  geom_bar(fill = "#98362b", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q QALIDUS 80CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto3

ciclo3 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q QALIDUS 10CAP`)
ciclo3<-na.omit(ciclo3)

C3<- ggplot(ciclo3, aes(x = `CICLO CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q QALIDUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto4

ciclo4 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)
ciclo4<-na.omit(ciclo4)

C4<- ggplot(ciclo4, aes(x = `CICLO CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto5
ciclo5 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ BELLISSIMO INTENSO 200GR`)
ciclo5<-na.omit(ciclo5)
C5 <- ggplot(ciclo5, aes(x = `CICLO CAFÉ BELLISSIMO INTENSO 200GR`)) +
  geom_bar(fill = "#b24159", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto6

ciclo6 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA RITUAL MU 220G`)
ciclo6<-na.omit(ciclo6)

C6<- ggplot(ciclo6, aes(x = `CICLO CAFÉ DELTA RITUAL MU 220G`)) +
  geom_bar(fill = "#222222", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA RITUAL MU 220G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto7

ciclo7 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ SOLÚVEL DELTA FRASCO 100G`)
ciclo7<-na.omit(ciclo7)

C7<- ggplot(ciclo7, aes(x = `CICLO CAFÉ SOLÚVEL DELTA FRASCO 100G`)) +
  geom_bar(fill = "#710302", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ SOLÚVEL DELTA FRASCO 100G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto8

ciclo8 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
ciclo8<-na.omit(ciclo8)

C8<- ggplot(ciclo8, aes(x = `CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "#710302", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto9

ciclo9 <-DATAFUSION %>%
  dplyr::select(`CICLO CEVADA SOLÚVEL DELTA FRASCO 200G`)
ciclo9<-na.omit(ciclo9)

C9<- ggplot(ciclo9, aes(x = `CICLO CEVADA SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "#ee9b25", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CEVADA SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto10

ciclo10 <-DATAFUSION %>%
  dplyr::select(`CICLO BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)
ciclo10<-na.omit(ciclo10)

C10<- ggplot(ciclo10, aes(x = `CICLO BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) +
  geom_bar(fill = "#ad4a23", position = "stack") +
  labs(title = "`CICLO ASSEGURADO BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())


#gráficos
Cp1<-subplot(C1, C2, C3)
Cp2<-subplot(C4, C5, C6)
Cp3<-subplot(C7, C8, C9, C10)

subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)






###   Rotura  ###



#Produto1  

rotura1 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q MYTHIQ 80CAP`)
rotura1<-na.omit(rotura1)

rot1 <- table(rotura1$`ROTURA CAFÉ DELTA Q MYTHIQ 80CAP`)
dfRot1<-data.frame(prop.table(rot1))
dfRot1<-na.omit(dfRot1)


R11 <- ggplot(dfRot1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#b24159", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())




#Produto2

rotura2 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q QALIDUS 80CAP`)
rotura2<-na.omit(rotura2)

rot2 <- table(rotura2$`ROTURA CAFÉ DELTA Q QALIDUS 80CAP`)
dfRot2<-data.frame(prop.table(rot2))
dfRot2<-na.omit(dfRot2)


R21 <- ggplot(dfRot2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#662867", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())




#Produto3

rotura3 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`)
rotura3<-na.omit(rotura3)

rot3 <- table(rotura3$`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`)
dfRot3<-data.frame(prop.table(rot3))
dfRot3<-na.omit(dfRot3)


R31 <- ggplot(dfRot3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9e2f24", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto4

rotura4 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)
rotura4<-na.omit(rotura4)

rot4 <- table(rotura4$`ROTURA CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)
dfRot4<-data.frame(prop.table(rot4))
dfRot4<-na.omit(dfRot4)


R41 <- ggplot(dfRot4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#4b3b3b", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#Produto5

rotura5 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ BELLISSIMO INTENSO 200GR`)
rotura5<-na.omit(rotura5)

rot5 <- table(rotura5$`ROTURA CAFÉ BELLISSIMO INTENSO 200GR`)
dfRot5<-data.frame(prop.table(rot5))
dfRot5<-na.omit(dfRot5)


R51 <- ggplot(dfRot5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#b24159", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R51 <- ggplot(rotura5, aes(x = `ROTURA CAFÉ BELLISSIMO INTENSO 200GR`)) +
#  geom_bar(fill = "#b24159", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto6

rotura6 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA RITUAL MU 220G`)
rotura6<-na.omit(rotura6)

rot6 <- table(rotura6$`ROTURA CAFÉ DELTA RITUAL MU 220G`)
dfRot6<-data.frame(prop.table(rot6))
dfRot6<-na.omit(dfRot6)


R61 <- ggplot(dfRot6, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#222222", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R61 <- ggplot(rotura6, aes(x = `ROTURA CAFÉ DELTA RITUAL MU 220G`)) +
#  geom_bar(fill = "#222222", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto7

rotura7 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 100G`)
rotura7<-na.omit(rotura7)

rot7 <- table(rotura7$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 100G`)
dfRot7<-data.frame(prop.table(rot7))
dfRot7<-na.omit(dfRot7)


R71 <- ggplot(dfRot7, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R71 <- ggplot(rotura7, aes(x = `ROTURA CAFÉ SOLÚVEL DELTA FRASCO 100G`)) +
#  geom_bar(fill = "#710302", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto8

rotura8 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)
rotura8<-na.omit(rotura8)

rot8 <- table(rotura8$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfRot8<-data.frame(prop.table(rot8))
dfRot8<-na.omit(dfRot8)


R81 <- ggplot(dfRot8, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R81 <- ggplot(rotura8, aes(x = `ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
#  geom_bar(fill = "#710302", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto9

rotura9 <-DATAFUSION %>%
  dplyr::select(`ROTURA CEVADA SOLÚVEL DELTA FRASCO 200G`)
rotura9<-na.omit(rotura9)

rot9 <- table(rotura9$`ROTURA CEVADA SOLÚVEL DELTA FRASCO 200G`)
dfRot9<-data.frame(prop.table(rot9))
dfRot9<-na.omit(dfRot9)


R91 <- ggplot(dfRot9, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ee9b25", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R91 <- ggplot(rotura9, aes(x = `ROTURA CEVADA SOLÚVEL DELTA FRASCO 200G`)) +
#  geom_bar(fill = "#ee9b25", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto10

rotura10 <-DATAFUSION %>%
  dplyr::select(`ROTURA BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)
rotura10<-na.omit(rotura10)

rot10 <- table(rotura10$`ROTURA BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)
dfRot10<-data.frame(prop.table(rot10))
dfRot10<-na.omit(dfRot10)


R101 <- ggplot(dfRot10, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ad4a23", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R101 <- ggplot(rotura10, aes(x = `ROTURA BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) +
#  geom_bar(fill = "#ad4a23", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#gráficos
Rp1<-subplot(R11, R21, R31)
Rp2<-subplot(R41, R51, R61)
Rp3<-subplot(R71, R81, R91, R101)
subplot(Rp1, Rp2, Rp3, nrows=3, widths = 1, heights = NULL, margin = 0.02)






### ADEQUAÇÃO DE STOCK ###




#Produto1  

adequa1 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q MYTHIQ 80CAP`)
adequa1<-na.omit(adequa1)

Freq1 <- table(adequa1$`ADEQUAÇÃO CAFÉ DELTA Q MYTHIQ 80CAP`)
perce1<- Freq1/sum(Freq1)*100
dfAdPerc1<-data.frame(prop.table(Freq1))


A11 <- ggplot(dfAdPerc1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#98362b", stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto2

adequa2 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 80CAP`)
adequa2<-na.omit(adequa2)

Freq2 <- table(adequa2$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 80CAP`)
dfAdPerc2<-data.frame(prop.table(Freq2))

A21 <- ggplot(dfAdPerc2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#662867", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto3

adequa3 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`)
adequa3<-na.omit(adequa3)

Freq3 <- table(adequa3$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`)
dfAdPerc3<-data.frame(prop.table(Freq3))

A31<- ggplot(dfAdPerc3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9e2f24", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto4

adequa4 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)
adequa4<-na.omit(adequa4)

Freq4 <- table(adequa4$`ADEQUAÇÃO CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)
dfAdPerc4<-data.frame(prop.table(Freq4))

A41<- ggplot(dfAdPerc4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#4b3b3b", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())


#Produto5

adequa5 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ BELLISSIMO INTENSO 200GR`)
adequa5<-na.omit(adequa5)

Freq5 <- table(adequa5$`ADEQUAÇÃO CAFÉ BELLISSIMO INTENSO 200GR`)
dfAdPerc5<-data.frame(prop.table(Freq5))

A51<-ggplot(dfAdPerc5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#b24159", stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto6

adequa6 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA RITUAL MU 220G`)
adequa6<-na.omit(adequa6)

Freq6 <- table(adequa6$`ADEQUAÇÃO CAFÉ DELTA RITUAL MU 220G`)
dfAdPerc6<-data.frame(prop.table(Freq6))

A61 <- ggplot(dfAdPerc6, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#222222", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto7

adequa7 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 100G`)
adequa7<-na.omit(adequa7)

Freq7 <- table(adequa7$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 100G`)
dfAdPerc7<-data.frame(prop.table(Freq7))

A71 <- ggplot(dfAdPerc7, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto8

adequa8 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
adequa8<-na.omit(adequa8)

Freq8 <- table(adequa8$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfAdPerc8<-data.frame(prop.table(Freq8))

A81 <- ggplot(dfAdPerc8, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto9

adequa9 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CEVADA SOLÚVEL DELTA FRASCO 200G`)
adequa9<-na.omit(adequa9)

Freq9 <- table(adequa9$`ADEQUAÇÃO CEVADA SOLÚVEL DELTA FRASCO 200G`)
dfAdPerc9<-data.frame(prop.table(Freq9))

A91 <- ggplot(dfAdPerc9, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ee9b25", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto10

adequa10 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)
adequa10<-na.omit(adequa10)

Freq10 <- table(adequa10$`ADEQUAÇÃO BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)
dfAdPerc10<-data.frame(prop.table(Freq10))


A101 <- ggplot(dfAdPerc10, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ad4a23", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())


#gráficos
Ap1<-subplot(A11, A21)
Ap2<-subplot(A31, A41, A51)
Ap3<-subplot(A61, A71)
Ap4<-subplot(A81, A91)#, A101)
subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)


#ggplotly(Ep1)


### GRÁFICOS ###

#Presença de produto   Solução 1x5x1 
#P1<-subplot(G5,G4,G3,G2,G1)
#subplot(Classic, P1, FINAL, nrows = 3, widths = 0.9, heights = NULL, margin = 0.009)

#SINAL do café (Stock e ninjas)
subplot(Ep1, Ep2, nrows=2, widths = 1, heights = NULL, margin = 0.02)

#Ciclos de reposição assegurados
subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Rotura de stock
subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Adequação de Stock
subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)

manter <- c("Ep1", "Ep2", "Cp1", "Cp2", "Rp1", "Rp2", "Ap1", "Ap2", "Ap3")



#######CICLOS DE REPOSIÇÃO


Ci1<-ggplot(ciclo1, aes(x="", y=`CICLO CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#98362b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")


Ci2<-ggplot(ciclo2, aes(x="", y=`CICLO CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci3<-ggplot(ciclo3, aes(x="", y=`CICLO CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#9e2f24", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci4<-ggplot(ciclo4, aes(x="", y=`CICLO CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#4b3b3b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci5<-ggplot(ciclo5, aes(x="", y=`CICLO CAFÉ BELLISSIMO INTENSO 200GR`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#b24159", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci6<-ggplot(ciclo6, aes(x="", y=`CICLO CAFÉ DELTA RITUAL MU 220G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#222222", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci7<-ggplot(ciclo7, aes(x="", y=`CICLO CAFÉ SOLÚVEL DELTA FRASCO 100G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci8<-ggplot(ciclo8, aes(x="", y=`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci9<-ggplot(ciclo9, aes(x="", y=`CICLO CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ee9b25", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci10<-ggplot(ciclo10, aes(x="", y=`CICLO BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ad4a23", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")



######SELLOUTS 2 ANOS


attach(DATAFUSION2022)


SO1<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#98362b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO11<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#98362b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO2<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO21<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO3<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#9e2f24", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO31<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#9e2f24", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO4<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#4b3b3b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO41<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#4b3b3b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO5<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ BELLISSIMO INTENSO 200GR`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#b24159", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO51<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ BELLISSIMO INTENSO 200GR`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#b24159", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO6<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA RITUAL MU 220G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#222222", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO61<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ DELTA RITUAL MU 220G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#222222", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO7<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ SOLÚVEL DELTA FRASCO 100G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO71<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ SOLÚVEL DELTA FRASCO 100G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO8<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO81<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO9<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ee9b25", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO91<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ee9b25", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO10<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ad4a23", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO101<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ad4a23", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

ggplotly(SO101)






#### DIAS PARA A ROTURA


DR1<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#98362b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

ggplotly(DR1)


DR2<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR3<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  
  geom_boxplot(width=0.1, fill="#9e2f24", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR4<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) + 
  geom_boxplot(width=0.1, fill="#4b3b3b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR5<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ BELLISSIMO INTENSO 200GR`)) + 
  geom_boxplot(width=0.1, fill="#b24159", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR6<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA RITUAL MU 220G`)) + 
  geom_boxplot(width=0.1, fill="#222222", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR7<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ SOLÚVEL DELTA FRASCO 100G`)) + 
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR8<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR9<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_boxplot(width=0.1, fill="#ee9b25", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR10<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) + 
  geom_boxplot(width=0.1, fill="#ad4a23", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR10





#### DIAS PARA A ROTURA DE LINEAR



DRL1<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#98362b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

ggplotly(DRL1)


DRL2<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL3<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#9e2f24", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL4<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ MOÍDO MÁQUINA CHÁVENA DELTA 250G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#4b3b3b", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL5<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ BELLISSIMO INTENSO 200GR`)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#b24159", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL6<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA RITUAL MU 220G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#222222", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL7<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ SOLÚVEL DELTA FRASCO 100G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL8<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL9<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CEVADA SOLÚVEL DELTA FRASCO 200G`)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ee9b25", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL10<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear BEBIDA CEREAIS DELTA C/20%CAFE FR 200G`)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ad4a23", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")













#rm(list = setdiff(ls(), manter))

### Exportar ficheiro CSV###
#write.csv(DATAFUSION, "DATAFUSIONFinal.csv",row.names = FALSE, sep=";")