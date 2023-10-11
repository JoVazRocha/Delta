####Stocks SONAE fim de semana 24,25,26###

library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)

# Ler ficheiros


DataFusion <- read_excel("D:\\B&N Dados\\Delta\\Mar-abril23\\zFinalWide2.xlsx")
Produtos <- read.table("D:\\B&N Dados\\Delta\\Mar-abril23\\zProdutos2.txt", sep='\t', header=FALSE)
Promos<- c("Promoção")


dfMedia <- read_excel("D:\\B&N Dados\\Delta\\Mar-abril23\\zgraficos.xlsx")
 

DATAFUSION <- data.frame(DataFusion)
attach(DATAFUSION)


#Ordem dos elementos
#1º Elemento: Promoção
#Último elemento: CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL


#Nº de Promoções: 1
#promo1 <- Promoção



#Nº de Produtos: 5
#produto1 <- "CAFÉ DELTA Q QALIDUS 10CAP"
#produto2 <- "CAFÉ DELTA Q AQTIVUS 10CAP"
#produto3 <- "CAFÉ DELTA Q DEQAFEINATUS 10CAP"
#produto4 <- "CERV.C/ALC.T/P CORONA 35,5CL"
#produto5 <- "CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL"

#produto6 <- ""
#produto7 <- ""
#produto8 <- ""
#produto9 <- ""
#produto10 <- ""





#GRÁFICOS


#GRÁFICO_Clássico -> Presença em Linear para cada elemento em análise

Classic <- ggplot(dfMedia, aes(x=Frequência*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#17706E")+
  geom_text(aes(label = paste0(round(Frequência * 100), "%")), hjust = -0.2)+
  labs(title= "Presença de Produto em Linear verificado pelo Ninja")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.background = element_blank())


ggplotly(Classic)

Classic<- Classic+ annotation_custom(rasterGrob(img),            #O que é isto?
 xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 14) +
 scale_y_continuous(limits = c(0, 14))




#COM REPO
install.packages("flexdashboards")

PresençaPercSL <- data.frame(Produto = c("PROMOÇÃO PRESENTE", "CAFÉ DELTA Q QALIDUS 10CAP", "CAFÉ DELTA Q AQTIVUS 10CAP", 
                               "CAFÉ DELTA Q DEQAFEINATUS 10CAP", "CERV.C/ALC.T/P CORONA 35,5CL", 
                               "CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL"),
                   PresençaPerc = c(63.15789474, 94.73684211, 94.73684211, 89.47368421, 100, 100))


ClassicSL <- ggplot(PresençaPercSL, aes(x=PresençaPerc, y=Produto))+
  geom_bar(stat = "identity", fill="#FFA07A")+
  geom_text(aes(label = paste0(round(PresençaPerc), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


CLASSICR<-ClassicSL+theme(panel.background = element_blank())

PresençaPercNL <- data.frame(Produto = c("PROMOÇÃO PRESENTE", "CAFÉ DELTA Q QALIDUS 10CAP", "CAFÉ DELTA Q AQTIVUS 10CAP", 
                                         "CAFÉ DELTA Q DEQAFEINATUS 10CAP", "CERV.C/ALC.T/P CORONA 35,5CL", 
                                         "CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL"),
                             PresençaPerc = c(44.34783, 92.17391, 98.26087, 93.043, 95.94595, 98.64865))


ClassicNL <- ggplot(PresençaPercNL, aes(x=PresençaPerc, y=Produto))+
  geom_bar(stat = "identity", fill="#6495ED")+
  geom_text(aes(label = paste0(round(PresençaPerc), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

CLASSICRC<-ClassicNL+theme(panel.background = element_blank())

# O que é o repo_s?

FINALSL<-ggplot(data = Repo_S, aes(x = `Nome da Loja`, y = `Total`)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  labs(x = "Ponto de Venda", y = "") + theme(axis.text.x = element_blank(), panel.background = element_blank())
FINALSL

FINALNL<-ggplot(data = Repo_N, aes(x = `Nome da Loja`, y = `Total`)) +
  geom_bar(stat = "identity", fill = "#6495ED") +
  labs(x = "Ponto de Venda", y = "") + theme(axis.text.x = element_blank(), panel.background = element_blank())







#GRÁFICO POR FIM DE SEMANA


#Promoção

perc1 <- aggregate(`Promoção` ~ DATA,DATAFUSION, mean)
G1<-ggplot(perc1, aes(x = DATA, y = `Promoção`)) + 
  geom_col(fill="#704f78") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())
  

F1<-ggplot(data = DATAFUSION, aes(x = Loja, y = `Promoção`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

G1
#Produto1

perc2 <- aggregate(`CAFÉ DELTA Q QALIDUS 10CAP` ~ DATA,DATAFUSION, mean)
G2<-ggplot(perc2, aes(x = DATA, y = `CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_col(fill="#735b4d") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())
 

F2<-ggplot(data = DATAFUSION, aes(x = Loja, y = `CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto2

perc3 <- aggregate(`CAFÉ DELTA Q AQTIVUS 10CAP` ~ DATA,DATAFUSION, mean)
G3<-ggplot(perc3, aes(x = DATA, y = `CAFÉ DELTA Q AQTIVUS 10CAP`)) + 
  geom_col(fill="#9d5c52") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())


F3<-ggplot(data = DATAFUSION, aes(x = Loja, y = `CAFÉ DELTA Q AQTIVUS 10CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto3

perc4 <- aggregate(`CAFÉ DELTA Q DEQAFEINATUS 10CAP` ~ DATA,DATAFUSION, mean)
G4<-ggplot(perc4, aes(x = DATA, y = `CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) + 
  geom_col(fill="#4b625a") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())


F4<-ggplot(data = DATAFUSION, aes(x = Loja, y = `CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto4

perc5 <- aggregate(`CERV.C/ALC.T/P CORONA 35,5CL` ~ DATA,DATAFUSION, mean)
G5<-ggplot(perc5, aes(x = DATA, y = `CERV.C/ALC.T/P CORONA 35,5CL`)) + 
  geom_col(fill="#e73b37") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())


F5<-ggplot(data = DATAFUSION, aes(x = Loja, y = `CERV.C/ALC.T/P CORONA 35,5CL`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto5

perc6 <- aggregate(`CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL` ~ DATA,DATAFUSION, mean)
G6<-ggplot(perc6, aes(x = DATA, y = `CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) + 
  geom_col(fill="#e73b37") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())


F6<-ggplot(data = DATAFUSION, aes(x = Loja, y = `CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Total

FINAL<-ggplot(data = DATAFUSION, aes(x = Loja, y = `Presença_em_Percentagem`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Percentagem de produtos Delta no ponto de venda") + theme(axis.text.x = element_blank(), panel.background = element_blank())

ggplotly(FINAL)





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

#PROMOÇÕES
padrãoPromo <- "[nome]"

#Promoção1

promo1 <- gsub("\\[nome\\]", Promos[1], padrãoPromo)

Presença1 <- DATAFUSION %>%
  dplyr::select(all_of(promo1))             #Isolar a promoção e tirar valores em falta 
Presença1<-na.omit(Presença1)

Freq1 <- table(Presença1$`Promoção`)
dfPerc1<-data.frame(prop.table(Freq1))  # Fazer a Frequência de cada um dos casos
names(dfPerc1) <- c(Promos[1], "Freq")

GPromo1 <- ggplot(dfPerc1, aes(x = Promoção, y = Freq)) +  # Não consigo mudar aqui....!!!
  geom_bar(fill = "#40E0D0", stat = "identity") + 
  geom_text(aes(label = paste0(round(Freq * 100), "%"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + 
  theme(panel.background = element_blank())



#PRODUTOS
padrãoSinal<- "Sinal [nome]"

#Produto1

sinal1<-gsub("\\[nome\\]", Produtos[1,], padrãoSinal)
sinal1
PresençaC1 <- DATAFUSION %>%
  dplyr::select(all_of(sinal1))
PresençaC1<-na.omit(PresençaC1)

FreqC2 <- table(PresençaC1$`Sinal CAFÉ DELTA Q QALIDUS 10CAP`)  #Também não consigo mudar aqui
dfPercC1<-data.frame(prop.table(FreqC2))
names(dfPercC1) <- c(Promos[1], "Freq")

GPresença1 <- ggplot(dfPercC1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9d5c52", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())




#Produto2

PresençaC2 <- DATAFUSION %>%
  dplyr::select(`Sinal CAFÉ DELTA Q AQTIVUS 10CAP`)
PresençaC2<-na.omit(PresençaC2)

FreqC2 <- table(PresençaC2$`Sinal CAFÉ DELTA Q AQTIVUS 10CAP`)
dfPercC2<-data.frame(prop.table(FreqC2))

GPresença2 <- ggplot(dfPercC2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#fe6900", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#Produto3

PresençaC3 <- DATAFUSION %>%
  dplyr::select(`Sinal CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
PresençaC3<-na.omit(PresençaC3)

FreqC3 <- table(PresençaC3$`Sinal CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
dfPercC3<-data.frame(prop.table(FreqC3))

GPresença3<- ggplot(dfPercC3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#01aed8", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto4

PresençaC4 <- DATAFUSION %>%
  dplyr::select(`Sinal CERV.C/ALC.T/P CORONA 35,5CL`)
PresençaC4<-na.omit(PresençaC4)

FreqC4 <- table(PresençaC4$`Sinal CERV.C/ALC.T/P CORONA 35,5CL`)
dfPercC4<-data.frame(prop.table(FreqC4))

GPresença4<- ggplot(dfPercC4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#1a2246", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto5

PresençaC5 <- DATAFUSION %>%
  dplyr::select(`Sinal CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
PresençaC5<-na.omit(PresençaC5)

FreqC5 <- table(PresençaC5$`Sinal CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
dfPercC5<-data.frame(prop.table(FreqC5))

GPresença5<- ggplot(dfPercC5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e1cf98", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

# Junção

Ep1<-subplot(GPromo1, GPresença1, GPresença2)
Ep2<-subplot(GPresença3, GPresença4, GPresença5)

subplot(Ep1, Ep2, nrows=2, widths = 1, heights = NULL, margin = 0.02)



# Tentativa de automatizar
var_names <- c("PresençaC2", "PresençaC2", "PresençaC3", "PresençaC4", "PresençaC5")

# loop over variable names and create the variables
for (i in 1:length(var_names)) {
  # select and clean the data
  var_names[i] <- DATAFUSION %>%
    dplyr::select(!!sym(paste("`", var_names[i], "`", sep=""))) %>%
    na.omit()
  
  # calculate the Frequency and percentage
  Freq <- table(var_names[i][[1]])
  dfPercC <- data.frame(prop.table(Freq))
  
  # create the plot
  assign(paste("E2", i, sep=""), ggplot(dfPercC, aes(x = Var1, y = Freq)) +
           geom_bar(fill = "#9d5c52", stat = "identity", width = 0.7) +
           scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
           labs(title = "", x = "", y = "") + 
           theme(panel.background = element_blank()))
}




### CICLOS ###



#Produto1
ciclo1 <-DATAFUSION %>%
  dplyr::select(`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
ciclo1<-na.omit(ciclo1)
C61 <- ggplot(ciclo1, aes(x = `CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) +
  geom_bar(fill = "#e1cf98", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#Produto2 

ciclo2 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q QALIDUS 10CAP`)
ciclo2<-na.omit(ciclo2)

C2 <- ggplot(ciclo2, aes(x = `CICLO CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(fill = "#735b4d", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q QALIDUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())



#Produto3

ciclo3 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)
ciclo3<-na.omit(ciclo3)

C3 <- ggplot(ciclo3, aes(x = `CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)) +
  geom_bar(fill = "#9d5c52", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q AQTIVUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto4

ciclo4 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
ciclo4<-na.omit(ciclo4)

C4<- ggplot(ciclo4, aes(x = `CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q DEQAFEINATUS 10CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#Produto5

ciclo5 <-DATAFUSION %>%
  dplyr::select(`CICLO CERV.C/ALC.T/P CORONA 35,5CL`)
ciclo5<-na.omit(ciclo5)

C5<- ggplot(ciclo5, aes(x = `CICLO CERV.C/ALC.T/P CORONA 35,5CL`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CERV.C/ALC.T/P CORONA 35,5CL`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())


#gráficos
Cp1<-subplot(C1, C2, C3)
Cp2<-subplot(C4, C5)

#subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)







###   Rotura  ###



#Produto1

rotura1 <-DATAFUSION %>%
  dplyr::select(`ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
rotura1<-na.omit(rotura1)
R61 <- ggplot(rotura1, aes(x = `ROTURA CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) +
  geom_bar(fill = "#e1cf98", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto2  

rotura2 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q QALIDUS 10CAP`)
rotura2<-na.omit(rotura2)

R12 <- ggplot(rotura2, aes(x = `ROTURA CAFÉ DELTA Q QALIDUS 10CAP`)) +
  geom_bar(fill = "#e1cf98", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto3

rotura3 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`)
rotura3<-na.omit(rotura3)

R31 <- ggplot(rotura3, aes(x = `ROTURA CAFÉ DELTA Q AQTIVUS 10CAP`)) +
  geom_bar(fill = "#fe6900", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto4

rotura4 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
rotura4<-na.omit(rotura4)

R41<- ggplot(rotura4, aes(x = `ROTURA CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) +
  geom_bar(fill = "#01aed8", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto5

rotura5 <-DATAFUSION %>%
  dplyr::select(`ROTURA CERV.C/ALC.T/P CORONA 35,5CL`)
rotura5<-na.omit(rotura5)

R51<- ggplot(rotura5, aes(x = `ROTURA CERV.C/ALC.T/P CORONA 35,5CL`)) +
  geom_bar(fill = "#1a2246", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#gráficos
Rp1<-subplot(R1, R2, R3)
Rp2<-subplot(R4, R5)

#subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)






### ADEQUAÇÃO DE STOCK ###




#Produto1

adequa1 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
adequa1<-na.omit(adequa1)

Freq1 <- table(adequa1$`ADEQUAÇÃO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)
dfAdPerc1<-data.frame(prop.table(Freq1))

A61<-ggplot(dfAdPerc1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e1cf98", stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())


#Produto2  

adequa2 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`)
adequa2<-na.omit(adequa2)

Freq2 <- table(adequa2$`ADEQUAÇÃO CAFÉ DELTA Q QALIDUS 10CAP`)
perce2<- Freq2/sum(Freq2)*100
dfAdPerc2<-data.frame(prop.table(Freq2))


A21 <- ggplot(dfAdPerc2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#9d5c52", stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto3

adequa3 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`)
adequa3<-na.omit(adequa3)

Freq3 <- table(adequa3$`ADEQUAÇÃO CAFÉ DELTA Q AQTIVUS 10CAP`)
dfAdPerc3<-data.frame(prop.table(Freq3))

A31 <- ggplot(dfAdPerc3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#fe6900", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto4

adequa4 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
adequa4<-na.omit(adequa4)

Freq4 <- table(adequa4$`ADEQUAÇÃO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)
dfAdPerc4<-data.frame(prop.table(Freq4))

A41<- ggplot(dfAdPerc4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#01aed8", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto5

adequa5 <-DATAFUSION %>%
  dplyr::select(`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`)
adequa5<-na.omit(adequa5)

Freq5 <- table(adequa5$`ADEQUAÇÃO CERV.C/ALC.T/P CORONA 35,5CL`)
dfAdPerc5<-data.frame(prop.table(Freq5))

A51<- ggplot(dfAdPerc5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#1a2246", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())


#gráficos
Ap1<-subplot(A1, A2)
Ap2<-plot(A3)
Ap3<-subplot(A4, A5)

#subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)


ggplotly(Ep1)


### GRÁFICOS ###

#Presença de produto   Solução 1x5x1 
#P1<-subplot(G5,G4,G3,G2,G1)
#subplot(Classic, P1, FINAL, nrows = 3, widths = 0.9, heights = NULL, margin = 0.009)

#Sinal do café (Stock e ninjas)
subplot(Ep1, Ep2, nrows=2, widths = 1, heights = NULL, margin = 0.02)

#Ciclos de reposição assegurados
subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Rotura de stock
subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)

#Adequação de Stock
subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)

manter <- c("Ep1", "Ep2", "Cp1", "Cp2", "Rp1", "Rp2", "Ap1", "Ap2", "Ap3")



#######CICLOS DE REPOSIÇÃO


Ci1<-ggplot(ciclo2, aes(x="", y=`CICLO CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#9d5c52", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

ggplotly(Ci5)

Ci2<-ggplot(ciclo3, aes(x="", y=`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#fe6900", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci3<-ggplot(ciclo4, aes(x="", y=`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#01aed8", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci4<-ggplot(ciclo5, aes(x="", y=`CICLO CERV.C/ALC.T/P CORONA 35,5CL`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#1a2246", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci5<-ggplot(ciclo1, aes(x="", y=`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e1cf98", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")




######SELLOUTS 2 ANOS


attach(DataFusionComMed)


SO1<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#9d5c52", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO11<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CAFÉ DELTA Q QALIDUS 10CAP 22`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#9d5c52", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO2<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CAFÉ DELTA Q AQTIVUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#fe6900", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO21<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CAFÉ DELTA Q AQTIVUS 10CAP 22`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#fe6900", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO3<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#01aed8", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO31<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CAFÉ DELTA Q DEQAFEINATUS 10CAP 22`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#01aed8", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO4<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CERV.C/ALC.T/P CORONA 35,5CL`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#1a2246", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO41<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CERV.C/ALC.T/P CORONA 35,5CL 22`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#1a2246", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO5<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e1cf98", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO51<-ggplot(DataFusionComMed, aes(x="", y=`SELLOUT CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL 22`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e1cf98", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

ggplotly(SO5)


#### DIAS PARA A ROTURA


DR1<-ggplot(DataFusionComMed, aes(x="", y=`Dias para Rotura CAFÉ DELTA Q QALIDUS 10CAP`)) + 
  geom_boxplot(width=0.1, fill="#9d5c52", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

ggplotly(DR1)


DR2<-ggplot(DataFusionComMed, aes(x="", y=`CICLO CAFÉ DELTA Q AQTIVUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#fe6900", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR3<-ggplot(DataFusionComMed, aes(x="", y=`CICLO CAFÉ DELTA Q DEQAFEINATUS 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#01aed8", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR4<-ggplot(DataFusionComMed, aes(x="", y=`CICLO CERV.C/ALC.T/P CORONA 35,5CL`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#1a2246", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR5<-ggplot(DataFusionComMed, aes(x="", y=`CICLO CERV.C/ALC.T/P FRANZISKANER WEISSBI.50CL`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e1cf98", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

















rm(list = setdiff(ls(), manter))

### Exportar ficheiro CSV###
#write.csv(DATAFUSION, "DataFusionFinal.csv",row.names = FALSE, sep=";")