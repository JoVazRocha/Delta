
### Estudo Padrão Delta -> Maio 2023 ###


library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)

# Ler ficheiros

#Dados todos
Dados <- read_excel("D:\\B&N Dados\\Delta\\JM\\Padrao\\Padrao_08_2023\\Wide_DataFusion.xlsx")
#Dados_22 <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_xx_2023\\Wide_DataFusion2022.xlsx")


#Variáveis
#Produtos <- read.table("D:\\B&N Dados\\Delta\\xxxxxxxx.txt", sep='\t', header=FALSE)
#Promos<- c("Promoção")


#Gráficos

df_Classic <- read_excel("D:\\B&N Dados\\Delta\\JM\\Padrao\\Padrao_08_2023\\G_Classic.xlsx")

#df_ClassicRep <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_xx_2023\\G_Classic_Reposto.xlsx")
#df_ClassicSemRep <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_xx_2023\\G_Classic_SemReposto.xlsx")

df_NumProdutos <- read_excel("D:\\B&N Dados\\Delta\\JM\\Padrao\\Padrao_08_2023\\G_NumProdutos.xlsx")

df_Rotura <- read_excel("D:\\B&N Dados\\Delta\\JM\\Padrao\\Padrao_08_2023\\G_Rotura.xlsx")


DATAFUSION <- Dados
#DATAFUSION2022 <- Dados_22


#Ordem dos elementos
#1º Elemento: CERVEJA FRANZISKANER WEISSBIER 50 CL 5%
#Último elemento: PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP


#Nº de Produtos: 3
#produto1 <-  "#ff6504 CERVEJA FRANZISKANER WEISSBIER 50 CL 5%"  
#produto2 <-  "#662867 CERVEJA CORONA 35,5CL (UN)"  
#produto3 <-  "#e8e4c9 CERVEJA LEFFE BLONDE 33 CL CX24" 




#Nº de Promoções: 0



#GRÁFICOS


# CLÁSSICO -> Presença em Linear para cada elemento em análise


## tudo ##


Classic <- ggplot(df_Classic, aes(x=Frequência*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#17706E")+
  geom_text(aes(label = paste0(round(Frequência * 100), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.background = element_blank())


#Classic<- Classic+ annotation_custom(rasterGrob(img),            #O que é isto?
# xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 14) +
# scale_y_continuous(limits = c(0, 14))

Classic
#install.packages("flexdashboards")


# Proporção de Produtos presentes





#GRÁFICO POR FIM DE SEMANA

#G é gráfico de proporção de presença por produto por dia
#F é gráfico de número de vezes que foi detectada a presença do produto no linear por loja (se calhar devia fazer proporção)

#Produto1

perc1 <- aggregate(`CERVEJA FRANZISKANER WEISSBIER 50 CL 5%` ~ DATA,DATAFUSION, mean)
G1<-ggplot(perc1, aes(x = DATA, y = `CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)) + 
  geom_col(fill="#ff6504") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimQalidus<- ifelse(DATAFUSION$`CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`==0, 0.1, 1)
fim1 <- aggregate(`fimQalidus` ~ Loja,DATAFUSION, mean)
F1<-ggplot(data = fim1, aes(x = Loja, y = `fimQalidus`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

ggplotly(G1)

#Produto2

perc2 <- aggregate(`CERVEJA CORONA 35,5CL (UN)` ~ DATA,DATAFUSION, mean)
G2<-ggplot(perc2, aes(x = DATA, y = `CERVEJA CORONA 35,5CL (UN)`)) + 
  geom_col(fill="#662867") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimMythiq<- ifelse(DATAFUSION$`CERVEJA CORONA 35,5CL (UN)` ==0, 0.1, 1)
fim2 <- aggregate(`fimMythiq` ~ Loja,DATAFUSION, mean)
F2<-ggplot(data = fim2, aes(x = Loja, y = `fimMythiq`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


#Produto3

perc3 <- aggregate(`CERVEJA LEFFE BLONDE 33 CL CX24` ~ DATA,DATAFUSION, mean)
G3<-ggplot(perc3, aes(x = DATA, y = `CERVEJA LEFFE BLONDE 33 CL CX24`)) + 
  geom_col(fill="#ff6504") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimQharacter<- ifelse(DATAFUSION$`CERVEJA LEFFE BLONDE 33 CL CX24` ==0, 0.1, 1)
fim3 <- aggregate(`fimQharacter` ~ Loja,DATAFUSION, mean)
F3<-ggplot(data = fim3, aes(x = Loja, y = `fimQharacter`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


#Total

fim <- aggregate(`Prop_Produtos` ~ Loja,df_NumProdutos, mean)
FINAL<-ggplot(data = fim, aes(x = Loja, y = `Prop_Produtos`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "") + theme(axis.text.x = element_blank(), panel.background = element_blank())






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

#FreqC1 <- table(PresençaC1$`SINAL CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)  #Também não consigo mudar aqui
#dfPercC1<-data.frame(prop.table(FreqC1))
#names(dfPercC1) <- c(Promos[1], "Freq")

#GPresença1 <- ggplot(dfPercC1, aes(x = Var1, y = Freq)) +
#  geom_bar(fill = "#ff6504", stat = "identity", width = 0.7) +
#  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

PresençaC1 <- DATAFUSION %>%
  dplyr::select(`SINAL CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)
PresençaC1<-na.omit(PresençaC1)
PresençaC1

FreqC1 <- table(PresençaC1$`SINAL CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)
dfPercC1<-data.frame(prop.table(FreqC1))
dfPercC1<-na.omit(dfPercC1)


GPresença1 <- ggplot(dfPercC1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ff6504", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

GPresença1
#Produto2

PresençaC2 <- DATAFUSION %>%
  dplyr::select(`SINAL CERVEJA CORONA 35,5CL (UN)`)
PresençaC2<-na.omit(PresençaC2)

FreqC2 <- table(PresençaC2$`SINAL CERVEJA CORONA 35,5CL (UN)`)
dfPercC2<-data.frame(prop.table(FreqC2))

GPresença2 <- ggplot(dfPercC2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#662867", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#Produto3

PresençaC3 <- DATAFUSION %>%
  dplyr::select(`SINAL CERVEJA LEFFE BLONDE 33 CL CX24`)
PresençaC3<-na.omit(PresençaC3)

FreqC3 <- table(PresençaC3$`SINAL CERVEJA LEFFE BLONDE 33 CL CX24`)
dfPercC3<-data.frame(prop.table(FreqC3))

GPresença3<- ggplot(dfPercC3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e8e4c9", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

GPresença3

# Junção


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
#           geom_bar(fill = "#ff6504", stat = "identity", width = 0.7) +
#           scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#           labs(title = "", x = "", y = "") + 
#           theme(panel.background = element_blank()))
#}








###   Rotura  ###



#Produto1  

rotura1 <-DATAFUSION %>%
  dplyr::select(`ROTURA CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)
rotura1<-na.omit(rotura1)

rot1 <- table(rotura1$`ROTURA CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)
dfRot1<-data.frame(prop.table(rot1))
dfRot1<-na.omit(dfRot1)


R11 <- ggplot(dfRot1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#3a2120", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())





#Produto2

rotura2 <-DATAFUSION %>%
  dplyr::select(`ROTURA CERVEJA CORONA 35,5CL (UN)`)
rotura2<-na.omit(rotura2)

rot2 <- table(rotura2$`ROTURA CERVEJA CORONA 35,5CL (UN)`)
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
  dplyr::select(`ROTURA CERVEJA LEFFE BLONDE 33 CL CX24`)
rotura3<-na.omit(rotura3)

rot3 <- table(rotura3$`ROTURA CERVEJA LEFFE BLONDE 33 CL CX24`)
dfRot3<-data.frame(prop.table(rot3))
dfRot3<-na.omit(dfRot3)


R31 <- ggplot(dfRot3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e8e4c9", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())




#ggplotly(Ep1)


### GRÁFICOS ###

#Presença de produto   Solução 1x5x1 
#P1<-subplot(G5,G4,G3,G2,G1)
#subplot(Classic, P1, FINAL, nrows = 3, widths = 0.9, heights = NULL, margin = 0.009)



######SELLOUTS 2 ANOS




SO1<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ff6504", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO2<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CERVEJA CORONA 35,5CL (UN)`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO3<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CERVEJA LEFFE BLONDE 33 CL CX24`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e8e4c9", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")





#### DIAS PARA A ROTURA


DR1<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CERVEJA FRANZISKANER WEISSBIER 50 CL 5%`)) + 
  geom_boxplot(width=0.1, fill="#ff6504", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

ggplotly(DR1)


DR2<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CERVEJA CORONA 35,5CL (UN)`)) + 
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR3<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CERVEJA LEFFE BLONDE 33 CL CX24`)) + 
  
  geom_boxplot(width=0.1, fill="#e8e4c9", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")





##### Balanço



sessionInfo()





#rm(list = setdiff(ls(), manter))

### Exportar ficheiro CSV###
#write.csv(DATAFUSION, "DATAFUSIONFinal.csv",row.names = FALSE, sep=";")