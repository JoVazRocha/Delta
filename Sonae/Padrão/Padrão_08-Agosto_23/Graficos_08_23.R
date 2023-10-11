
### Estudo Padrão Delta -> Maio 2023 ###


library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)

# Ler ficheiros

#Dados todos
Dados <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_08_2023\\Wide_DataFusion2.xlsx")
Dados_22 <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_08_2023\\Wide_DataFusion2022.xlsx")


#Variáveis
#Produtos <- read.table("D:\\B&N Dados\\Delta\\xxxxxxxx.txt", sep='\t', header=FALSE)
#Promos<- c("Promoção")


#Gráficos

df_Classic <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_08_2023\\G_Classic.xlsx")
df_ClassicRep <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_08_2023\\G_Classic_Reposto.xlsx")
df_ClassicSemRep <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_08_2023\\G_Classic_SemReposto.xlsx")

df_NumProdutos <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_08_2023\\G_NumProdutos.xlsx")

df_Rotura <- read_excel("D:\\B&N Dados\\Delta\\Padrão\\Padrão_08_2023\\G_Rotura.xlsx")


DATAFUSION <- Dados
DATAFUSION2022 <- Dados_22


#Ordem dos elementos
#1º Elemento: CAFÉ DELTA Q EPIQ 80CAP
#Último elemento: CAFÉ SOLÚVEL DELTA FRASCO 200G


#Nº de Produtos: 10
#produto1 <-  "#ff6504 CAFÉ DELTA Q EPIQ 80CAP"  
#produto2 <-  "#662867 CAFÉ DELTA Q MYTHIQ 80CAP"  
#produto3 <-  "#e8e4c9 CAFÉ BELLISSIMO AROMA INTENSO 30CAP" x
#produto4 <-  "#c42d1d CAFÉ GINGA 10CAP"x  
#produto5 <-  "#3a2120 CAFÉ DELTA Q QALIDUS 80CAP"  
#produto6 <-  "#dedec7 CAFÉ DELTA Q QHARACTER 80CAP" 
#produto7 <-  "#dedec7 CEVADA SOLÚVEL DELTA FRASCO 200G" 
#produto8 <-  "#24a01a CAFÉ BELLISSIMO INTENSO 10 CAP" 
#produto9 <-  "#b26628 CAFÉ BELLISSIMO ORIGINALE 10 CAP" 
#produto10 <- "#710302 CAFÉ SOLÚVEL DELTA FRASCO 200G" x
#produto11 <- "#564d49 CAFFELATTE GO CHILL DELTA 230 ML"
#produto12 <- "#98584f CAPPUCCINO GO CHILL DELTA 230 ML"
#produto13 <- "#6d4c76 CARAMEL MACCHIATO GO CHILL DELTA 230ML"
#produto14 <- "#715a49 LATTE MACCHIATO GO CHILL S/LAC 230ML"


#Nº de Promoções: 0



#GRÁFICOS


# CLÁSSICO -> Presença em Linear para cada elemento em análise


## tudo ##
df_Classic

Classic <- ggplot(df_Classic, aes(x=Frequencia*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#17706E")+
  geom_text(aes(label = paste0(round(Frequencia * 100), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.background = element_blank())


#Classic<- Classic+ annotation_custom(rasterGrob(img),            #O que é isto?
# xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 14) +
# scale_y_continuous(limits = c(0, 14))

Classic
#install.packages("flexdashboards")


## com reposição ##

ClassicSL <- ggplot(df_ClassicRep, aes(x=Frequencia*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#FFA07A")+
  geom_text(aes(label = paste0(round(Frequencia*100), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

CLASSICR<-ClassicSL+theme(panel.background = element_blank())
ggplotly(CLASSICR)

## sem reposição ##

ClassicNL <- ggplot(df_ClassicSemRep, aes(x=Frequencia*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="#6495ED")+
  geom_text(aes(label = paste0(round(Frequencia*100.0), "%")), hjust = -0.2)+
  labs(title= "")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

CLASSICRC<-ClassicNL+theme(panel.background = element_blank())



# Proporção de Produtos presentes


df_Reposto <- aggregate(`Prop_ProdutosRep` ~ Loja,df_NumProdutos, mean)
FINALSL<-ggplot(data = df_Reposto, aes(x = `Loja`, y = `Prop_ProdutosRep`)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  #geom_text(aes(label = paste0(round(Num_ProdutosRep), "%")), hjust = -0.2)+       # PODE SER TIRADO
  labs(x = "Ponto de Venda", y = "") + theme(axis.text.x = element_blank(), panel.background = element_blank())


df_NReposto <- aggregate(`Prop_ProdutosNRep` ~ Loja,df_NumProdutos, mean)
FINALNL<-ggplot(data = df_NReposto, aes(x = `Loja`, y = `Prop_ProdutosNRep`)) +
  geom_bar(stat = "identity", fill = "#6495ED") +
  labs(x = "Ponto de Venda", y = "") + theme(axis.text.x = element_blank(), panel.background = element_blank())








#GRÁFICO POR FIM DE SEMANA

#G é gráfico de proporção de presença por produto por dia
#F é gráfico de número de vezes que foi detectada a presença do produto no linear por loja (se calhar devia fazer proporção)

#Produto1

perc1 <- aggregate(`CAFÉ DELTA Q EPIQ 80CAP` ~ DATA,DATAFUSION, mean)
G1<-ggplot(perc1, aes(x = DATA, y = `CAFÉ DELTA Q EPIQ 80CAP`)) + 
  geom_col(fill="#ff6504") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimQalidus<- ifelse(DATAFUSION$`CAFÉ DELTA Q EPIQ 80CAP`==0, 0.1, 1)
fim1 <- aggregate(`fimQalidus` ~ Loja,DATAFUSION, mean)
F1<-ggplot(data = fim1, aes(x = Loja, y = `fimQalidus`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

ggplotly(G1)

#Produto2

perc2 <- aggregate(`CAFÉ DELTA Q MYTHIQ 80CAP` ~ DATA,DATAFUSION, mean)
G2<-ggplot(perc2, aes(x = DATA, y = `CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_col(fill="#662867") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimMythiq<- ifelse(DATAFUSION$`CAFÉ DELTA Q MYTHIQ 80CAP` ==0, 0.1, 1)
fim2 <- aggregate(`fimMythiq` ~ Loja,DATAFUSION, mean)
F2<-ggplot(data = fim2, aes(x = Loja, y = `fimMythiq`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


#Produto3

perc3 <- aggregate(`CAFÉ BELLISSIMO AROMA INTENSO 30CAP` ~ DATA,DATAFUSION, mean)
G3<-ggplot(perc3, aes(x = DATA, y = `CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)) + 
  geom_col(fill="#ff6504") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimQharacter<- ifelse(DATAFUSION$`CAFÉ BELLISSIMO AROMA INTENSO 30CAP` ==0, 0.1, 1)
fim3 <- aggregate(`fimQharacter` ~ Loja,DATAFUSION, mean)
F3<-ggplot(data = fim3, aes(x = Loja, y = `fimQharacter`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto4

perc4 <- aggregate(`CAFÉ GINGA 10CAP` ~ DATA,DATAFUSION, mean)
G4<-ggplot(perc4, aes(x = DATA, y = `CAFÉ GINGA 10CAP`)) + 
  geom_col(fill="#c42d1d") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimGinga<- ifelse(DATAFUSION$`CAFÉ GINGA 10CAP` ==0, 0.1, 1)

fim4 <- aggregate(`fimGinga` ~ Loja,DATAFUSION, mean)
F4<-ggplot(data = fim4, aes(x = Loja, y = `fimGinga`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto5

perc5 <- aggregate(`CAFÉ DELTA Q QALIDUS 80CAP` ~ DATA,DATAFUSION, mean)
G5<-ggplot(perc5, aes(x = DATA, y = `CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_col(fill="#3a2120") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimChav<- ifelse(DATAFUSION$`CAFÉ DELTA Q QALIDUS 80CAP` ==0, 0.1, 1)

fim5 <- aggregate(`fimChav` ~ Loja,DATAFUSION, mean)
F5<-ggplot(data = fim5, aes(x = Loja, y = `fimChav`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto6

perc6 <- aggregate(`CAFÉ DELTA Q QHARACTER 80CAP` ~ DATA,DATAFUSION, mean)
G6<-ggplot(perc6, aes(x = DATA, y = `CAFÉ DELTA Q QHARACTER 80CAP`)) + 
  geom_col(fill="#dedec7") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimRit2<- ifelse(DATAFUSION$`CAFÉ DELTA Q QHARACTER 80CAP` ==0, 0.1, 1)

fim6 <- aggregate(`fimRit2` ~ Loja,DATAFUSION, mean)
F6<-ggplot(data = fim6, aes(x = Loja, y = `fimRit2`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto7

perc7 <- aggregate(`CEVADA SOLÚVEL DELTA FRASCO 200G` ~ DATA,DATAFUSION, mean)
G7<-ggplot(perc7, aes(x = DATA, y = `CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_col(fill="#dedec7") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimRit5<- ifelse(DATAFUSION$`CEVADA SOLÚVEL DELTA FRASCO 200G` ==0, 0.1, 1)

fim7 <- aggregate(`fimRit5` ~ Loja,DATAFUSION, mean)
F7<-ggplot(data = fim7, aes(x = Loja, y = `fimRit5`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto8

perc8 <- aggregate(`CAFÉ BELLISSIMO INTENSO 10 CAP` ~ DATA,DATAFUSION, mean)
G8<-ggplot(perc8, aes(x = DATA, y = `CAFÉ BELLISSIMO INTENSO 10 CAP`)) + 
  geom_col(fill="#24a01a") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimBrasil<- ifelse(DATAFUSION$`CAFÉ BELLISSIMO INTENSO 10 CAP` ==0, 0.1, 1)

fim8 <- aggregate(`fimBrasil` ~ Loja,DATAFUSION, mean)
F8<-ggplot(data = fim8, aes(x = Loja, y = `fimBrasil`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto9

perc9 <- aggregate(`CAFÉ BELLISSIMO ORIGINALE 10 CAP` ~ DATA,DATAFUSION, mean)
G9<-ggplot(perc9, aes(x = DATA, y = `CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) + 
  geom_col(fill="#b26628") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimAngola<- ifelse(DATAFUSION$`CAFÉ BELLISSIMO ORIGINALE 10 CAP` ==0, 0.1, 1)

fim9 <- aggregate(`fimAngola` ~ Loja,DATAFUSION, mean)
F9<-ggplot(data = fim9, aes(x = Loja, y = `fimAngola`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto10

perc10 <- aggregate(`CAFÉ SOLÚVEL DELTA FRASCO 200G` ~ DATA,DATAFUSION, mean)
G10<-ggplot(perc10, aes(x = DATA, y = `CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_col(fill="#710302") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimFrasco<- ifelse(DATAFUSION$`CAFÉ SOLÚVEL DELTA FRASCO 200G` ==0, 0.1, 1)

fim10 <- aggregate(`fimFrasco` ~ Loja,DATAFUSION, mean)
F10<-ggplot(data = fim10, aes(x = Loja, y = `fimFrasco`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())


#Produto11

perc11 <- aggregate(`CAFFELATTE GO CHILL DELTA 230 ML` ~ DATA,DATAFUSION, mean)
G11<-ggplot(perc11, aes(x = DATA, y = `CAFFELATTE GO CHILL DELTA 230 ML`)) + 
  geom_col(fill="#564d49") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimIntenso<- ifelse(DATAFUSION$`CAFFELATTE GO CHILL DELTA 230 ML` ==0, 0.1, 1)

fim11 <- aggregate(`fimIntenso` ~ Loja,DATAFUSION, mean)
F11<-ggplot(data = fim11, aes(x = Loja, y = `fimIntenso`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto12

perc12 <- aggregate(`CAPPUCCINO GO CHILL DELTA 230 ML` ~ DATA,DATAFUSION, mean)
G12<-ggplot(perc12, aes(x = DATA, y = `CAPPUCCINO GO CHILL DELTA 230 ML`)) + 
  geom_col(fill="#98584f") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimQali80<- ifelse(DATAFUSION$`CAPPUCCINO GO CHILL DELTA 230 ML` ==0, 0.1, 1)

fim12 <- aggregate(`fimQali80` ~ Loja,DATAFUSION, mean)
F12<-ggplot(data = fim12, aes(x = Loja, y = `fimQali80`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto13

perc13 <- aggregate(`CARAMEL MACCHIATO GO CHILL DELTA 230ML` ~ DATA,DATAFUSION, mean)
G13<-ggplot(perc13, aes(x = DATA, y = `CARAMEL MACCHIATO GO CHILL DELTA 230ML`)) + 
  geom_col(fill="#6d4c76") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimMythiq80<- ifelse(DATAFUSION$`CARAMEL MACCHIATO GO CHILL DELTA 230ML` ==0, 0.1, 1)

fim13 <- aggregate(`fimMythiq80` ~ Loja,DATAFUSION, mean)
F13<-ggplot(data = fim13, aes(x = Loja, y = `fimMythiq80`)) +
  geom_bar(stat = "identity", fill = "dark orange") +
  labs(x = "Loja", y = "Presença do produto no Linear") + theme(axis.text.x = element_blank(), panel.background = element_blank())

#Produto14

perc14 <- aggregate(`LATTE MACCHIATO GO CHILL S/LAC 230ML` ~ DATA,DATAFUSION, mean)
G14<-ggplot(perc14, aes(x = DATA, y = `LATTE MACCHIATO GO CHILL S/LAC 230ML`)) + 
  geom_col(fill="#715a49") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1)) +
  labs(x = "Data - fim de semana", y = "Proporção de presenças em lojas")+theme(panel.background = element_blank())

DATAFUSION$fimEpic80<- ifelse(DATAFUSION$`LATTE MACCHIATO GO CHILL S/LAC 230ML` ==0, 0.1, 1)
fim14 <- aggregate(`fimEpic80` ~ Loja,DATAFUSION, mean)
F14<-ggplot(data = fim14, aes(x = Loja, y = `fimEpic80`)) +
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

#FreqC1 <- table(PresençaC1$`SINAL CAFÉ DELTA Q EPIQ 80CAP`)  #Também não consigo mudar aqui
#dfPercC1<-data.frame(prop.table(FreqC1))
#names(dfPercC1) <- c(Promos[1], "Freq")

#GPresença1 <- ggplot(dfPercC1, aes(x = Var1, y = Freq)) +
#  geom_bar(fill = "#ff6504", stat = "identity", width = 0.7) +
#  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

PresençaC1 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ DELTA Q EPIQ 80CAP`)
PresençaC1<-na.omit(PresençaC1)
PresençaC1

FreqC1 <- table(PresençaC1$`SINAL CAFÉ DELTA Q EPIQ 80CAP`)
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
  dplyr::select(`SINAL CAFÉ DELTA Q MYTHIQ 80CAP`)
PresençaC2<-na.omit(PresençaC2)

FreqC2 <- table(PresençaC2$`SINAL CAFÉ DELTA Q MYTHIQ 80CAP`)
dfPercC2<-data.frame(prop.table(FreqC2))

GPresença2 <- ggplot(dfPercC2, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#662867", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#Produto3

PresençaC3 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)
PresençaC3<-na.omit(PresençaC3)

FreqC3 <- table(PresençaC3$`SINAL CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)
dfPercC3<-data.frame(prop.table(FreqC3))

GPresença3<- ggplot(dfPercC3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e8e4c9", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

GPresença3
#Produto4

PresençaC4 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ GINGA 10CAP`)
PresençaC4<-na.omit(PresençaC4)

FreqC4 <- table(PresençaC4$`SINAL CAFÉ GINGA 10CAP`)
dfPercC4<-data.frame(prop.table(FreqC4))

GPresença4<- ggplot(dfPercC4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#c42d1d", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto5

PresençaC5 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ DELTA Q QALIDUS 80CAP`)
PresençaC5<-na.omit(PresençaC5)

FreqC5 <- table(PresençaC5$`SINAL CAFÉ DELTA Q QALIDUS 80CAP`)
dfPercC5<-data.frame(prop.table(FreqC5))

GPresença5<- ggplot(dfPercC5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#3a2120", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto6

PresençaC6 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ DELTA Q QHARACTER 80CAP`)
PresençaC6<-na.omit(PresençaC6)

FreqC6 <- table(PresençaC6$`SINAL CAFÉ DELTA Q QHARACTER 80CAP`)
dfPercC6<-data.frame(prop.table(FreqC6))

GPresença6<- ggplot(dfPercC6, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#dedec7", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto7

PresençaC7 <- DATAFUSION %>%
  dplyr::select(`SINAL CEVADA SOLÚVEL DELTA FRASCO 200G`)
PresençaC7<-na.omit(PresençaC7)

FreqC7 <- table(PresençaC7$`SINAL CEVADA SOLÚVEL DELTA FRASCO 200G`)
dfPercC7<-data.frame(prop.table(FreqC7))

GPresença7<- ggplot(dfPercC7, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#dedec7", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto8

PresençaC8 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ BELLISSIMO INTENSO 10 CAP`)
PresençaC8<-na.omit(PresençaC8)

FreqC8 <- table(PresençaC8$`SINAL CAFÉ BELLISSIMO INTENSO 10 CAP`)
dfPercC8<-data.frame(prop.table(FreqC8))

GPresença8<- ggplot(dfPercC8, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#24a01a", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto9

PresençaC9 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ BELLISSIMO ORIGINALE 10 CAP`)
PresençaC9<-na.omit(PresençaC9)

FreqC9 <- table(PresençaC9$`SINAL CAFÉ BELLISSIMO ORIGINALE 10 CAP`)
dfPercC9<-data.frame(prop.table(FreqC9))

GPresença9<- ggplot(dfPercC9, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#b26628", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto10

PresençaC10 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFÉ SOLÚVEL DELTA FRASCO 200G`)
PresençaC10<-na.omit(PresençaC10)

FreqC10 <- table(PresençaC10$`SINAL CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfPercC10<-data.frame(prop.table(FreqC10))

GPresença10<- ggplot(dfPercC10, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto11

PresençaC11 <- DATAFUSION %>%
  dplyr::select(`SINAL CAFFELATTE GO CHILL DELTA 230 ML`)
PresençaC11<-na.omit(PresençaC11)

FreqC11 <- table(PresençaC11$`SINAL CAFFELATTE GO CHILL DELTA 230 ML`)
dfPercC11<-data.frame(prop.table(FreqC11))

GPresença11<- ggplot(dfPercC11, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#564d49", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto12

PresençaC12 <- DATAFUSION %>%
  dplyr::select(`SINAL CAPPUCCINO GO CHILL DELTA 230 ML`)
PresençaC12<-na.omit(PresençaC12)

FreqC12 <- table(PresençaC12$`SINAL CAPPUCCINO GO CHILL DELTA 230 ML`)
dfPercC12<-data.frame(prop.table(FreqC12))

GPresença12<- ggplot(dfPercC12, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#98584f", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto13

PresençaC13 <- DATAFUSION %>%
  dplyr::select(`SINAL CARAMEL MACCHIATO GO CHILL DELTA 230ML`)
PresençaC13<-na.omit(PresençaC13)

FreqC13 <- table(PresençaC13$`SINAL CARAMEL MACCHIATO GO CHILL DELTA 230ML`)
dfPercC13<-data.frame(prop.table(FreqC13))

GPresença13<- ggplot(dfPercC13, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#6d4c76", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto14

PresençaC14 <- DATAFUSION %>%
  dplyr::select(`SINAL LATTE MACCHIATO GO CHILL S/LAC 230ML`)
PresençaC14<-na.omit(PresençaC14)

FreqC14 <- table(PresençaC14$`SINAL LATTE MACCHIATO GO CHILL S/LAC 230ML`)
dfPercC14<-data.frame(prop.table(FreqC14))

GPresença14<- ggplot(dfPercC14, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#715a49", stat = "identity", width = 0.7) +
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
#           geom_bar(fill = "#ff6504", stat = "identity", width = 0.7) +
#           scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#           labs(title = "", x = "", y = "") + 
#           theme(panel.background = element_blank()))
#}




### CICLOS ###




#Produto1 

ciclo1 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q EPIQ 80CAP`)
ciclo1<-na.omit(ciclo1)

C1 <- ggplot(ciclo1, aes(x = `CICLO CAFÉ DELTA Q EPIQ 80CAP`)) +
  geom_bar(fill = "#ff6504", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q EPIQ 80CAP`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())



#Produto2

ciclo2 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q MYTHIQ 80CAP`)
ciclo2<-na.omit(ciclo2)

C2 <- ggplot(ciclo2, aes(x = `CICLO CAFÉ DELTA Q MYTHIQ 80CAP`)) +
  geom_bar(fill = "#662867", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q MYTHIQ 80CAP`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto3

ciclo3 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)
ciclo3<-na.omit(ciclo3)

C3<- ggplot(ciclo3, aes(x = `CICLO CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)) +
  geom_bar(fill = "#4b625a", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ BELLISSIMO AROMA INTENSO 30CAP`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto4

ciclo4 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ GINGA 10CAP`)
ciclo4<-na.omit(ciclo4)

C4<- ggplot(ciclo4, aes(x = `CICLO CAFÉ GINGA 10CAP`)) +
  geom_bar(fill = "#e73b37", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ GINGA 10CAP`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto5
ciclo5 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q QALIDUS 80CAP`)
ciclo5<-na.omit(ciclo5)
C5 <- ggplot(ciclo5, aes(x = `CICLO CAFÉ DELTA Q QALIDUS 80CAP`)) +
  geom_bar(fill = "#3a2120", position = "stack") +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto6

ciclo6 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ DELTA Q QHARACTER 80CAP`)
ciclo6<-na.omit(ciclo6)

C6<- ggplot(ciclo6, aes(x = `CICLO CAFÉ DELTA Q QHARACTER 80CAP`)) +
  geom_bar(fill = "#dedec7", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA Q QHARACTER 80CAP`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto7

ciclo7 <-DATAFUSION %>%
  dplyr::select(`CICLO CEVADA SOLÚVEL DELTA FRASCO 200G`)
ciclo7<-na.omit(ciclo7)

C7<- ggplot(ciclo7, aes(x = `CICLO CEVADA SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "#dedec7", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CEVADA SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto8

ciclo8 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ BELLISSIMO INTENSO 10 CAP`)
ciclo8<-na.omit(ciclo8)

C8<- ggplot(ciclo8, aes(x = `CICLO CAFÉ BELLISSIMO INTENSO 10 CAP`)) +
  geom_bar(fill = "#24a01a", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ BELLISSIMO INTENSO 10 CAP`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto9

ciclo9 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ BELLISSIMO ORIGINALE 10 CAP`)
ciclo9<-na.omit(ciclo9)

C9<- ggplot(ciclo9, aes(x = `CICLO CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) +
  geom_bar(fill = "#b26628", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ BELLISSIMO ORIGINALE 10 CAP`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto10

ciclo10 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
ciclo10<-na.omit(ciclo10)

C10<- ggplot(ciclo10, aes(x = `CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "#710302", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequencia") + theme(panel.background = element_blank())

#Produto11

ciclo11 <-DATAFUSION %>%
  dplyr::select(`CICLO CAFFELATTE GO CHILL DELTA 230 ML`)
ciclo11<-na.omit(ciclo11)

C11<- ggplot(ciclo11, aes(x = `CICLO CAFFELATTE GO CHILL DELTA 230 ML`)) +
  geom_bar(fill = "#564d49", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFFELATTE GO CHILL DELTA 230 ML`",
       x = "Classes",
       y = "frequencia") + theme(panel.background = element_blank())

#Produto12

ciclo12 <-DATAFUSION %>%
  dplyr::select(`CICLO CAPPUCCINO GO CHILL DELTA 230 ML`)
ciclo12<-na.omit(ciclo12)

C12<- ggplot(ciclo12, aes(x = `CICLO CAPPUCCINO GO CHILL DELTA 230 ML`)) +
  geom_bar(fill = "#98584f", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAPPUCCINO GO CHILL DELTA 230 ML`",
       x = "Classes",
       y = "frequencia") + theme(panel.background = element_blank())

#Produto13

ciclo13 <-DATAFUSION %>%
  dplyr::select(`CICLO CARAMEL MACCHIATO GO CHILL DELTA 230ML`)
ciclo13<-na.omit(ciclo13)

C13<- ggplot(ciclo13, aes(x = `CICLO CARAMEL MACCHIATO GO CHILL DELTA 230ML`)) +
  geom_bar(fill = "#6d4c76", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CARAMEL MACCHIATO GO CHILL DELTA 230ML`",
       x = "Classes",
       y = "frequencia") + theme(panel.background = element_blank())

#Produto14

ciclo14 <-DATAFUSION %>%
  dplyr::select(`CICLO LATTE MACCHIATO GO CHILL S/LAC 230ML`)
ciclo14<-na.omit(ciclo14)

C14<- ggplot(ciclo14, aes(x = `CICLO LATTE MACCHIATO GO CHILL S/LAC 230ML`)) +
  geom_bar(fill = "#715a49", position = "stack") +
  labs(title = "`CICLO ASSEGURADO LATTE MACCHIATO GO CHILL S/LAC 230ML`",
       x = "Classes",
       y = "frequencia") + theme(panel.background = element_blank())


#gráficos
Cp1<-subplot(C1, C2, C3)
Cp2<-subplot(C4, C5, C6)
Cp3<-subplot(C7, C8, C9, C10)

subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)






###   Rotura  ###



#Produto1  

rotura1 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q EPIQ 80CAP`)
rotura1<-na.omit(rotura1)

rot1 <- table(rotura1$`ROTURA CAFÉ DELTA Q EPIQ 80CAP`)
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
  dplyr::select(`ROTURA CAFÉ DELTA Q MYTHIQ 80CAP`)
rotura2<-na.omit(rotura2)

rot2 <- table(rotura2$`ROTURA CAFÉ DELTA Q MYTHIQ 80CAP`)
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
  dplyr::select(`ROTURA CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)
rotura3<-na.omit(rotura3)

rot3 <- table(rotura3$`ROTURA CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)
dfRot3<-data.frame(prop.table(rot3))
dfRot3<-na.omit(dfRot3)


R31 <- ggplot(dfRot3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e8e4c9", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto4

rotura4 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ GINGA 10CAP`)
rotura4<-na.omit(rotura4)

rot4 <- table(rotura4$`ROTURA CAFÉ GINGA 10CAP`)
dfRot4<-data.frame(prop.table(rot4))
dfRot4<-na.omit(dfRot4)


R41 <- ggplot(dfRot4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#c42d1d", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())



#Produto5

rotura5 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q QALIDUS 80CAP`)
rotura5<-na.omit(rotura5)

rot5 <- table(rotura5$`ROTURA CAFÉ DELTA Q QALIDUS 80CAP`)
dfRot5<-data.frame(prop.table(rot5))
dfRot5<-na.omit(dfRot5)


R51 <- ggplot(dfRot5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#3a2120", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R51 <- ggplot(rotura5, aes(x = `ROTURA CAFÉ DELTA Q QALIDUS 80CAP`)) +
#  geom_bar(fill = "#3a2120", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto6

rotura6 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ DELTA Q QHARACTER 80CAP`)
rotura6<-na.omit(rotura6)

rot6 <- table(rotura6$`ROTURA CAFÉ DELTA Q QHARACTER 80CAP`)
dfRot6<-data.frame(prop.table(rot6))
dfRot6<-na.omit(dfRot6)


R61 <- ggplot(dfRot6, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#dedec7", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R61 <- ggplot(rotura6, aes(x = `ROTURA CAFÉ DELTA Q QHARACTER 80CAP`)) +
#  geom_bar(fill = "#dedec7", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto7

rotura7 <-DATAFUSION %>%
  dplyr::select(`ROTURA CEVADA SOLÚVEL DELTA FRASCO 200G`)
rotura7<-na.omit(rotura7)

rot7 <- table(rotura7$`ROTURA CEVADA SOLÚVEL DELTA FRASCO 200G`)
dfRot7<-data.frame(prop.table(rot7))
dfRot7<-na.omit(dfRot7)


R71 <- ggplot(dfRot7, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#dedec7", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R71 <- ggplot(rotura7, aes(x = `ROTURA CEVADA SOLÚVEL DELTA FRASCO 200G`)) +
#  geom_bar(fill = "#dedec7", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto8

rotura8 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ BELLISSIMO INTENSO 10 CAP`)
rotura8<-na.omit(rotura8)

rot8 <- table(rotura8$`ROTURA CAFÉ BELLISSIMO INTENSO 10 CAP`)
dfRot8<-data.frame(prop.table(rot8))
dfRot8<-na.omit(dfRot8)


R81 <- ggplot(dfRot8, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#24a01a", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R81 <- ggplot(rotura8, aes(x = `ROTURA CAFÉ BELLISSIMO INTENSO 10 CAP`)) +
#  geom_bar(fill = "#dedec7", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto9

rotura9 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ BELLISSIMO ORIGINALE 10 CAP`)
rotura9<-na.omit(rotura9)

rot9 <- table(rotura9$`ROTURA CAFÉ BELLISSIMO ORIGINALE 10 CAP`)
dfRot9<-data.frame(prop.table(rot9))
dfRot9<-na.omit(dfRot9)


R91 <- ggplot(dfRot9, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#b26628", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#R91 <- ggplot(rotura9, aes(x = `ROTURA CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) +
#  geom_bar(fill = "#b26628", position = "stack") +
#  labs(title = "",
#       x = "",
#       y = "") + theme(panel.background = element_blank())

#Produto10

rotura10 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)
rotura10<-na.omit(rotura10)

rot10 <- table(rotura10$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfRot10<-data.frame(prop.table(rot10))
dfRot10<-na.omit(dfRot10)


R101 <- ggplot(dfRot10, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())


#Produto11

rotura11 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAFFELATTE GO CHILL DELTA 230 ML`)
rotura11<-na.omit(rotura11)

rot11 <- table(rotura11$`ROTURA CAFFELATTE GO CHILL DELTA 230 ML`)
dfRot11<-data.frame(prop.table(rot11))
dfRot11<-na.omit(dfRot11)


R111 <- ggplot(dfRot11, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#564d49", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto12

rotura12 <-DATAFUSION %>%
  dplyr::select(`ROTURA CAPPUCCINO GO CHILL DELTA 230 ML`)
rotura12<-na.omit(rotura12)

rot12 <- table(rotura12$`ROTURA CAPPUCCINO GO CHILL DELTA 230 ML`)
dfRot12<-data.frame(prop.table(rot12))
dfRot12<-na.omit(dfRot12)


R121 <- ggplot(dfRot12, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#98584f", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto13

rotura13 <-DATAFUSION %>%
  dplyr::select(`ROTURA CARAMEL MACCHIATO GO CHILL DELTA 230ML`)
rotura13<-na.omit(rotura13)

rot13 <- table(rotura13$`ROTURA CARAMEL MACCHIATO GO CHILL DELTA 230ML`)
dfRot13<-data.frame(prop.table(rot13))
dfRot13<-na.omit(dfRot13)


R131 <- ggplot(dfRot13, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#6d4c76", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())

#Produto14

rotura14 <-DATAFUSION %>%
  dplyr::select(`ROTURA LATTE MACCHIATO GO CHILL S/LAC 230ML`)
rotura14<-na.omit(rotura14)

rot14 <- table(rotura14$`ROTURA LATTE MACCHIATO GO CHILL S/LAC 230ML`)
dfRot14<-data.frame(prop.table(rot14))
dfRot14<-na.omit(dfRot14)


R141 <- ggplot(dfRot14, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#715a49", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") + theme(panel.background = element_blank())






#gráficos
Rp1<-subplot(R11, R21, R31)
Rp2<-subplot(R41, R51, R61)
Rp3<-subplot(R71, R81, R91, R101)
subplot(Rp1, Rp2, Rp3, nrows=3, widths = 1, heights = NULL, margin = 0.02)






### ADEQUACAO DE STOCK ###




#Produto1  

adequa1 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ DELTA Q EPIQ 80CAP`)
adequa1<-na.omit(adequa1)

Freq1 <- table(adequa1$`ADEQUACAO CAFÉ DELTA Q EPIQ 80CAP`)
perce1<- Freq1/sum(Freq1)*100
dfAdPerc1<-data.frame(prop.table(Freq1))


A11 <- ggplot(dfAdPerc1, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#ff6504", stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto2

adequa2 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ DELTA Q MYTHIQ 80CAP`)
adequa2<-na.omit(adequa2)

Freq2 <- table(adequa2$`ADEQUACAO CAFÉ DELTA Q MYTHIQ 80CAP`)
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
  dplyr::select(`ADEQUACAO CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)
adequa3<-na.omit(adequa3)

Freq3 <- table(adequa3$`ADEQUACAO CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)
dfAdPerc3<-data.frame(prop.table(Freq3))

A31<- ggplot(dfAdPerc3, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#e8e4c9", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto4

adequa4 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ GINGA 10CAP`)
adequa4<-na.omit(adequa4)

Freq4 <- table(adequa4$`ADEQUACAO CAFÉ GINGA 10CAP`)
dfAdPerc4<-data.frame(prop.table(Freq4))

A41<- ggplot(dfAdPerc4, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#c42d1d", stat = "identity", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())


#Produto5

adequa5 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ DELTA Q QALIDUS 80CAP`)
adequa5<-na.omit(adequa5)

Freq5 <- table(adequa5$`ADEQUACAO CAFÉ DELTA Q QALIDUS 80CAP`)
dfAdPerc5<-data.frame(prop.table(Freq5))

A51<-ggplot(dfAdPerc5, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#3a2120", stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto6

adequa6 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ DELTA Q QHARACTER 80CAP`)
adequa6<-na.omit(adequa6)

Freq6 <- table(adequa6$`ADEQUACAO CAFÉ DELTA Q QHARACTER 80CAP`)
dfAdPerc6<-data.frame(prop.table(Freq6))

A61 <- ggplot(dfAdPerc6, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#dedec7", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto7

adequa7 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CEVADA SOLÚVEL DELTA FRASCO 200G`)
adequa7<-na.omit(adequa7)

Freq7 <- table(adequa7$`ADEQUACAO CEVADA SOLÚVEL DELTA FRASCO 200G`)
dfAdPerc7<-data.frame(prop.table(Freq7))

A71 <- ggplot(dfAdPerc7, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#dedec7", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto8

adequa8 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ BELLISSIMO INTENSO 10 CAP`)
adequa8<-na.omit(adequa8)

Freq8 <- table(adequa8$`ADEQUACAO CAFÉ BELLISSIMO INTENSO 10 CAP`)
dfAdPerc8<-data.frame(prop.table(Freq8))

A81 <- ggplot(dfAdPerc8, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#24a01a", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto9

adequa9 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ BELLISSIMO ORIGINALE 10 CAP`)
adequa9<-na.omit(adequa9)

Freq9 <- table(adequa9$`ADEQUACAO CAFÉ BELLISSIMO ORIGINALE 10 CAP`)
dfAdPerc9<-data.frame(prop.table(Freq9))

A91 <- ggplot(dfAdPerc9, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#b26628", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto10

adequa10 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
adequa10<-na.omit(adequa10)

Freq10 <- table(adequa10$`ADEQUACAO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
dfAdPerc10<-data.frame(prop.table(Freq10))


A101 <- ggplot(dfAdPerc10, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#710302", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())


#Produto11

adequa11 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAFFELATTE GO CHILL DELTA 230 ML`)
adequa11<-na.omit(adequa11)

Freq11 <- table(adequa11$`ADEQUACAO CAFFELATTE GO CHILL DELTA 230 ML`)
dfAdPerc11<-data.frame(prop.table(Freq11))


A111 <- ggplot(dfAdPerc11, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#564d49", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())


#Produto12

adequa12 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CAPPUCCINO GO CHILL DELTA 230 ML`)
adequa12<-na.omit(adequa12)

Freq12 <- table(adequa12$`ADEQUACAO CAPPUCCINO GO CHILL DELTA 230 ML`)
dfAdPerc12<-data.frame(prop.table(Freq12))


A121 <- ggplot(dfAdPerc12, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#98584f", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto13

adequa13 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO CARAMEL MACCHIATO GO CHILL DELTA 230ML`)
adequa13<-na.omit(adequa13)

Freq13 <- table(adequa13$`ADEQUACAO CARAMEL MACCHIATO GO CHILL DELTA 230ML`)
dfAdPerc13<-data.frame(prop.table(Freq13))


A131 <- ggplot(dfAdPerc13, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#6d4c76", stat = "identity", width = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       x = "",
       y = "") +
  theme(panel.background = element_blank())

#Produto14

adequa14 <-DATAFUSION %>%
  dplyr::select(`ADEQUACAO LATTE MACCHIATO GO CHILL S/LAC 230ML`)
adequa14<-na.omit(adequa14)

Freq14 <- table(adequa14$`ADEQUACAO LATTE MACCHIATO GO CHILL S/LAC 230ML`)
dfAdPerc14<-data.frame(prop.table(Freq14))


A141 <- ggplot(dfAdPerc14, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#715a49", stat = "identity", width = 0.25) +
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

#ADEQUACAO de Stock
subplot(Ap1, Ap2, Ap3, nrows=3, widths = 1, heights = NULL, margin = 0.02)

manter <- c("Ep1", "Ep2", "Cp1", "Cp2", "Rp1", "Rp2", "Ap1", "Ap2", "Ap3")



#######CICLOS DE REPOSIÇÃO


Ci1<-ggplot(ciclo1, aes(x="", y=`CICLO CAFÉ DELTA Q EPIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ff6504", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")


Ci2<-ggplot(ciclo2, aes(x="", y=`CICLO CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci3<-ggplot(ciclo3, aes(x="", y=`CICLO CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e8e4c9", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci4<-ggplot(ciclo4, aes(x="", y=`CICLO CAFÉ GINGA 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#c42d1d", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci5<-ggplot(ciclo5, aes(x="", y=`CICLO CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#3a2120", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci6<-ggplot(ciclo6, aes(x="", y=`CICLO CAFÉ DELTA Q QHARACTER 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci7<-ggplot(ciclo7, aes(x="", y=`CICLO CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci8<-ggplot(ciclo8, aes(x="", y=`CICLO CAFÉ BELLISSIMO INTENSO 10 CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#24a01a", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci9<-ggplot(ciclo9, aes(x="", y=`CICLO CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#b26628", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci10<-ggplot(ciclo10, aes(x="", y=`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")


Ci11<-ggplot(ciclo11, aes(x="", y=`CICLO CAFFELATTE GO CHILL DELTA 230 ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#564d49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci12<-ggplot(ciclo12, aes(x="", y=`CICLO CAPPUCCINO GO CHILL DELTA 230 ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#98584f", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci13<-ggplot(ciclo13, aes(x="", y=`CICLO CARAMEL MACCHIATO GO CHILL DELTA 230ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#6d4c76", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")

Ci14<-ggplot(ciclo14, aes(x="", y=`CICLO LATTE MACCHIATO GO CHILL S/LAC 230ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#715a49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Ciclos de Reposição")
######SELLOUTS 2 ANOS




SO1<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA Q EPIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ff6504", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO11<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ DELTA Q EPIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#ff6504", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO2<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO21<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO3<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e8e4c9", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO31<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#e8e4c9", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO4<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ GINGA 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#c42d1d", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO41<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ GINGA 10CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#c42d1d", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO5<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#3a2120", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")

SO51<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#3a2120", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO6<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ DELTA Q QHARACTER 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO61<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ DELTA Q QHARACTER 80CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO7<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO71<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO8<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ BELLISSIMO INTENSO 10 CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#24a01a", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO81<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ BELLISSIMO INTENSO 10 CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#24a01a", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO9<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#b26628", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO91<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#b26628", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO10<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO101<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")




SO11<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAFFELATTE GO CHILL DELTA 230 ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#564d49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO111<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAFFELATTE GO CHILL DELTA 230 ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#564d49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")

SO12<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CAPPUCCINO GO CHILL DELTA 230 ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#98584f", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO121<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CAPPUCCINO GO CHILL DELTA 230 ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#98584f", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO13<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT CARAMEL MACCHIATO GO CHILL DELTA 230ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#6d4c76", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO131<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 CARAMEL MACCHIATO GO CHILL DELTA 230ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#6d4c76", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")


SO14<-ggplot(DATAFUSION, aes(x="", y=`SELLOUT LATTE MACCHIATO GO CHILL S/LAC 230ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#715a49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2023")


SO141<-ggplot(DATAFUSION2022, aes(x="", y=`SELLOUT22 LATTE MACCHIATO GO CHILL S/LAC 230ML`)) + 
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="#715a49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="SellOut 2022")






#### DIAS PARA A ROTURA


DR1<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA Q EPIQ 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#ff6504", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

ggplotly(DR1)


DR2<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR3<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)) + 
  
  geom_boxplot(width=0.1, fill="#e8e4c9", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR4<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ GINGA 10CAP`)) + 
  geom_boxplot(width=0.1, fill="#c42d1d", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR5<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA Q QALIDUS 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#3a2120", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR6<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ DELTA Q QHARACTER 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR7<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR8<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ BELLISSIMO INTENSO 10 CAP`)) + 
  geom_boxplot(width=0.1, fill="#24a01a", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR9<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) + 
  geom_boxplot(width=0.1, fill="#b26628", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR10<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFÉ SOLÚVEL DELTA FRASCO 200G`)) + 
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DR11<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAFFELATTE GO CHILL DELTA 230 ML`)) + 
  geom_boxplot(width=0.1, fill="#564d49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR12<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CAPPUCCINO GO CHILL DELTA 230 ML`)) + 
  geom_boxplot(width=0.1, fill="#98584f", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR13<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura CARAMEL MACCHIATO GO CHILL DELTA 230ML`)) + 
  geom_boxplot(width=0.1, fill="#6d4c76", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DR14<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura LATTE MACCHIATO GO CHILL S/LAC 230ML`)) + 
  geom_boxplot(width=0.1, fill="#715a49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


#### DIAS PARA A ROTURA DE LINEAR

DRL1<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA Q EPIQ 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#ff6504", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")




DRL2<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA Q MYTHIQ 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#662867", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL3<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ BELLISSIMO AROMA INTENSO 30CAP`)) + 
  geom_boxplot(width=0.1, fill="#e8e4c9", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL4<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ GINGA 10CAP`)) + 
  geom_boxplot(width=0.1, fill="#c42d1d", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL5<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA Q QALIDUS 80CAP`)) +
  geom_boxplot(width=0.1, fill="#3a2120", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL6<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ DELTA Q QHARACTER 80CAP`)) + 
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL7<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CEVADA SOLÚVEL DELTA FRASCO 200G`)) + 
  
  geom_boxplot(width=0.1, fill="#dedec7", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL8<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ BELLISSIMO INTENSO 10 CAP`)) + 
  geom_boxplot(width=0.1, fill="#24a01a", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL9<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ BELLISSIMO ORIGINALE 10 CAP`)) +
  geom_boxplot(width=0.1, fill="#b26628", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL10<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_boxplot(width=0.1, fill="#710302", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")


DRL11<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAFFELATTE GO CHILL DELTA 230 ML`)) +
  geom_boxplot(width=0.1, fill="#564d49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL12<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CAPPUCCINO GO CHILL DELTA 230 ML`)) +
  geom_boxplot(width=0.1, fill="#98584f", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL13<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear CARAMEL MACCHIATO GO CHILL DELTA 230ML`)) +
  geom_boxplot(width=0.1, fill="#6d4c76", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

DRL14<-ggplot(DATAFUSION, aes(x="", y=`Dias_para_Rotura_Linear LATTE MACCHIATO GO CHILL S/LAC 230ML`)) +
  geom_boxplot(width=0.1, fill="#715a49", outlier.size=0.5) +
  coord_flip() +
  theme_classic() +
  labs(x="", y="Dias para a rotura")

##### Balanço



sessionInfo()





#rm(list = setdiff(ls(), manter))

### Exportar ficheiro CSV###
#write.csv(DATAFUSION, "DATAFUSIONFinal.csv",row.names = FALSE, sep=";")