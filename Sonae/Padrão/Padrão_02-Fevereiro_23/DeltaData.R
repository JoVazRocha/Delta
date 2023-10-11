library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)

signalDetection <- read_excel("3ficheiros.xlsx")


###    CICLOS ASSEGURADOS?   ###
#cafe1
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<-NA
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` >1.1] <- "SIM"
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` <=1.1] <- "NÃO"

ciclo1 <-signalDetection %>%
  dplyr::select(`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
ciclo1<-na.omit(ciclo1)
C1 <- ggplot(ciclo1, aes(x = ` PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = " ASSEGURADO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())
C1
#cafe2  
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<-NA
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`/signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` >1.1] <- "SIM"
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`/signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP` <=1.1] <- "NÃO"

ciclo2 <-signalDetection %>%
  dplyr::select(`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
ciclo2<-na.omit(ciclo2)

C2 <- ggplot(ciclo2, aes(x = `CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`CICLO ASSEGURADO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe3
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-NA
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` >1.1] <- "SIM"
signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`/signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP` <=1.1] <- "NÃO"

ciclo3 <-signalDetection %>%
  dplyr::select(`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
ciclo3<-na.omit(ciclo3)

C3 <- ggplot(ciclo3, aes(x = `CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`CICLO ASSEGURADO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
signalDetection$`CICLO CAFÉ DELTA PORTUGAL MU 220G`<-NA
signalDetection$`CICLO CAFÉ DELTA PORTUGAL MU 220G`[signalDetection$`STOCK CAFÉ DELTA PORTUGAL MU 220G`/signalDetection$`Preslinear CAFÉ DELTA PORTUGAL MU 220G` >1.1] <- "SIM"
signalDetection$`CICLO CAFÉ DELTA PORTUGAL MU 220G`[signalDetection$`STOCK CAFÉ DELTA PORTUGAL MU 220G`/signalDetection$`Preslinear CAFÉ DELTA PORTUGAL MU 220G` <=1.1] <- "NÃO"

ciclo4 <-signalDetection %>%
  dplyr::select(`CICLO CAFÉ DELTA PORTUGAL MU 220G`)
ciclo4<-na.omit(ciclo4)

C4<- ggplot(ciclo4, aes(x = `CICLO CAFÉ DELTA PORTUGAL MU 220G`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ DELTA PORTUGAL MU 220G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
signalDetection$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`<-NA
signalDetection$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`[signalDetection$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`/signalDetection$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G` >1.1] <- "SIM"
signalDetection$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`[signalDetection$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`/signalDetection$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G` <=1.1] <- "NÃO"

ciclo5 <-signalDetection %>%
  dplyr::select(`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
ciclo5<-na.omit(ciclo5)

C5<- ggplot(ciclo5, aes(x = `CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`CICLO ASSEGURADO CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#gráficos
Cp1<-subplot(C1, C2, C3)
Cp2<-subplot(C4, C5)

subplot(Cp1, Cp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)







###   HÁ STOCK DISPONÍVEL?  ###

#cafe1
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<-NA
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<=0] <- "ROTURA"
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`>0] <- "EM STOCK"

rotura1 <-signalDetection %>%
  dplyr::select(`ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
rotura1<-na.omit(rotura1)
R1 <- ggplot(rotura1, aes(x = `ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "ROTURA PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe2  
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<-NA
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<=0] <- "ROTURA"
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`>0] <- "EM STOCK"

rotura2 <-signalDetection %>%
  dplyr::select(`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
rotura2<-na.omit(rotura2)

R2 <- ggplot(rotura2, aes(x = `ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ROTURA PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe3
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-NA
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<=0] <- "ROTURA"
signalDetection$`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`>0] <- "EM STOCK"

rotura3 <-signalDetection %>%
  dplyr::select(`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
rotura3<-na.omit(rotura3)

R3 <- ggplot(rotura3, aes(x = `ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ROTURA PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
signalDetection$`ROTURA CAFÉ DELTA PORTUGAL MU 220G`<-NA
signalDetection$`ROTURA CAFÉ DELTA PORTUGAL MU 220G`[signalDetection$`STOCK CAFÉ DELTA PORTUGAL MU 220G`<=0] <- "ROTURA"
signalDetection$`ROTURA CAFÉ DELTA PORTUGAL MU 220G`[signalDetection$`STOCK CAFÉ DELTA PORTUGAL MU 220G`>0] <- "EM STOCK"

rotura4 <-signalDetection %>%
  dplyr::select(`ROTURA CAFÉ DELTA PORTUGAL MU 220G`)
rotura4<-na.omit(rotura4)

R4<- ggplot(rotura4, aes(x = `ROTURA CAFÉ DELTA PORTUGAL MU 220G`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ROTURA CAFÉ DELTA PORTUGAL MU 220G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
signalDetection$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`<-NA
signalDetection$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`[signalDetection$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`<=0] <- "ROTURA"
signalDetection$`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`[signalDetection$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`>0] <- "EM STOCK"

rotura5 <-signalDetection %>%
  dplyr::select(`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)
rotura5<-na.omit(rotura5)

R5<- ggplot(rotura5, aes(x = `ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ROTURA CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#gráficos
Rp1<-subplot(R1, R2, R3)
Rp2<-subplot(R4, R5)

subplot(Rp1, Rp2, nrows=2,widths = 1, heights = NULL, margin = 0.02)






###   ADEQUAÇÃO DE STOCK  ###

#cafe1
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`<-NA
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`=="SIM"] <- "STOCK SUFICIENTE"
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`=="NÃO" & signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+signalDetection$`INTRANSIT PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+signalDetection$`EXPECTED PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`>= signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`] <- "STOCK INSUFICIENTE COM FORNECIMENTO ADEQUADO"
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`=="NÃO" & signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+signalDetection$`INTRANSIT PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`+signalDetection$`EXPECTED PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`< signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`] <- "STOCK INSUFICIENTE COM FORNECIMENTO DESADEQUADO"

adequa1 <-signalDetection %>%
  dplyr::select(`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)
adequa1<-na.omit(adequa1)

A1 <- ggplot(adequa1, aes(x = `ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe2  
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`<-NA
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`=="SIM"] <- "STOCK SUFICIENTE"
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`=="NÃO" & signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+signalDetection$`INTRANSIT PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+signalDetection$`EXPECTED PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`>= signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`] <- "STOCK INSUFICIENTE COM FORNECIMENTO ADEQUADO"
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`=="NÃO" & signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+signalDetection$`INTRANSIT PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`+signalDetection$`EXPECTED PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`< signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`] <- "STOCK INSUFICIENTE COM FORNECIMENTO DESADEQUADO"

adequa2 <-signalDetection %>%
  dplyr::select(`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)
adequa2<-na.omit(adequa2)

A2 <- ggplot(adequa2, aes(x = `ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND EPIQ 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe3
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`<-NA
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`=="SIM"] <- "STOCK SUFICIENTE"
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`=="NÃO" & signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+signalDetection$`INTRANSIT PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+signalDetection$`EXPECTED PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`>= signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`] <- "STOCK INSUFICIENTE COM FORNECIMENTO ADEQUADO"
signalDetection$`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`[signalDetection$`CICLO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`=="NÃO" & signalDetection$`STOCK PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+signalDetection$`INTRANSIT PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`+signalDetection$`EXPECTED PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`< signalDetection$`Preslinear PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`] <- "STOCK INSUFICIENTE COM FORNECIMENTO DESADEQUADO"

adequa3 <-signalDetection %>%
  dplyr::select(`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)
adequa3<-na.omit(adequa3)

A3 <- ggplot(adequa3, aes(x = `ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ADEQUAÇÃO PACK CAFÉ DELTA Q GRAND QALIDUS 72+8 CAP`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe4
signalDetection$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`<-NA
signalDetection$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`[signalDetection$`CICLO CAFÉ DELTA PORTUGAL MU 220G`=="SIM"] <- "STOCK SUFICIENTE"
signalDetection$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`[signalDetection$`CICLO CAFÉ DELTA PORTUGAL MU 220G`=="NÃO" & signalDetection$`STOCK CAFÉ DELTA PORTUGAL MU 220G`+signalDetection$`INTRANSIT CAFÉ DELTA PORTUGAL MU 220G`+signalDetection$`EXPECTED CAFÉ DELTA PORTUGAL MU 220G`>= signalDetection$`Preslinear CAFÉ DELTA PORTUGAL MU 220G`] <- "STOCK INSUFICIENTE COM FORNECIMENTO ADEQUADO"
signalDetection$`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`[signalDetection$`CICLO CAFÉ DELTA PORTUGAL MU 220G`=="NÃO" & signalDetection$`STOCK CAFÉ DELTA PORTUGAL MU 220G`+signalDetection$`INTRANSIT CAFÉ DELTA PORTUGAL MU 220G`+signalDetection$`EXPECTED CAFÉ DELTA PORTUGAL MU 220G`< signalDetection$`Preslinear CAFÉ DELTA PORTUGAL MU 220G`] <- "STOCK INSUFICIENTE COM FORNECIMENTO DESADEQUADO"

adequa4 <-signalDetection %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`)
adequa4<-na.omit(adequa4)

A4<- ggplot(adequa4, aes(x = `ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ADEQUAÇÃO CAFÉ DELTA PORTUGAL MU 220G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#cafe5
signalDetection$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`<-NA
signalDetection$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`[signalDetection$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`=="SIM"] <- "STOCK SUFICIENTE"
signalDetection$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`[signalDetection$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`=="NÃO" & signalDetection$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`+signalDetection$`INTRANSIT CAFÉ SOLÚVEL DELTA FRASCO 200G`+signalDetection$`EXPECTED CAFÉ SOLÚVEL DELTA FRASCO 200G`>= signalDetection$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`] <- "STOCK INSUFICIENTE COM FORNECIMENTO ADEQUADO"
signalDetection$`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`[signalDetection$`CICLO CAFÉ SOLÚVEL DELTA FRASCO 200G`=="NÃO" & signalDetection$`STOCK CAFÉ SOLÚVEL DELTA FRASCO 200G`+signalDetection$`INTRANSIT CAFÉ SOLÚVEL DELTA FRASCO 200G`+signalDetection$`EXPECTED CAFÉ SOLÚVEL DELTA FRASCO 200G`< signalDetection$`Preslinear CAFÉ SOLÚVEL DELTA FRASCO 200G`] <- "STOCK INSUFICIENTE COM FORNECIMENTO DESADEQUADO"

adequa5 <-signalDetection %>%
  dplyr::select(`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`)
adequa5<-na.omit(adequa5)

A5<- ggplot(adequa5, aes(x = `ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`)) +
  geom_bar(fill = "blue", position = "stack") +
  labs(title = "`ADEQUAÇÃO CAFÉ SOLÚVEL DELTA FRASCO 200G`",
       x = "Classes",
       y = "Frequência") + theme(panel.background = element_blank())

#gráficos
Ap1<-subplot(A1, A2, A3)
Ap2<-subplot(A4, A5)

subplot(Ap1, Ap2, nrows=2,widths = 1, heights = NULL, margin = 0.02)


