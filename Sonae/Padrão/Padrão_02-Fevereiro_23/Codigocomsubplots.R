library(readxl)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(plotly)
fileWide <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brand and Ninjas/Fevereiro/Resultado Sonae_fevereiro 23.xlsx")

fileLong <- gather(fileWide, Produto, Presença, "PACK CAFÉ DELTA Q GRAND MYTHIQ 72+8 CAP":"CAFÉ SOLÚVEL DELTA FRASCO 200G", factor_key=TRUE)

PresençaPerc <- fileLong %>%
  group_by(Produto) %>%
  summarise(PresençaPerc = sum(Presença) / nrow(fileWide)) %>%
  arrange(desc(PresençaPerc))


a <- ggplot(PresençaPerc, aes(x=PresençaPerc*100.0, y=Produto, color=Produto))+
  geom_bar(stat = "identity", fill="red")+
  geom_text(aes(label = paste0(round(PresençaPerc * 100), "%")), hjust = -0.2)+
  labs(title= "Presença de Produto em Linear")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
  #scale_x_continuous(limits = c(80, 100))

b <- ggplot(PresençaPerc, aes(x=PresençaPerc*100.0, y=Produto))+
  geom_bar(stat = "identity", fill="red")+
  geom_text(aes(label = paste0(round(PresençaPerc * 100), "%")), hjust = -0.2)+
  labs(title= "Presença de Produto em Linear 2")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

subplot(a, b, nrows=2)
a


