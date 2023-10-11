####Stocks SONAE fim de semana março 2022 e 1º de Abril###

library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)

# Com sexta feira
file1 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds1.xlsx")
file2 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds2.xlsx")
file3 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds3.xlsx")
file4 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds4.xlsx")
file5 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds5.xlsx")

#Com segunda feira
file12 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds1s.xlsx")
file22 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds2s.xlsx")
file32 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds3s.xlsx")
file42 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds4s.xlsx")
file52 <- read_excel("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds5s.xlsx")
#fileTodos2<- read.csv("C:/Users/joao_/OneDrive/Ambiente de Trabalho/Brands and Ninjas/Delta/Produtos_todos_2022/fds5s.xlsx")

# Stocks: Média e Desvio padrão para cada FDS#

file1MedSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = mean)
file1StdSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = sd)
stocks1 <- cbind(file1MedSto, sdSto = file1StdSto$SOH)
stocks1$CVS <-stocks1$sdSto/stocks1$SOH*100

file2MedSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = mean)
file2StdSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = sd)
stocks2 <- cbind(file2MedSto, sdSto = file2StdSto$SOH)
stocks2$CVS <-stocks2$sdSto/stocks2$SOH*100

file3MedSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = mean)
file3StdSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = sd)
stocks3 <- cbind(file3MedSto, sdSto = file3StdSto$SOH)
stocks3$CVS <-stocks3$sdSto/stocks3$SOH*100

file4MedSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = mean)
file4StdSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = sd)
stocks4 <- cbind(file4MedSto, sdSto = file4StdSto$SOH)
stocks4$CVS <-stocks4$sdSto/stocks4$SOH*100

file5MedSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = mean)
file5StdSto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = sd)
stocks5 <- cbind(file5MedSto, sdSto = file5StdSto$SOH)
stocks5$CVS <-stocks5$sdSto/stocks5$SOH*100

# Preslinear: Média e Desvio padrão #

file1MedPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = mean)
file1StdPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = sd)
pres1 <- cbind(file1MedPres, sdPres = file1StdPres$PRES_STOCK)


file2MedPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = mean)
file2StdPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = sd)
pres2 <- cbind(file2MedPres, sdPres = file2StdPres$PRES_STOCK)

file3MedPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = mean)
file3StdPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = sd)
pres3 <- cbind(file3MedPres, sdPres = file3StdPres$PRES_STOCK)

file4MedPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = mean)
file4StdPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = sd)
pres4 <- cbind(file4MedPres, sdPres = file4StdPres$PRES_STOCK)

file5MedPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = mean)
file5StdPres <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = sd)
pres5 <- cbind(file5MedPres, sdPres = file5StdPres$PRES_STOCK)

# Expected: Média e Desvio padrão #


file1Exp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = mean)
file1StdExp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = sd)
Exp1 <- cbind(file1Exp, sdExp = file1StdExp$EXPECTED)


file2Exp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = mean)
file2StdExp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = sd)
Exp2 <- cbind(file2Exp, sdExp = file2StdExp$EXPECTED)

file3Exp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = mean)
file3StdExp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = sd)
Exp3 <- cbind(file3Exp, sdExp = file3StdExp$EXPECTED)

file4Exp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = mean)
file4StdExp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = sd)
Exp4 <- cbind(file4Exp, sdExp = file4StdExp$EXPECTED)

file5Exp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = mean)
file5StdExp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = sd)
Exp5 <- cbind(file5Exp, sdExp = file5StdExp$EXPECTED)

# Instransit: Média e Desvio padrão #

file1Int <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = mean)
file1StdInt <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file1, FUN = sd)
Int1 <- cbind(file1Int, sdInt = file1StdInt$INTRANSIT)


file2Int <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = mean)
file2StdInt <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file2, FUN = sd)
Int2 <- cbind(file2Int, sdInt = file2StdInt$INTRANSIT)

file3Int <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = mean)
file3StdInt <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file3, FUN = sd)
Int3 <- cbind(file3Int, sdInt = file3StdInt$INTRANSIT)

file4Int <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = mean)
file4StdInt <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file4, FUN = sd)
Int4 <- cbind(file4Int, sdInt = file4StdInt$INTRANSIT)

file5Int <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = mean)
file5StdInt <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = file5, FUN = sd)
Int5 <- cbind(file5Int, sdInt = file5StdInt$INTRANSIT)

# Sell out: Média e Desvio padrão #

file1Sell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file12, FUN = mean)
file1StdSell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file12, FUN = sd)
Sell1 <- cbind(file1Sell, sdSell = file1StdSell$Sellout)
Sell1$CVV <- Sell1$sdSell/Sell1$Sellout*100

file2Sell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file22, FUN = mean)
file2StdSell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file22, FUN = sd)
Sell2 <- cbind(file2Sell, sdSell = file2StdSell$Sellout)
Sell2$CVV <-Sell2$sdSell/Sell2$Sellout*100

file3Sell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file32, FUN = mean)
file3StdSell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file32, FUN = sd)
Sell3 <- cbind(file3Sell, sdSell = file3StdSell$Sellout)
Sell3$CVV <-Sell3$sdSell/Sell3$Sellout*100

file4Sell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file42, FUN = mean)
file4StdSell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file42, FUN = sd)
Sell4 <- cbind(file4Sell, sdSell = file4StdSell$Sellout)
Sell4$CVV <-Sell4$sdSell/Sell4$Sellout*100

file5Sell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file52, FUN = mean)
file5StdSell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = file52, FUN = sd)
Sell5 <- cbind(file5Sell, sdSell = file5StdSell$Sellout)
Sell5$CVV <-Sell5$sdSell/Sell5$Sellout*100


#MERGE por fim de semana


#1º FDS

df1 <- merge(stocks1, pres1, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df1_2 <- merge(df1, Exp1, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df1_3 <- merge(df1_2, Int1, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df1_4 <- merge(df1_3, Sell1, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)

df1_4$"Rotura"<-NA
df1_4$"Rotura"[df1_4$SOH==0] <-1
df1_4$"Rotura"[df1_4$SOH!=0] <-0
df1_4$"Proporcao_Rot" <- df1_4$Rotura/15 #dias em causa
df1_4$"Dias_Rot" <-df1_4$PRES_STOCK/df1_4$Sellout


#2º FDS

df2 <- merge(stocks2, pres2, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df2_2 <- merge(df2, Exp2, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df2_3 <- merge(df2_2, Int2, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df2_4 <- merge(df2_3, Sell2, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)

df2_4$"Rotura"<-NA
df2_4$"Rotura"[df2_4$SOH==0] <-1
df2_4$"Rotura"[df2_4$SOH!=0] <-0
df2_4$"Proporcao_Rot" <- df2_4$Rotura/15 #dias em causa
df2_4$"Dias_Rot" <-df2_4$PRES_STOCK/df2_4$Sellout

#3º FDS

df3 <- merge(stocks3, pres3, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df3_2 <- merge(df3, Exp3, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df3_3 <- merge(df3_2, Int3, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df3_4 <- merge(df3_3, Sell3, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)

df3_4$"Rotura"<-NA
df3_4$"Rotura"[df3_4$SOH==0] <-1
df3_4$"Rotura"[df3_4$SOH!=0] <-0
df3_4$"Proporcao_Rot" <- df3_4$Rotura/15 #dias em causa
df3_4$"Dias_Rot" <-df3_4$PRES_STOCK/df3_4$Sellout

#4º FDS

df4 <- merge(stocks4, pres4, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df4_2 <- merge(df4, Exp4, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df4_3 <- merge(df4_2, Int4, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df4_4 <- merge(df4_3, Sell4, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)

df4_4$"Rotura"<-NA
df4_4$"Rotura"[df4_4$SOH==0] <-1
df4_4$"Rotura"[df4_4$SOH!=0] <-0
df4_4$"Proporcao_Rot" <- df4_4$Rotura/15 #dias em causa
df4_4$"Dias_Rot" <-df4_4$PRES_STOCK/df4_4$Sellout

#5º FDS

df5 <- merge(stocks5, pres5, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df5_2 <- merge(df5, Exp5, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df5_3 <- merge(df5_2, Int5, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
df5_4 <- merge(df5_3, Sell5, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)

df5_4$"Rotura"<-NA
df5_4$"Rotura"[df5_4$SOH==0] <-1
df5_4$"Rotura"[df5_4$SOH!=0] <-0
df5_4$"Proporcao_Rot" <- df5_4$Rotura/15 #dias em causa
df5_4$"Dias_Rot" <-df5_4$PRES_STOCK/df5_4$Sellout

#FDS todos

dfTodos <- rbind(df1_4, df2_4, df3_4, df4_4, df5_4)

dfTodos1 <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos1sto <- aggregate(SOH ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
sto <- cbind(dfTodos1, sdSto = dfTodos1sto$SOH)

dfTodos2 <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos2li <- aggregate(PRES_STOCK ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
lin <- cbind(dfTodos2, sdLin = dfTodos2li$PRES_STOCK)

dfTodos3 <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos3int <- aggregate(INTRANSIT ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
int <- cbind(dfTodos3, sdInt = dfTodos3int$INTRANSIT)

dfTodos4 <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos4exp <- aggregate(EXPECTED ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
exp <- cbind(dfTodos4, sdExp = dfTodos4exp$EXPECTED)

dfTodos5 <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos5sell <- aggregate(Sellout ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
sell <- cbind(dfTodos5, sdSell = dfTodos5sell$Sellout)

dfTodos6 <- aggregate(Proporcao_Rot ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos6prop <- aggregate(Proporcao_Rot ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
prop <- cbind(dfTodos6, sdProp = dfTodos6prop$Proporcao_Rot)

dfTodos7 <- aggregate(Dias_Rot ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos7dias <- aggregate(Dias_Rot ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
dias <- cbind(dfTodos7, sdDias = dfTodos7dias$Dias_Rot)

dfTodos8 <- aggregate(Rotura ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = mean)
dfTodos8rot <- aggregate(Rotura ~ STORE_NAME + DESC_ARTIGO, data = dfTodos, FUN = sd)
rot <- cbind(dfTodos8, sdRot = dfTodos8rot$Rotura)

dfTodos_0 <- merge(sto, lin, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
dfTodos_1 <- merge(dfTodos_0, exp, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
dfTodos_2 <- merge(dfTodos_1, int, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
dfTodos_3 <- merge(dfTodos_2, sell, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
dfTodos_4 <- merge(dfTodos_3, prop, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
dfTodos_5 <- merge(dfTodos_4, dias, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)
dfTodos_6 <- merge(dfTodos_5, rot, by = c("STORE_NAME", "DESC_ARTIGO"), all.x = TRUE)



# Ficheiros relevantes #




View(df1_4)        #1º Fim de semana
#View(df2_4)        #2º Fim de semana
#View(df3_4)        #3º Fim de semana
#View(df4_4)        #4º Fim de semana
#View(dfTodos_6)    #Proporção de linear total vendido por dia na média dos 4 fds (média por fim de semana e depois média para os 4)

#write.csv(df1_4, "1ºFdsMar2020.csv",row.names = FALSE, sep=";")
#write.csv(df2_4, "2ºFdsMar2020.csv",row.names = FALSE, sep=";")
#write.csv(df3_4, "3ºFdsMar2020.csv",row.names = FALSE, sep=";")
#write.csv(df4_4, "4ºFdsMar2020.csv",row.names = FALSE, sep=";")
#write.csv(dfTodos_6, "FdsMar2020.csv",row.names = FALSE, sep=";")




#dfCafSol <- df1_4 %>%
#  filter(DESC_ARTIGO == "CAFÉ DELTA Q QALIDUS 10CAP" | "BEBIDA CEREAIS DELTA C/20%CAFE FR 200G")
#summary(dfCafSol$Rotura)

#teste <-df2_4 [df2_4$DESC_ARTIGO=="CAFÉ DELTA Q QALIDUS 10CAP" | df2_4$DESC_ARTIGO=="BEBIDA CEREAIS DELTA C/20%CAFE FR 200G", ]
