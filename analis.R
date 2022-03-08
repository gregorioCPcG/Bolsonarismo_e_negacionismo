# as variáveis ####

library(haven)
base <- read_sav("04745.SAV")
library(tidyverse)


table(base$P9C01) # transformar 4 e 5 em 1 e o resto em 0 - confiança (dep)
table(base$REND1) # faixa de renda, excluir 98 e 99
summary(base$IDADE)#OK
table(base$SEXO) # as levels
table(base$P3A) # 1 aporva, o resto 0 (aprovação)
summary(base$ESCOLARIDADE) # ok
summary(base$RELIGIAO)
summary(base$REG)
table(base$P1A05)# pra depóis outra INDEP
print(base$P10C04) # microchip pra depois


# recodificações ####

dados<- subset(base, select = c(P9C01,REND1,IDADE,SEXO,P3A,ESCOLARIDADE,REG,RELIGIAO,P1A05,P10C04))#aqui
# dep
summary(dados$P9C01)
table(dados$P9C01)
dados$P9C01[dados$P9C01 == 1] <- 0
dados$P9C01[dados$P9C01 == 4] <- 1 
dados$P9C01[dados$P9C01 == 5] <- 1
dados$P9C01[dados$P9C01 == 2] <- 0
dados$P9C01[dados$P9C01 == 3] <- 0
dados$P9C01[dados$P9C01 == 99] <- 0
summary(dados$P9C01)
table(dados$P9C01)#testar
dados$dep <- dados$P9C01
dados$dep <- as.numeric(dados$dep) # usar dep e excluir P9C01 
table(dados$dep) # um desconfia, zero confia

#renda
table(dados$REND1)
dados$REND1[dados$REND1 == 98] <- 0 #não tem rendimento
dados$REND1[dados$REND1 == 99] <- NA
table(dados$REND1)
library(memisc)
dados$REND1 <- memisc::recode(as.factor(dados$REND1), 0 <- c(0), 1 <- c(6),
                                           2 <- c(5), 3<- c(4), 4<- c(1,2,3))
table(dados$REND1)
detach("package:memisc", unload = TRUE)


#Idade e ESCOLARIDADE ok

#SEXO 
dados$SEXO <- as.factor(dados$SEXO)
levels(dados$SEXO)
levels(dados$SEXO) <- c('MASC','FEM')
levels(dados$SEXO)


# Aprovacao Bolsonaro
table(dados$P3A)
library(memisc)
dados$P3A <- memisc::recode(as.numeric(dados$P3A), 0 <- c(2,99), 1 <- c(1))
table(dados$P3A)
detach("package:memisc", unload = TRUE)
table(dados$P3A) # um aprova, zero não
dados$aprova <- dados$P3A


#religião
library(memisc)
table(dados$RELIGIAO)
dados$RELIGIAO <- memisc::recode(as.numeric(dados$RELIGIAO), 1 <- c(1), 2 <- c(2,3,4,5,6,7,8,9,10,11),
                                 NA <-c(99), 4 <- c(19,20), 3 <- c(12,13,15,16,17,18))
table(dados$RELIGIAO)
detach("package:memisc", unload = TRUE)

library(car)
dados <- dados %>%
  mutate(Evangelico = case_when(RELIGIAO == "2" ~ 1,
                                TRUE ~ 0))%>%
  mutate(Catolico = case_when(RELIGIAO == "1" ~ 1,
                              TRUE ~ 0))%>%
  mutate(Ateu_Agnostico = case_when(RELIGIAO == "4" ~ 1,
                                    TRUE ~ 0))%>%
  mutate(Outras = case_when(RELIGIAO == "3" ~ 1, TRUE ~0))
dados$REG <- as.numeric(dados$REG)
dados <- dados %>%
  mutate(NORTE_CENTROOESTE = case_when(REG == "1" ~ 1,
                                TRUE ~ 0))%>%
  mutate(NORDESTE = case_when(REG == "2" ~ 1,
                              TRUE ~ 0))%>%
  mutate(SUDESTE = case_when(REG == "3" ~ 1,
                                    TRUE ~ 0))%>%
  mutate(SUL = case_when(REG == "4" ~ 1, TRUE ~0))




dados<- subset(dados, select = c(dep,REND1,IDADE,SEXO,aprova,ESCOLARIDADE,
                                 Evangelico, Catolico, Ateu_Agnostico,Outras,
                                 NORTE_CENTROOESTE,NORDESTE,SUDESTE, SUL, P1A05, P10C04)) %>% na.omit()
dados$REND1 <- as.numeric(dados$REND1)
# analises ########### #dep 1

summary(dados)
table(dados$dep)
table(dados$aprova)
cor.test(dados$aprova, dados$dep)
library(polycor) #pacote exigido
polyserial(dados$aprova, dados$dep)
model <- glm(dep ~ aprova, data = dados, family=binomial(link=logit))
library(sjPlot)
tab_model(model, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
(1.32-1)*100
full1 <- glm(dep ~ aprova + REND1 + IDADE + SEXO + ESCOLARIDADE, data = dados, family=binomial(link=logit))
tab_model(model, full1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
full2 <- glm(dep ~ aprova + REND1 + IDADE + SEXO + ESCOLARIDADE + Catolico +
               Evangelico + Ateu_Agnostico + NORDESTE + SUDESTE + SUL, data = dados, family=binomial(link=logit))
tab_model(model, full1, full2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

# OUTRA indDEP - COM CERTEZA VOTARIA EM bOLSONARO ###### OUTRA INDEP 2

table(dados$P1A05)
library(memisc)
dados$bolsonaro <- memisc::recode(as.numeric(dados$P1A05), 1 <- c(1), 0 <- c(2,3,4,99))
table(dados$bolsonaro)
detach("package:memisc", unload = TRUE)

# anbalise
cor.test(dados$bolsonaro, dados$dep)
polyserial(dados$bolsonaro, dados$dep)
model <- glm(dep ~ bolsonaro, data = dados, family=binomial(link=logit))
tab_model(model, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
full1 <- glm(dep ~ bolsonaro + REND1 + IDADE + SEXO + ESCOLARIDADE, data = dados, family=binomial(link=logit))
tab_model(model, full1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
full2 <- glm(dep ~ bolsonaro + REND1 + IDADE + SEXO + ESCOLARIDADE + Catolico +
               Evangelico + Ateu_Agnostico + NORDESTE + SUDESTE + SUL, data = dados, family=binomial(link=logit))
tab_model(model, full1, full2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


polyserial(dados$aprova, dados$bolsonaro)



# #### Microchip 

# testada com aprova e votaria de certeza

# primeiro recod

print(base$P10C04)
library(memisc)
dados$microchip <- memisc::recode(as.numeric(dados$P10C04), 1 <- c(1,2), 0 <- c(3,4,5,99))
table(dados$microchip)
detach("package:memisc", unload = TRUE)

#aprova
cor.test(dados$aprova, dados$microchip)
polyserial(dados$aprova, dados$microchip)
model <- glm(microchip ~ aprova, data = dados, family=binomial(link=logit))
tab_model(model, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
full1 <- glm(microchip ~ aprova + REND1 + IDADE + SEXO + ESCOLARIDADE, data = dados, family=binomial(link=logit))
tab_model(model, full1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
full2 <- glm(microchip ~ aprova + REND1 + IDADE + SEXO + ESCOLARIDADE + Catolico +
               Evangelico + Ateu_Agnostico + NORDESTE + SUDESTE + SUL, data = dados, family=binomial(link=logit))
tab_model(model, full1, full2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

prop.table(table(dados$aprova, dados$microchip),2)
table(dados$aprova, dados$microchip)

# votaria com certeza
cor.test(dados$bolsonaro, dados$microchip)
polyserial(dados$bolsonaro, dados$microchip)
model <- glm(microchip ~ bolsonaro, data = dados, family=binomial(link=logit))
tab_model(model, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
full1 <- glm(microchip ~ bolsonaro + REND1 + IDADE + SEXO + ESCOLARIDADE, data = dados, family=binomial(link=logit))
tab_model(model, full1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
full2 <- glm(microchip ~ bolsonaro + REND1 + IDADE + SEXO + ESCOLARIDADE + Catolico +
               Evangelico + Ateu_Agnostico + NORDESTE + SUDESTE + SUL, data = dados, family=binomial(link=logit))
tab_model(model, full1, full2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
prop.table(table(dados$bolsonaro, dados$microchip),2)

library(coefplot)
coefplot(full2, intercept=FALSE)
# tabelinhas

aprova <- dados$aprova
microchip <- dados$microchip
bolsonaro <- dados$bolsonaro
aprova <- as.factor(aprova)
levels(aprova)
levels(aprova) <- c('Desaprova','Aprova')
levels(aprova)
microchip <- as.factor(microchip)
levels(microchip)
levels(microchip) <- c('Discorda','Concorda')
levels(microchip)
bolsonaro <- as.factor(bolsonaro)
levels(bolsonaro)
levels(bolsonaro) <- c('','Votaria com certeza')
levels(bolsonaro)
print(base$P9C01)
inseguro <- as.factor(dados$dep)
levels(inseguro)
levels(inseguro) <- c('seguro','inseguro')
levels(inseguro)

b <- data.frame(bolsonaro, microchip, inseguro, aprova)
library(RColorBrewer)
options(scipen = 999)
g <- ggplot (b, aes(microchip, aprova))
g + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Aprovação à Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                        x = "Há Microchip na Vacina contra a Covid-19?",
                        y= "Aprova o Governo de Jair Bolsonaro?",caption = "Fonte:IPEC 2021")
table(b$aprova, b$microchip)
prop.table(table(b$aprova, b$microchip),1)*100


g <- ggplot (b, aes(microchip, aprova, color=bolsonaro))
g + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Aprovação à Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                                                  x = "Há Microchip na Vacina contra a Covid-19?",
                                                  y= "Aprova o Governo de Jair Bolsonaro?",caption = "Fonte:IPEC 2021")


h <- ggplot (b, aes(microchip, bolsonaro))
h + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Certeza do voto em Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                                                  x = "Há Microchip na Vacina contra a Covid-19?",
                                                  y= "Votaria com certeza em Jair Bolsonaro?",
                                                  caption = "Fonte:IPEC 2021")
table(b$bolsonaro, b$microchip)
prop.table(table(b$bolsonaro, b$microchip),1)*100


# sente-se inseguro em relação à vacina

i <- ggplot (b, aes(inseguro, aprova))
i + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Aprovação à Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                                                  x = "Sente-se inseguro em relação às vacinas de Covid-19?",
                                                  y= "Aprova o Governo de Jair Bolsonaro?",caption = "Fonte:IPEC 2021")
table(b$aprova, b$inseguro)
prop.table(table(b$aprova, b$inseguro),1)*100

j <- ggplot (b, aes(inseguro, bolsonaro))
j + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Certeza do voto em Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                                                  x = "Sente-se inseguro em relação às vacinas de Covid-19?",
                                                  y= "Votaria com certeza em Jair Bolsonaro?",
                                                  caption = "Fonte:IPEC 2021")
table(b$bolsonaro, b$inseguro)
prop.table(table(b$bolsonaro, b$inseguro),1)*100


b$SEXO <- dados$SEXO

g <- ggplot (b, aes(microchip, aprova, color=SEXO))
g + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Aprovação à Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                                                  x = "Há Microchip na Vacina contra a Covid-19?",
                                                  y= "Aprova o Governo de Jair Bolsonaro?",caption = "Fonte:IPEC 2021")
b$EVANGELICO <- dados$Evangelico
b$ESCOLARIDADE <- dados$ESCOLARIDADE
b$ESCOLARIDADE <- as.numeric(b$ESCOLARIDADE)

g <- ggplot (b, aes(microchip, aprova, color=ESCOLARIDADE))
g + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Aprovação à Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                                                  x = "Há Microchip na Vacina contra a Covid-19?",
                                                  y= "Aprova o Governo de Jair Bolsonaro?",caption = "Fonte:IPEC 2021")

j <- ggplot (b, aes(inseguro, bolsonaro, color=ESCOLARIDADE))
j + geom_jitter(height = 0.4, width = 0.4) + labs(title = "Certeza do voto em Bolsonaro e negacionismo",
                                                  subtitle = "Pesquisa realizada em 2021",
                                                  x = "Sente-se inseguro em relação às vacinas de Covid-19?",
                                                  y= "Votaria com certeza em Jair Bolsonaro?",
                                                  caption = "Fonte:IPEC 2021")


