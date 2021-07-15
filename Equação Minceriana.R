## Carregando pacotes
library(PNADcIBGE)
library(dplyr)
library(lmtest)

## Importando dados (Microdados da PNAD Contínuma - Ano: 2020 Trimestre: 3)
pnadc.svy <- get_pnadc(year=2020, quarter=3, vars=c('V1022','V1023','V2007','V2009','V2010','V40401','V40402','V40403','VD3004','VD3005','VD4001','VD4002','VD4009','VD4019','VD4031'), defyear=2020, defperiod=3,
                       labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
data<-data.frame(pnadc.svy[["variables"]])

#Filtrando dados
data<-data %>% filter(data$UF %in% c("São Paulo"),
                      data$V1023 %in% c("Capital"),
                      !is.na(data$VD4019),
                      data$VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",
                                         "Empregado no setor privado sem carteira de trabalho assinada",
                                         "Empregado no setor público com carteira de trabalho assinada",
                                         "Empregado no setor público sem carteira de trabalho assinada"))
data$V40403[is.na(data$V40403)]<-0
data$VD3005<-gsub('Sem instrução e menos de 1 ano de estudo',0,data$VD3005)
data$VD3005<-gsub('anos de estudo','',data$VD3005)
data$VD3005<-gsub('1 ano de estudo',1,data$VD3005)
data$VD3005<-gsub('16 anos ou mais de estudo',16,data$VD3005)
data$VD3005<-gsub(' ','',data$VD3005)
data$VD3005<-as.numeric(data$VD3005)
data<-mutate(data, S_PUB=ifelse(data$VD4009 %in% c("Empregado no setor público sem carteira de trabalho assinada",
                                                   "Empregado no setor público com carteira de trabalho assinada"),1,0),
             RURAL=ifelse(data$V1022 %in% c("Rural"),1,0),
             C_ASS=ifelse(data$VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",
                                             "Empregado no setor público com carteira de trabalho assinada"),1,0),
             GENERO=ifelse(data$V2007 %in% c("Homem"),1,0),
             BRANCO=ifelse(data$V2010 %in% c("Branca"),1,0),
             SUPERIOR=ifelse(data$VD3004 %in% c("Superior completo"),1,0))
data$EXP<-((ifelse(is.na(data$V40401),0,data$V40401))/12)+(ifelse(is.na(data$V40402),0,1+(data$V40402/12)))+data$V40403
Dados <- data %>% select(c("Ano","Trimestre","UF","V1022","V2007","V2009","V2010"
                           ,"EXP","VD3004","VD3005","VD4019","VD4031","S_PUB","RURAL","C_ASS"
                           ,"GENERO","BRANCO","SUPERIOR"))
colnames(Dados)<-c("ANO","TRI","UF","SIT_DOM","SEXO","IDADE","COR","EXP"
                   ,"INST","ANOS_EST","REND","HORAS","S_PUB","RURAL","C_ASS","GENERO"
                   ,"BRANCO","SUPERIOR")
Dados$REND_HORA<-Dados$REND/(Dados$HORAS*4)

## Estatísticas descritivas
summary(Dados)
prop.table(table(Dados$SIT_DOM))
prop.table(table(Dados$SEXO))
prop.table(table(Dados$COR))
prop.table(table(Dados$INST))

## Estimação do modelo
Dados$EXP2<-Dados$EXP^2
Dados$IDADE2<-Dados$IDADE^2
modelo<-lm(log(REND_HORA)~ANOS_EST+EXP+EXP2+GENERO+BRANCO+RURAL,Dados)
summary(modelo)

## Teste de Adição de variáveis
modelo.2<-lm(log(REND_HORA)~ANOS_EST+EXP+EXP2+GENERO+BRANCO+RURAL+C_ASS+S_PUB,Dados)
anova(modelo,modelo.2)

## Teste Reset
resettest(modelo, power = 2:3, type = "regressor", Dados)

##Teste de Breusch-Pagan
bptest(modelo, varformula = NULL, studentize = TRUE, data = Dados)

## Teste de Endogeneidade
endog<-lm(EXP~ANOS_EST+GENERO+BRANCO+RURAL,Dados)
summary(endog)
Dados$EXP_FITTED<-endog[["fitted.values"]]
Dados$EXP_FITTED2<-endog[["fitted.values"]]^2
Dados$V<-endog[["residuals"]]
modelo2<-lm(log(REND_HORA)~ANOS_EST+EXP_FITTED+EXP_FITTED2+V+GENERO+BRANCO+RURAL,Dados)
summary(modelo2)

## Mínimo Quadrado em dois estágios
estg1<-lm(EXP~ANOS_EST+IDADE,Dados)
summary(estg1)
Dados$EXP_FITTED<-estg1[["fitted.values"]]
Dados$EXP_FITTED2<-estg1[["fitted.values"]]^2
estg2<-lm(log(REND_HORA)~ANOS_EST+EXP_FITTED+EXP_FITTED2+GENERO+BRANCO+RURAL,Dados)
summary(estg2)
