#PREPARAÇÃO, ORGANIZAÇÃO E ESTRUTURAÇÃO DOS DADOS (PRÉ PROCESSAMENTO)


#BAIXAR PACOTES
if(!require(dplyr)) install.packages("dplyr") # Manipulação de Dados
if(!require(ggplot2)) install.packages("ggplot2") # Gráficos alto nível

#CARREGAR PACOTES
library(dplyr)
library(ggplot2)

#ABRIR ARQUIVO
enem2019 <- read.csv('ENEM_SP_2019.csv')

View(enem2019)

# EXCLUIR UMA COLUNA
enem2019$NU_NOTA_CN <- NULL
# EXCLUIR VÁRIAS COLUNAS
excluir <- c("NU_NOTA_CH", "NU_NOTA_LC","NU_NOTA_MT")
enem2019 <- enem2019[,!(names(enem2019)%in% excluir)]
excluir2 <- c("NU_ANO", "SG_UF_RESIDENCIA")
enem2019 <- enem2019[,!(names(enem2019)%in% excluir2)]

#RENOMEAR UMA COLUNA
enem2019 <- rename(enem2019, NOTA_COMP1 = NU_NOTA_COMP1)
#RENOMEAR VÁRIAS COLUNAS
enem2019 <- rename(enem2019, NOTA_COMP2 = NU_NOTA_COMP2, NOTA_COMP3 = NU_NOTA_COMP3,
                   NOTA_COMP4 = NU_NOTA_COMP4,NOTA_COMP5 = NU_NOTA_COMP5,
                   NOTA_REDACAO = NU_NOTA_REDACAO)

#VERIFICAÇÃO DA TIPAGEM DOS ATRIBUTOS
str(enem2019)
# OU
glimpse(enem2019)

#Transformando a variável Código escola em caracter
enem2019$CO_ESCOLA <- as.factor(enem2019$CO_ESCOLA)

#Verificando valores missing
sapply(enem2019, function(x) sum(is.na(x)))

#TREINEIROS
treineiros <- enem2019 %>% filter(IN_TREINEIRO==1)# 93991 treineiros

#RETIRAR TREINEIROS
vestibulandos <- enem2019 %>% filter(IN_TREINEIRO==0)

# EXCLUIR UMA COLUNA
vestibulandos$IN_TREINEIRO <- NULL

#Exportando o arquivo treineiros
write.table(treineiros, file ="treineiros.csv")

#CRIANDO COLUNA
vestibulandos["presenca"] <- vestibulandos$TP_PRESENCA_CN + vestibulandos$TP_PRESENCA_CH +
                        vestibulandos$TP_PRESENCA_LC + vestibulandos$TP_PRESENCA_MT


vestibulandos %>% filter(presenca==0) #185137 não compareceram nos dois dias
vestibulandos %>% filter(presenca==2) #33686 não compareceram em um dos dias
vestibulandos %>% filter(presenca==6) # 301 desclassificados em um dos dias
vestibulandos %>% filter(presenca==8) # nenhum desclassificado nos dois dias

#SELECIONANDO APENAS OS QUE COMPARECERAM NOS DOIS DIAS.
vestibulandos_presentes <- vestibulandos %>% filter(presenca==4)

#Verificando valores missing
sapply(vestibulandos_presentes, function(x) sum(is.na(x)))

vestibulandos_presentes %>% filter(NOTA_REDACAO==0) #9141 notas zeros
vestibulandos_presentes %>% filter(NOTA_COMP1==0) #9148 notas zeros
vestibulandos_presentes %>% filter(NOTA_COMP2==0) #9141 notas zeros
vestibulandos_presentes %>% filter(NOTA_COMP3==0) #9157 notas zeros
vestibulandos_presentes %>% filter(NOTA_COMP4==0) #9154 notas zeros
vestibulandos_presentes %>% filter(NOTA_COMP5==0) #73293 notas zeros

vestibulandos_presentes %>% filter(TP_STATUS_REDACAO==1) #490730 redações sem problemas

vestibulandos_presentes %>% filter(NOTA_MT==0) #602 notas zeros
vestibulandos_presentes %>% filter(NOTA_CH==0) #889 notas zeros
vestibulandos_presentes %>% filter(NOTA_CN==0) #582 notas zeros
vestibulandos_presentes %>% filter(NOTA_LC==0) #681 notas zeros

#VERIFICAÇÃO DA TIPAGEM DOS ATRIBUTOS
str(enem2019)

#SUBSTITUINDO NA POR 0 PARA NÃO DAR PROBLEMAS NA ANÁLISE ESTATÍSTICA
vestibulandos_presentes$NOTA_COMP1[which(is.na(vestibulandos_presentes$NOTA_COMP1))] <- 0
vestibulandos_presentes$NOTA_COMP2[which(is.na(vestibulandos_presentes$NOTA_COMP2))] <- 0
vestibulandos_presentes$NOTA_COMP3[which(is.na(vestibulandos_presentes$NOTA_COMP3))] <- 0
vestibulandos_presentes$NOTA_COMP4[which(is.na(vestibulandos_presentes$NOTA_COMP4))] <- 0
vestibulandos_presentes$NOTA_COMP5[which(is.na(vestibulandos_presentes$NOTA_COMP5))] <- 0
vestibulandos_presentes$NOTA_REDACAO[which(is.na(vestibulandos_presentes$NOTA_REDACAO))] <- 0

#Verificando valores missing
sapply(vestibulandos_presentes, function(x) sum(is.na(x)))

# EXCLUIR UMA COLUNA
vestibulandos$presenca <- NULL

#EXPORTAR ARQUIVO TRATADO
write.table(vestibulandos_presentes, file ="enem2019_tratado.csv")

