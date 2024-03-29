#Instalando os pacotes
install.packages("Amelia")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("reshape")
install.packages("e1071")
install.packages("randomForest")

#Carregando os pacotes
library("Amelia")
library("caret")
library("ggplot2")
library("dplyr")
library("reshape")
library("reshape")
library("e1071")
library("randomForest")

#Carregando os dados do dataset
dados_clientes <- read.csv("dados/dataset.csv")

#An�lise explorat�ria, limpeza e transforma��o
dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

#Renomeando a coluna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "inadimplente"
colnames(dados_clientes)
View(dados_clientes)

#verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
?missmap
missmap(dados_clientes, main="Valores missing observados")
dados_clientes<-na.omit(dados_clientes)

#Convertendo os atributos genero,escolaridade, estado civil e idade

#Renomeando colunas categoricas
colnames(dados_clientes)
colnames(dados_clientes)[2] <-"Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <-"Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)
View(dados_clientes)

#Genero
View(dados_clientes$Genero)
str(dados_clientes$Genero)
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero,
                             c(0,1,2),
                             labels = c("Masculino",
                                        "Feminino"))
View(dados_clientes$Genero)
str(dados_clientes$Genero)

#Escolaridade
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
?cut
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
                             c(0,1,2,3,4),
                             labels = c("Pos Graduado",
                                        "Graduado",
                                         "Ensino Medio",
                                        "Outros"))
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)

#Estado civil
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
?cut
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,
                                   c(-1,0,1,2,3),
                                   labels = c("Desconhecido",
                                              "Casado",
                                              "Solteiro",
                                              "Outro"))
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)

#Convertendo a variavel para faixa etaria
View(dados_clientes$Idade)
str(dados_clientes$Idade)
?cut
dados_clientes$Idade <- cut(dados_clientes$Idade,
                                   c(0,30,50,100),
                                   labels = c("Jovem",
                                              "Adulto",
                                              "Idoso"))
View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)

#Convertendo a variavel que indica pagamentos para o tipo fator
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

#Dataset ap�s a convers�o
str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores missing observados")
dados_clientes <- na.omit(dados_clientes)
missmap(dados_clientes, main="Valores missing observados")
dim(dados_clientes)

#Alterando a variavel dependente para o tipo fator
str(dados_clientes$inadimplente)
collnames(dados_clientes)
dados_clientes$inadimplente <- as.factor(dados_clientes$inadimplente)
View(dados_clientes)

#Total de indimplentes versus n�o-inadimplentes
?table
table(dados_clientes$inadimplente)

#Vejamos as porcentagens entre as classes
prop.table(table(dados_clientes$inadimplente))

#Plot da distribui��o usando ggplot2
qplot(inadimplente, data= dados_clientes, geom="bar") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Set seed
set.seed(12345)

#Amostragem estratificada
#Seleciona as linhas de acordo com a vari�vel inadimplente como strata
?createDataPartition
indice <- createDataPartition(dados_clientes$inadimplente, p= 0.75, list=FALSE)
dim(indice)

#Definimos os dados de treinamento como subconjunto do conjunto de dados original
#Com n�meros de indice de linha (conforme indicado acima) E TODAS as colunas
dados_treino <- dados_clientes[indice,]
table(dados_treino$inadimplente)

#Veja a porcentagens entre as classes 
prop.table(table(dados_treino$inadimplente))

#Comparamos as porcentagens entre as classes de treinamento e dados originais
compara_dados <- cbind(prop.table(table(dados_treino$inadimplente)),
                                  prop.table(table(dados_clientes$inadimplente)))
colnames(compara_dados) <-c("Treinamento", "Original")
compara_dados

#Melt data - Converte colunas em linhas
?reshape2::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

#Plot para ver distribui��o do treinamento vs original
ggplot(melt_compara_dados, aes(x=X1, y=value)) +
   geom_bar(aes(fill=X2), stat = "identity" , position = "dodge") +
  theme(axis.text.x =  element_text(angle=90, hjust = 1))

#Tudo o que n�o est� no dataset de treinamento est� no dataset de teste. Observe o sinal de - (menos)
dados_teste <-dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)

#Machine learning
?randomForest
modelo_v1 <-randomForest(inadimplente ~ ., data=dados_treino)
modelo_v1

#Avaliando o modelo
plot(modelo_v1)

#Previs�es com dados testes
previsoes_v1 <- predict(modelo_v1, dados_teste)

#Configura��es matriz 
cm_v1 <- caret::confusionMatrix(previsoes_v1,dados_teste$inadimplente,positive="1")
cm_v1

#Calculando prevision, recall e f1 score, metricas de avalia��o do modelo preditivo
y <- dados_teste$inadimplente
v_pred_v1<-previsoes_v1

precision <- posPredValue(v_pred_v1,y)
precision

recall<- sensitivity(v_pred_v1,y)
recall

f1 <- (2* precision* recall) / (precision+recall)
f1


library(DMwR)

table(dados_treino$inadimplente)
prop.table(table(dados_treino$inadimplente))
set.seed(9560)
dados_treino_bal <- SMOTE(inadimplente ~ ., data= dados_treino)
table(dados_treino_bal$inadimplente)
prop.table(table(dados_treino_bal$inadimplente))

#Construindo a segunda vers�o do modelo
modelo_v2 <- randomForest(inadimplente ~ ., data= dados_treino_bal)
modelo_v2

#Avaliando o modelo
plot(modelo_v2)

#Previs�es com dados testes
previsoes_v2 <- predict(modelo_v2, dados_teste)

#Configura��es matriz 
cm_v2 <- caret::confusionMatrix(previsoes_v2,dados_teste$inadimplente,positive="1")
cm_v2

#Calculando prevision, recall e f1 score, metricas de avalia��o do modelo preditivo
y <- dados_teste$inadimplente
v_pred_v2<-previsoes_v2

precision <- posPredValue(v_pred_v2,y)
precision

recall<- sensitivity(v_pred_v2,y)
recall

f1 <- (2* precision* recall) / (precision+recall)
f1

#Obtendo as variaveis mais importantes
imp_var <- importance(modelo_v2)
varImportance <- data.frame(Variables = row.names(imp_var),
                            Importance= round(imp_var[, 'MeanDecreaseGini'],2))

#Criando o rank de variaveis baseada na importancia
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

#Usando o ggplot2 para visualizar
ggplot(rankImportance,
       aes(x= reorder(Variables,Importance),
       y= Importance,
       fill= Importance)) +
    geom_bar(stat='identity') +
    geom_text(aes(x = Variables , y = 0.5 , label = Rank),
              hjust = 0,
              vjust= 0.55,
              size=4,
              colour='red') +
  labs(x='Variables') + 
  coord_flip()


colnames(dados_treino_bal)
modelo_v3 <- randomForest(inadimplente ~ PAY_0 + PAY_2 + PAY_3 + PAY_AMT1 + PAY_AMT2 + PAY_5 + BILL_AMT1, 
                          data = dados_treino_bal)
modelo_v3

# Avaliando o modelo
plot(modelo_v3)

# Previsoes com dados de teste
previsoes_v3 <- predict(modelo_v3, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v3 <- caret::confusionMatrix(previsoes_v3, dados_teste$inadimplente, positive = "1")
cm_v3

# Calculando Precision, Recall e F1-Score, metricas de avalia�ao do modelo preditivo
y <- dados_teste$inadimplente
y_pred_v3 <- previsoes_v3

precision <- posPredValue(y_pred_v3, y)
precision

recall <- sensitivity(y_pred_v3, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

#Salvando o modelo
saveRDS(modelo_v3, file="modelo/modelo_v3.rds")

#Carregando o modelo 
modelo_final <- readRDS('modelo/modelo_v3.rds')

# Dados dos clientes
PAY_0 <- c(0, 0, 0) 
PAY_2 <- c(0, 0, 0) 
PAY_3 <- c(1, 0, 0) 
PAY_AMT1 <- c(1100, 1000, 1200) 
PAY_AMT2 <- c(1500, 1300, 1150) 
PAY_5 <- c(0, 0, 0) 
BILL_AMT1 <- c(350, 420, 280) 

# Concatena em um dataframe
novos_clientes <- data.frame(PAY_0, PAY_2, PAY_3, PAY_AMT1, PAY_AMT2, PAY_5, BILL_AMT1)
View(novos_clientes)

# Previsoes
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)

# Checando os tipos de dados
str(dados_treino_bal)
str(novos_clientes)

# Convertendo os tipos de dados
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino_bal$PAY_5))
str(novos_clientes)

# Previsoes
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)
View(previsoes_novos_clientes)
