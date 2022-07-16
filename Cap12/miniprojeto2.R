setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")

#Instalando os pacotes
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")

library(dplyr)
library(data.table)
library(ggplot2)

dados_iris <- iris
View(dados_iris)

library(dplyr)
medias_iris <- summarize(
  group_by(dados_iris, Species),
  media_sepal_length = mean(Sepal.Length),
  media_sepal_width = mean(Sepal.Width),
  media_petal_length = mean(Petal.Length),
  media_petal_width = mean(Petal.Width)
)
View(medias_iris)

library(data.table)
dados_iris_id <-dados_iris
View(dados_iris_id)
dados_iris_id$Sepal.Length<- as.integer(dados_iris_id$Sepal.Length)
View(dados_iris_id)

library(data.table)
dados_iris_id <-data.table(dados_iris)
dados_iris_id$Sepal.Length <- as.integer(dados_iris_id$Sepal.Length)

library(ggplot2)
ggplot(data=dados_iris , aes(x=Petal.Width , y =Petal.Length))+
  geom_point(aes(color=Species), size= 3)+
  ggtitle("Largura e comprimento das petalas")+
  labs(
    x="Largura da petala",
    y="Altura da petala") +
    theme_bw() +
    theme (title = element_text(size=15, color= "turquoise4"))

#Cores disponíveis 
colours()
