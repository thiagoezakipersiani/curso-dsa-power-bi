setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")

vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252" )

View(vendas)
str(vendas)
summary(vendas$Valor)
summary(vendas$Custo)

#Media
?mean
mean(vendas$Valor)
mean(vendas$Custo)

#Media ponderada
?weighted.mean
weighted.mean(vendas$Valor, w = vendas$Custo)

#Mediana
median(vendas$Valor)
median(vendas$Custo)

#Função moda
moda <-function(v){
  valor_unico<- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

resultado <-moda(vendas$Valor)
print(resultado)

install.packages("ggplot2")
library(ggplot2)

ggplot(vendas) +
  stat_summary(aes(x=Estado,
                   y = Valor),
               fun=mean,
               geom = "bar",
               fill= "lightgreen",
               col= "grey50") + 
  labs(title= "Média de valor por Estado")