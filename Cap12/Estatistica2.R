setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")

vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252" )

View(vendas)
str(vendas)
summary(vendas$Valor)

#Variancia
var(vendas$Valor)

#Desvio padr�o
sd(vendas$Valor)

 