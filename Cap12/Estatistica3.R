setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")

vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252" )

head(vendas)
tail(vendas)

#Medidas de Tendência Central
summary(vendas[c('Valor', 'Custo')])

#Variáveis Numéricas
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01, 0.99))
quantile(vendas$Valor, seq(from=0, to=1, by=0.20))
#Diferença do Q3 e Q1
IQR(vendas$Valor) 
range(vendas$Valor)
diff(range(vendas$Valor))
