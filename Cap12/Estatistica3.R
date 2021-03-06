setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")

vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252" )

head(vendas)
tail(vendas)

#Medidas de Tend�ncia Central
summary(vendas[c('Valor', 'Custo')])

#Vari�veis Num�ricas
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01, 0.99))
quantile(vendas$Valor, seq(from=0, to=1, by=0.20))
#Diferen�a do Q3 e Q1
IQR(vendas$Valor) 
range(vendas$Valor)
diff(range(vendas$Valor))
