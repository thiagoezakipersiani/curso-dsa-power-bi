setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")

notas <- read.csv("Notas.csv", fileEncoding = "windows-1252" )

View(notas)
#1
summary(notas)

#2
mean(notas$TurmaA)
mean(notas$TurmaB)

#3
sd(notas$TurmaA)
sd(notas$TurmaB)

#4
media_ta <- mean(notas$TurmaA)
media_tb <- mean(notas$TurmaB)

sd_ta <-sd(notas$TurmaA)
sd_tb <- sd(notas$TurmaB)

cv_ta <- sd_ta / media_ta * 100
cv_tb <- sd_tb / media_tb * 100

cv_ta
cv_tb

#5
moda <-function(v){
  valor_unico<- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

resultadoa <-moda(notas$TurmaA)
resultadob <-moda(notas$TurmaB)

print(resultadoa)
print(resultadob)
