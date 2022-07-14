setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")


?read.table
dados <- read.table("Usuarios.csv",
             dec = ".",
             sep = ",",
             h= T,
             fileEncoding = "windows-1252"
             )

#Tabela de frequência absoluta
freq<- table(dados$grau_instrucao)
View(freq)

#Tabela de frequências Relativas
freq_rel <- prop.table(freq)
View(freq_rel)

#Porcentagem(100* freq_rel_table)
p_freq_rel <-100* prop.table(freq_rel)
View(p_freq_rel)

#Adiciona linhas de total
freq <- c(freq,sum(freq))
View(freq)
names(freq) [4] <- "Total"
View(freq)

#Tabela final com todos os valores

#Calculamos frênquencia relativa e frequência proporcional
freq_rel <- c(freq_rel, sum(freq_rel))
p_freq_rel <- c(p_freq_rel,sum(p_freq_rel))

#Tabela final com todos os vetores
tabela_final <- cbind(freq,
                      freq_rel =round( freq_rel,digits=2 ),
                      p_freq_rel = round(p_freq_rel, digits=2))


View(tabela_final)