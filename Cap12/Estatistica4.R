setwd("C:/Users/thiago.persiani/Desktop/Estudo power bi/Cap12")

#Dados
vetor_total_resultados = c(3,12,5,18,45)
names(vetor_total_resultados) = c("A", "B","C","D","E")
vetor_total_resultados

?barplot
barplot(vetor_total_resultados)
barplot(vetor_total_resultados, col= c(1,2,3,4,5))

#Salvando o grafico em disco
png("barplot.png" , width = 480, height = 480)
barplot(vetor_total_resultados,
        col = rgb(0.5,0.1,0.6,0.6),
        xlab = "Categorias",
        yblab = "Valores",
        main= "Barplot em R",
        ylin= c(0,60)
        )
dev.off()

# Ggplot2 
library(ggplot2)
View(mtcars)

# Barplot
ggplot(mtcars, aes(x = as.factor(cyl) )) + 
  geom_bar()

ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue"))

# Criando dados dummy
dados = data.frame(group = c("A ","B ","C ","D ") , value = c(33,62,56,67))
View(dados)

# Barplot
ggplot(dados, aes(x = group, y = value ,fill = group )) + 
  geom_bar(width = 0.85, stat = "identity")


##### Pie Chart ##### 

fatias <- c(4, 12, 14, 16, 8)
paises <- c("Brasil", "Estados Unidos", "Alemanha", "Reino Unido", "Espanha")
pie(fatias, labels = paises, main = "Leitura de Livros Por Pessoa/Ano")

# Pie Chart 3D
install.packages("plotrix")
library(plotrix)

fatias <- c(4, 12, 14, 16, 8)
paises <- c("Brasil", "Estados Unidos", "Alemanha", "Reino Unido", "Espanha")
?pie3D
pie3D(fatias, labels = paises, explode = 0.1, main = "Leitura de Livros Por Pessoa/Ano")


##### Line Chart ##### 

# Dados
carros <- c(1, 3, 6, 4, 9)
caminhoes <- c(2, 5, 4, 5, 12)

# Plot
plot(carros, type = "o", col = "blue", ylim = c(0,12))

lines(caminhoes, type = "o", pch = 22, lty = 2, col = "red")

title(main = "Produ��o de Ve�culos", col.main = "red", font.main = 4)


##### Boxplot ##### 
View(mpg)

# Plot
ggplot(mpg, aes(x = reorder(class, hwy), y = hwy, fill = class)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position = "none")


##### Scatter Plot ##### 

library(ggplot2)
data = data.frame(cond = rep(c("condition_1", "condition_2"), each=10), 
                  my_x = 1:100 + rnorm(100,sd=9), my_y = 1:100 + rnorm(100,sd=16))

View(data)

ggplot(data, aes(x=my_x, y=my_y)) + 
  geom_point(shape=1)

# Adiciona linha de regress�o
ggplot(data, aes(x=my_x, y=my_y)) +    
  geom_point(shape=1) +  
  geom_smooth(method = lm , color="red", se=FALSE) 

# Adiciona smooth
ggplot(data, aes(x=my_x, y=my_y)) +    
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=TRUE)  


##### Treemap #####

install.packages("treemap")
library(treemap)

# Dados
grupo = c(rep("grupo-1",4), rep("grupo-2",2), rep("grupo-3",3))
subgrupo = paste("subgroup", c(1,2,3,4,1,2,1,2,3), sep = "-")
valor = c(13,5,22,12,11,7,3,1,23)
dados = data.frame(grupo, subgrupo, valor)
View(dados)

# Labels
?treemap
treemap(dados, 
        index = c("grupo", "subgrupo"),     
        vSize = "valor", 
        type = "index",
        fontsize.labels = c(15,12),               
        fontcolor.labels = c("white", "orange"),    
        fontface.labels = c(2,1),                 
        bg.labels = 220,             
        align.labels = list(c("center", "center"), c("right", "bottom")),                                  
        overlap.labels = 0.5,           
        inflate.labels = F)


# Customizando
treemap(dados, 
        index = c("grupo", "subgrupo"), 
        vSize = "valor", 
        type = "index",
        border.col = c("black", "white"),          
        border.lwds = c(7,2))


##### Histograma #####

# Gerando valores para x
x <- mtcars$mpg 

# Criando o histograma
?hist
h <- hist(x, 
          breaks = 10, 
          col = "red", 
          xlab = "Milhas Por Gal�o", 
          main = "Histograma com Curva de Distribui�ao") 

# Customizando o histograma
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col = "blue", lwd = 2)

# Usando o ggplot2
library(ggplot2)

# dataset
dados = data.frame(value = rnorm(10000))
View(dados)

# Tamanho das colunas
ggplot(dados, aes(x=value)) + 
  geom_histogram(binwidth = 0.05)

# Cor uniforme
ggplot(dados, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4) ) 

# Cor proporcional
ggplot(dados, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, aes(fill = ..count..) )