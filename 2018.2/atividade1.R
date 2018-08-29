dados <- read.csv("conjuntodeDados.csv", header = TRUE, sep = ";", dec = ",")
attach(dados)

table(Origem)
ni<-c(12,18)
names(ni)<-c("Importado","Nacional")
barplot(ni, main = "Grafico de origem de veiculos", ylim = c(0,20), col = c(1,2))
text(locator(n=2),c("40%","60%"))

hist(Preco, main = "Histograma de Preco dos Veiculos", ylim = range(0, 12), col=c(1,2,3,4,5,6,7,8))
hist(Comprimento, main = "Histograma do Comprimento dos Veiculos", xlim = range(3, 5), col=c(1,2,3,4,5,6,7), ylim = c(0, 10))

mean(Motor)

mean(Preco)
mean(Comprimento)

plot(Preco~Origem, main = "Analise de Preco por Origem", col = c(2,3))

help("boxplot")
