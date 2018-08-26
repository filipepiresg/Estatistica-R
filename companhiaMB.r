dados<-read.csv("DadosCiaMB.csv", header=TRUE,sep=";",dec=",")

attach(dados)


barplot(table(Instrucao))

barplot(prop.table(table(Instrucao)),col=c("darkgreen","darkblue","black"),main="Gráfico de Barras da Variável Grau de Instrução",xlab="Grau de Instrução")


#gr?fico em barras
ni<-c(12,18,6)
names(ni)<-c("Fundamental","MÃ©dio","Superior")
barplot(ni,col="yellow",ylab="frequÃªncia(ni)",xlab="Grau de instruÃ§Ã£o",ylim=range(0,20))


#gráfico de pizza
table(Instrucao)
pie(table(Instrucao))
pie(table(Instrucao),main="Grau de Instrução",labels=c("33%","50%","17%"),col=c(1,2,3))
legend("topright",fill=c(1,2,3),legend=c("fundamental","médio","superior"))

tabela1<-table(Filhos)
barplot(tabela1,main="Gráfico em barras para a variável Número de Filhos",col=c(1,2,3,4,5))
 
hist(salario)

hist(salario,prob=T,ylim=c(0,0.1),breaks=c(4,8,12,16,20,24),col="green",main="Histograma da variÃ¡velvel salÃ¡rio",xlim=c(4,24))
text(locator(n=5),c("28%","33%","22%","14%","3%"))

boxplot(salario,col="yellow")
text(locator(n=5),c("max(x_(1),LI)","q_1","Md","q_3","min(x_(n),LS)"))

boxplot(salario)

boxplot(salario~Instrucao,col="blue")

summary(idade)
plot(salario~idade)
cor(salario,idade)
boxplot(salario~procedencia)
