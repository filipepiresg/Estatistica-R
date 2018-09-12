M <- 8000
a <- 1
c <- 5
m <- 10000
vetor <- numeric(m)
vetor[1] <- 80 # seed
for (i in 2:(m-1)) {
  vetor[i] <- (a*vetor[i-1] + c) %% M
}
q1 <- vetor/M
hist(q1, xlab="Vetor", ylab = "Frequencia", main = "Histograma para o gerador", col = c(3))

#q2 <- -(log(1-q1)) / 10
#hist(q2)
hist(-(log(1-q1))/10, main = "Histograma de dados exponencial", col = c, xlab = "F(t) = 1 -e^(-10t)")

hist(rpois(1000, 5), main = "Histograma de Poisson", col = c, ylim=range(0,250),xlab = "Poisson com 1000 variaveis e lambda = 5")
#q3 <- rpois(1000, 5)
#hist(q3)

#hist(rnorm(1000, 500, 100), main = "Histograma de Sn")
tabela <- function(a, b, c) {
  result <- c()
  for(i in 1:1000){
    result[i] <- sum(rnorm(a, b, c))
  }
  return(result)
}

somatorio <- tabela(100, 5, 1)

hist(somatorio, main = "Histograma de Sn", xlab = "∑Xi ~ N(n.mean, n.sd)", col = c(7))

