populacao <- numeric(100)
for(i in 1:75) {
  populacao[i] <- 1
}

# 1- a)
mediaDe10com <- numeric(1000)
varianciaDe10com <- numeric(1000)
for(i in 1:1000) {
  amostra1com <- sample(populacao, size=10, replace = TRUE)
  mediaDe10com[i] <- mean(amostra1com)
  varianciaDe10com[i] <- var(amostra1com)
}
hist(mediaDe10com, main = "Histograma da amostra de tamanho 10 com reposição", xlab = "media amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9))
hist(varianciaDe10com, main = "Histograma da amostra de tamanho 10 com reposição", xlab = "variancia amostral", xlim = range(0, 0.3))

mediaDe30com <- numeric(1000)
varianciaDe30com <- numeric(1000)
for(i in 1:1000) {
  amostra2com <- sample(populacao, size=30, replace = TRUE)
  mediaDe30com[i] <- mean(amostra2com)
  varianciaDe30com[i] <- var(amostra2com)
}
hist(mediaDe30com, main = "Histograma da amostra de tamanho 30 com reposição", xlab = "media amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9))
hist(varianciaDe30com, main = "Histograma da amostra de tamanho 30 com reposição", xlab = "variancia amostral", xlim = range(0, 0.3))

mediaDe50com <- numeric(1000)
varianciaDe50com <- numeric(1000)
for(i in 1:1000) {
  amostra3com <- sample(populacao, size=50, replace = TRUE)
  mediaDe50com[i] <- mean(amostra3com)
  varianciaDe50com[i] <- var(amostra3com)
}
hist(mediaDe50com, main = "Histograma da amostra de tamanho 50 com reposição", xlab = "media amostral", ylim = range(0, 400), col = c(1,2,3,4,5,6,7,8,9))
hist(varianciaDe50com, main = "Histograma da amostra de tamanho 50 com reposição", xlab = "variancia amostral", xlim = range(0, 0.3))

# 1- b)
mediaDe10sem <- numeric(1000)
varianciaDe10sem <- numeric(1000)
for(i in 1:1000) {
  amostra1sem <- sample(populacao, size=10, replace = FALSE)
  mediaDe10sem[i] <- mean(amostra1sem)
  varianciaDe10sem[i] <- var(amostra1sem)
}
hist(mediaDe10sem, main = "Histograma da amostra de tamanho 10 sem reposição", xlab = "media amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9))
hist(varianciaDe10sem, main = "Histograma da amostra de tamanho 10 sem reposição", xlab = "variancia amostral", xlim = range(0, 0.3))

mediaDe30sem <- numeric(1000)
varianciaDe30sem <- numeric(1000)
for(i in 1:1000) {
  amostra2sem <- sample(populacao, size=30, replace = FALSE)
  mediaDe30sem[i] <- mean(amostra2sem)
  varianciaDe30sem[i] <- var(amostra2sem)
}
hist(mediaDe30sem, main = "Histograma da amostra de tamanho 30 sem reposição", xlab = "media amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9))
hist(varianciaDe30sem, main = "Histograma da amostra de tamanho 30 sem reposição", xlab = "variancia amostral", xlim = range(0, 0.3))

mediaDe50sem <- numeric(1000)
varianciaDe50sem <- numeric(1000)
for(i in 1:1000) {
  amostra3sem <- sample(populacao, size=50, replace = FALSE)
  mediaDe50sem[i] <- mean(amostra3sem)
  varianciaDe50sem[i] <- var(amostra3sem)
}
hist(mediaDe50sem, main = "Histograma da amostra de tamanho 50 sem reposição", xlab = "media amostral", ylim = range(0, 400), col = c(1,2,3,4,5,6,7,8,9))
hist(varianciaDe50sem, main = "Histograma da amostra de tamanho 50 sem reposição", xlab = "variancia amostral", xlim = range(0, 0.3))


#  1-c)
mean(populacao) # media populacional
var(populacao) # variancia populacional
