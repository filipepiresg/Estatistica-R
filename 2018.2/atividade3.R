populacaoEleitores <- numeric(100)
for(i in 1:75) {
  populacaoEleitores[i] <- 1
}

calcMediaVar <- function(pop,n, repo) {
  media <- numeric(1000);
  variancia <- numeric(1000);
  for(i in 1:1000) {
    amostra <- sample(pop, size = n, replace = repo);
    media[i] <- mean(amostra);
    variancia[i] <- var(amostra);
  }
  retorno <- matrix(nrow = 2, ncol = 1000);
  retorno[1,] <- media;
  retorno[2,] <- variancia;
  return(retorno);
}

# 1- a)
tam10Com <- calcMediaVar(populacaoEleitores, 10, TRUE);
hist(tam10Com[1,], main = "Histograma da amostra de tamanho 10 com reposição", xlab = "media amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9))
hist(tam10Com[2,], main = "Histograma da amostra de tamanho 10 com reposição", xlab = "variancia amostral", xlim = range(0, 0.3), col = c(1,2,3,4,5,6,7,8))

tam30Com <- calcMediaVar(populacaoEleitores,30, TRUE);
hist(tam30Com[1,], main = "Histograma da amostra de tamanho 30 com reposição", xlab = "media amostral", ylim = range(0, 350), col = c(1,2,3,4,5,6,7,8))
hist(tam30Com[2,], main = "Histograma da amostra de tamanho 30 com reposição", xlab = "variancia amostral", ylim = range(0, 350), xlim = range(0, 0.3), col = c(1,2,3,4,5,6,7,8))

tam50Com <- calcMediaVar(populacaoEleitores, 50, TRUE);
hist(tam50Com[1,], main = "Histograma da amostra de tamanho 50 com reposição", xlab = "media amostral", ylim = range(0, 400), col = c(1,2,3,4,5,6,7,8,9));
hist(tam50Com[2,], main = "Histograma da amostra de tamanho 50 com reposição", xlab = "variancia amostral", xlim = range(0, 0.3), col = c(1,2,3,4,5,6,7,8));

# 1- b)
tam10Sem <- calcMediaVar(populacaoEleitores, 10, FALSE);
hist(tam10Sem[1,], main = "Histograma da amostra de tamanho 10 sem reposição", xlab = "media amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9))
hist(tam10Sem[2,], main = "Histograma da amostra de tamanho 10 sem reposição", xlab = "variancia amostral", xlim = range(0, 0.3), col = c(1,2,3,4,5,6,7,8,9))

tam30Sem <- calcMediaVar(populacaoEleitores, 30, FALSE);
hist(tam30Sem[1,], main = "Histograma da amostra de tamanho 30 sem reposição", xlab = "media amostral", ylim = range(0, 350), xlim = range(0.5,1),col = c(1,2,3,4,5,6,7,8,9), ylab = 'Frenquência')
hist(tam30Sem[2,], main = "Histograma da amostra de tamanho 30 sem reposição", xlim=range(0.05,0.30),xlab = "variancia amostral",col = c(1,2,3,4,5,6,7,8), ylim = range(0, 350), ylab = 'Frenquência')

tam50Sem <- calcMediaVar(populacaoEleitores, 50, FALSE);
hist(x=tam50Sem[1,], main = "Histograma da amostra de tamanho 50 sem reposição", xlab = "media amostral", ylim = range(0, 200), col = c(1,2,3,4,5,6,7,8,9), xlim = range(0.60, 0.90), ylab = 'Frenquência')
hist(x=tam50Sem[2,], main = "Histograma da amostra de tamanho 50 sem reposição", xlab = "variancia amostral", xlim = range(0.1, 0.25), ylim = range(0, 200),col = c(1,2,3,4,5,6,7,8),  ylab = 'Frenquência')


#   1-c)
mediaPopulacional <- mean(populacaoEleitores) # media populacional
varianciaPopulacional <- var(populacaoEleitores) # variancia populacional
mediaPopulacional
varianciaPopulacional

#   2)
mediaPoisson <- numeric(1000);
varianciaPoisson <- numeric(1000);
for(i in 1:1000) {
  amostraPois <- rpois( n =100,lambda = 10);
  mediaPoisson[i] <- mean(amostra);
  varianciaPoisson[i] <- var(amostra);
}
hist(mediaPoisson, main = "Média populacional X ~ Poisson(10)", xlim = range(9,11.5), xlab = "média", ylab="Frenquência", col = c(22,11,44,55,82))
hist(varianciaPoisson, main = "Variância populacional X ~ Poisson(10)", ylim = range(0, 300), xlab = "variância", ylab="Frenquência", col = c(1,2,3,4))
# E(X) = lambda e Var(X) = lambda,no modelo de Poisson. Entao, media = var = 10

#   3)
mediaExp <- numeric(1000);
varianciaExp <- numeric(1000);
estimadorT <- c();
for(i in 1:1000) {
  amostraExp <- rexp(100, 5);
  mediaExp[i] <- mean(amostraExp)
  estimadorT[i] <- 1/mediaExp[i];
}
hist(mediaExp)

votos <- integer(100)
