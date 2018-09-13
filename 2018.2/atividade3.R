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

# 1- a) com reposicao
tam10Com <- calcMediaVar(populacaoEleitores, 10, TRUE);
hist(tam10Com[1,], main = "Amostra de tamanho 10 com reposição", xlab = "Média amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9), ylab = "Frenquência")
hist(tam10Com[2,], main = "Amostra de tamanho 10 com reposição", xlab = "Variância amostral", xlim = range(0, 0.3), col = c(1,2,3,4,5,6,7,8), ylab = "Frenquência")

tam30Com <- calcMediaVar(populacaoEleitores,30, TRUE);
hist(tam30Com[1,], main = "Amostra de tamanho 30 com reposição", xlab = "Média amostral", ylim = range(0, 350), xlim = range(0.5,1) ,col = c(1,2,3,4,5,6,7,8), ylab = "Frenquência")
hist(tam30Com[2,], main = "Amostra de tamanho 30 com reposição", xlab = "Variância amostral", ylim = range(0, 250), xlim = range(0.05, 0.3), col = c(1,2,3,4,5,6,7,8), ylab = "Frenquência")

tam50Com <- calcMediaVar(populacaoEleitores, 50, TRUE);
hist(tam50Com[1,], main = "Amostra de tamanho 50 com reposição", xlab = "Média amostral", ylim = range(0, 350), col = c(1,2,3,4,5,6,7,8,9), xlim=range(0.5,1));
hist(tam50Com[2,], main = "Amostra de tamanho 50 com reposição", xlab = "Variância amostral", xlim = range(0, 0.3), col = c(1,2,3,4,5,6,7,8), ylab = "Frenquência");

# 1- b) sem reposicao
tam10Sem <- calcMediaVar(populacaoEleitores, 10, FALSE);
hist(tam10Sem[1,], main = "Amostra de tamanho 10 sem reposição", xlab = "Média amostral", ylim = range(0, 300), col = c(1,2,3,4,5,6,7,8,9), ylab = "Frenquência");
hist(tam10Sem[2,], main = "Amostra de tamanho 10 sem reposição", xlab = "Variância amostral", xlim = range(0, 0.3), col = c(1,2,3,4,5,6,7,8,9), ylab = "Frenquência");

tam30Sem <- calcMediaVar(populacaoEleitores, 30, FALSE);
hist(tam30Sem[1,], main = "Amostra de tamanho 30 sem reposição", xlab = "Média amostral", ylim = range(0, 350), xlim = range(0.5,1),col = c(1,2,3,4,5,6,7,8,9), ylab = 'Frenquência')
hist(tam30Sem[2,], main = "Amostra de tamanho 30 sem reposição", xlim=range(0.05,0.30),xlab = "Variância amostral",col = c(1,2,3,4,5,6,7,8), ylim = range(0, 350), ylab = 'Frenquência')

tam50Sem <- calcMediaVar(populacaoEleitores, 50, FALSE);
hist(x=tam50Sem[1,], main = "Amostra de tamanho 50 sem reposição", xlab = "Média amostral", ylim = range(0, 200), col = c(1,2,3,4,5,6,7,8,9), xlim = range(0.60, 0.90), ylab = 'Frenquência')
hist(x=tam50Sem[2,], main = "Amostra de tamanho 50 sem reposição", xlab = "Variância amostral", xlim = range(0.1, 0.25), ylim = range(0, 200),col = c(1,2,3,4,5,6,7,8),  ylab = 'Frenquência')


#   1-c)
mediaPopulacional <- mean(populacaoEleitores) # media populacional
varianciaPopulacional <- var(populacaoEleitores) # variancia populacional
mediaPopulacional
varianciaPopulacional

#   2)
mediaPoisson <- numeric(1000);
varianciaPoisson <- numeric(1000);
for(i in 1:1000) {
  amostraPois <- rpois(100,lambda = 10);
  mediaPoisson[i] <- mean(amostraPois);
  varianciaPoisson[i] <- var(amostraPois);
}
hist(mediaPoisson)
hist(varianciaPoisson)
hist(mediaPoisson, main = "Média populacional X ~ Poisson(10)", xlab = "Média", ylab="Frenquência", col = c(22,11,44,55,82), xlim = range(9, 11.5))
hist(varianciaPoisson, main = "Variância populacional X ~ Poisson(10)", xlab = "Variância", ylab="Frenquência", col = c(22,11,44,55,82), xlim = range(4,16))
# E(X) = lambda e Var(X) = lambda,no modelo de Poisson. Entao, media = var = 10

#   3)
estimadorT <- numeric(1000);
for(i in 1:1000) {
  amostraExp <- rexp(100, 5);
  estimadorT[i] <- 1/(mean(amostraExp));
}
mean(estimadorT)
var(estimadorT)
hist(estimadorT, main = "Histograma do Estimador T", xlim = range(3, 8), ylab = "Frequência", col = c(1,2,3,5))