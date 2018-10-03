# 1
# letra a)
eleitorCandA <- 522097
eleitorDifCandA <- 720991
tamPop <- eleitorCandA + eleitorDifCandA
populacao <- numeric(tamPop)
for( i in 1: eleitorCandA) {
  populacao[i] <- 1;
}

proporcaoPopulacional <- sum(populacao)/tamPop
proporcaoPopulacional

# codigo em comum pra letra b)
calculaProporcaoAmostral <- function(k, n, conf.level = 0.95) {
  amostra <- c()
  for(j in 1: k) {
    amostra[j] <- (sum(sample(populacao, n, replace = F))/n)
  }
  sigma2 <- var(amostra) 
  z <- qt((1+conf.level)/2, n-1) 
  xbarra <- mean(amostra)
  return(xbarra + c(-1,1)*z*sqrt(sigma2)/sqrt(n))
}


# letra b) i)
amostrasTam100n100 <- calculaProporcaoAmostral(100, 100)
amostrasTam100n2200 <- calculaProporcaoAmostral(100, 2200)

amostrasTam100n100
amostrasTam100n2200

# letra b) ii)
amostrasTam1000n100 <- calculaProporcaoAmostral(1000, 100)
amostrasTam1000n2200 <- calculaProporcaoAmostral(1000, 2200)

amostrasTam1000n100
amostrasTam1000n2200


proporcaoPopulacional



