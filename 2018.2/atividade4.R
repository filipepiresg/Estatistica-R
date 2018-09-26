# 1
calculaEstimador = function(n) {
  media <- c()
  mediana <- c()
  for(i in 1:10000) {
    media[i] <- mean(rnorm(n, 500, sqrt(10)))
    mediana[i] <- median(rnorm(n, 500, sqrt(10)))
  }
  media_medias <- mean(media)
  var_medias <- var(media)
  media_medianas <- mean(mediana)
  var_medianas <- var(mediana)
  
  retorno <- data.frame(media_medias, var_medias, media_medianas, var_medianas)
}

letra_a <- calculaEstimador(10)
letra_b <- calculaEstimador(20)
letra_c <- calculaEstimador(30)
letra_d <- calculaEstimador(50)
letra_e <- calculaEstimador(100)

letra_a
letra_b
letra_c
letra_d
letra_e

# 2

calculaEstimadorU <- function(n) {
  amostra <- c()
  for(i in 1: 10000) {
    amostra[i] <- runif(n,0,500 )
  }
  estimador1 <- 2*mean(amostra)
  estimador2 <- ((n+1)/n)*max(amostra)
  
  return (data.frame(estimador1, estimador2))
}

teta <- calculaEstimadorU(100)

teta