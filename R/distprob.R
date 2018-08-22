#' Função para ajuste de distribuições de probabilidades e estimação de
#' parâmetros.
#'
#' @description Realiza os ajustes de probabilidade especificados pelo usuário e
#'   dada a probabilidade do evento, estima o valor da precipitação/vazão.
#'
#' @param valores Um vetor numérico com os valores a serem ajustados.
#' @param dist Um vetor de caracteres com os nomes das distribuições de
#'   probabilidade a serem utilizadas.
#' @param prob Um valor numérico, correspondente à probabilidade a qual o
#'   parâmetro (vazão ou precipitação) será estimado.
#'
#' @details No argumento \code{dist} podem ser utilizadas as seguintes
#'   distribuições:
#'
#'   \itemize{ \item Normal "\code{norm}";
#'
#'   \item Log-Normal "\code{lnorm}";
#'
#'   \item Gumbel para valores máximos "\code{gumbel}";
#'
#'   \item Weibull "\code{weibull}";
#'
#'   \item Gamma de 3 valores "\code{gamma3}" e
#'
#'   \item Log-Gamma de 3 valores "\code{lgamma3}". }
#'
#'   Para a utilização das distribuições \code{gumbel}, \code{gamma3} e
#'   \code{lgamma3} é necessário carregar o pacote \code{\link[FAdist]{FAdist}}.
#'
#'   A função retornará uma lista separada de acordo com as distribuições, os
#'   coeficientes encontrados do ajuste "\code{coefs}", o valor obtido do teste
#'   de Kolmogorov-Smirnov "\code{KS}", e o valor estimado para a dada
#'   probabilidade "\code{estimado}".
#' @export
#' @exportClass distprob.hidrowebr
#' @exportMethod  print.distprob.hidrowebr
distprob <- function(valores, dist, prob){
  for (i in 1:length(dist)){
    if(all(dist[i] != c("norm", "lnorm", "gumbel", "weibull", "gamma3", "lgamma3")))
      stop("Valores incorretos para o parâmetro dist")
  }

  if (any(dist == "gumbel") || any(dist == "gamma3") || any(dist == "lgamma3"))
    if("package:FAdist" %in% search() == FALSE)
      stop("Algumas distribuições escolhidas requerem o pacote FAdist carregado.")


  valores <- valores[!is.na(valores)]

  nomesDist <- c()
  distribuicao <- list()

  if(any(dist == "norm")){
    fit <- fitdistrplus::fitdist(valores, "norm")
    gof <- fitdistrplus::gofstat(fit, fitnames = "norm")
    estimado <- qnorm(prob, mean = coef(fit)[1],
                      sd = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("mean", "sd")
    KS <- as.numeric(gof$ks)
    norm <- list(coefs, KS, estimado)
    names(norm) <- c("coefs", "KS", "estimado")
    distribuicao[["norm"]] <- norm
    nomesDist <- c(nomesDist,"norm")
  }

  if(any(dist == "lnorm")){
    fit <- fitdistrplus::fitdist(valores, "lnorm")
    gof <- fitdistrplus::gofstat(fit, fitnames = "lnorm")
    estimado <- qlnorm(prob, meanlog = coef(fit)[1],
                      sdlog = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("meanlog", "sdlog")
    KS <- as.numeric(gof$ks)
    lnorm <- list(coefs, KS, estimado)
    names(lnorm) <- c("coefs", "KS", "estimado")
    distribuicao[["lnorm"]] <- lnorm
    nomesDist <- c(nomesDist,"lnorm")
  }

  if(any(dist == "gumbel")){
    fit <- fitdistrplus::fitdist(valores, "gumbel", start=list(scale=10,
                                                                 location=10))
    gof <- fitdistrplus::gofstat(fit, fitnames = "gumbel")
    estimado <- FAdist::qgumbel(prob, scale = coef(fit)[1],
                                location = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("scale", "location")
    KS <- as.numeric(gof$ks)
    gumbel <- list(coefs, KS, as.numeric(estimado))
    names(gumbel) <- c("coefs", "KS", "estimado")
    distribuicao[["gumbel"]] <- gumbel
    nomesDist <- c(nomesDist,"gumbel")
  }

  if(any(dist == "weibull")){
    fit <- fitdistrplus::fitdist(valores, "weibull")
    gof <- fitdistrplus::gofstat(fit, fitnames = "weibull")
    estimado <- qweibull(prob, shape = coef(fit)[1],
                         scale = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("shape", "scale")
    KS <- as.numeric(gof$ks)
    weibull <- list(coefs, KS, estimado)
    names(weibull) <- c("coefs", "KS", "estimado")
    distribuicao[["weibull"]] <- weibull
    nomesDist <- c(nomesDist,"weibull")
  }

  if(any(dist == "gamma3")){
    fit <- fitdistrplus::fitdist(valores, "gamma3",
                                 start=list(shape=10, scale=10))
    gof <- fitdistrplus::gofstat(fit, fitnames = "gamma3")
    estimado <- FAdist::qgamma3(prob, shape = coef(fit)[1],
                                scale = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("shape", "scale")
    KS <- as.numeric(gof$ks)
    gamma3 <- list(coefs, KS, as.numeric(estimado))
    names(gamma3) <- c("coefs", "KS", "estimado")
    distribuicao[["gamma3"]] <- gamma3
    nomesDist <- c(nomesDist, "gamma3")
  }

  if(any(dist == "lgamma3")){
    fit <- fitdistrplus::fitdist(valores, "lgamma3",
                                 start=list(shape=10, scale=10))
    gof <- fitdistrplus::gofstat(fit, fitnames = "lgamma3")
    estimado <- FAdist::qlgamma3(prob, shape = coef(fit)[1],
                                 scale = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("shape", "scale")
    KS <- as.numeric(gof$ks)
    lgamma3 <- list(coefs, KS, as.numeric(estimado))
    names(lgamma3) <- c("coefs", "KS", "estimado")
    distribuicao[["lgamma3"]] <- lgamma3
    nomesDist <- c(nomesDist, "lgamma3")
  }

  names(distribuicao) <- nomesDist
  class(distribuicao) <- c("distprob.hidrowebr", "distprob")
  return(distribuicao)
}

print.distprob.hidrowebr <- function(x, ...){
  if (!inherits(x, "distprob.hidrowebr"))
    stop("Use somente com objetos 'distprob.hidrowebr'.")
  numDist <- length(x)
  namesDist <- names(x)
  KS <- c()
  estimado <- c()
  for(i in 1:numDist){
    KS <- c(KS, x[[i]]$KS)
    estimado <- c(estimado, x[[i]]$estimado)
  }
  names(KS) <- namesDist
  names(estimado) <- namesDist
  cat("Teste Kolmogorov-Smirnov\n")
  print(KS)
  cat("\nValores estimados\n")
  print(estimado)
}
