distprob <- function(valores, dist, prob){
  if(all(dist != c("norm", "lnorm", "gumbel",
                   "weibull", "pearson3", "logpear3"))){
    stop("Valores incorretos para o parâmetro dist")
  }

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
    dgumbel <- FAdist::dgumbel
    pgumbel <- FAdist::pgumbel
    qgumbel <- FAdist::qgumbel
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

  if(any(dist == "pearson3")){
    dpearson3 <- FAdist::dgamma3
    ppearson3 <- FAdist::pgamma3
    qpearson3 <- FAdist::qgamma3
    fit <- fitdistrplus::fitdist(valores, "pearson3",
                                 start=list(shape=10, scale=10))
    gof <- fitdistrplus::gofstat(fit, fitnames = "pearson3")
    estimado <- FAdist::qgamma3(prob, shape = coef(fit)[1],
                                scale = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("shape", "scale")
    KS <- as.numeric(gof$ks)
    pearson3 <- list(coefs, KS, as.numeric(estimado))
    names(pearson3) <- c("coefs", "KS", "estimado")
    distribuicao[["pearson3"]] <- pearson3
    nomesDist <- c(nomesDist, "pearson3")
  }

  if(any(dist == "logpear3")){
    dlogpear3 <- FAdist::dlgamma3
    plogpear3 <- FAdist::plgamma3
    qlogpear3 <- FAdist::qlgamma3
    fit <- fitdistrplus::fitdist(valores, "logpear3",
                                 start=list(shape=10, scale=10))
    gof <- fitdistrplus::gofstat(fit, fitnames = "logpear3")
    estimado <- FAdist::qlgamma3(prob, shape = coef(fit)[1],
                                scale = coef(fit)[2])
    coefs <- list(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    names(coefs) <- c("shape", "scale")
    KS <- as.numeric(gof$ks)
    logpear3 <- list(coefs, KS, as.numeric(estimado))
    names(logpear3) <- c("coefs", "KS", "estimado")
    distribuicao[["logpear3"]] <- logpear3
    nomesDist <- c(nomesDist, "logpear3")
  }

  names(distribuicao) <- nomesDist
  return(distribuicao)
}
