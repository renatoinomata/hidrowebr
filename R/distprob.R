#' Função para ajuste de distribuições de probabilidades e estimação de
#' parâmetros
#'
#' @description Realiza os ajustes de probabilidade especificados pelo usuário e
#'   dada a probabilidade do evento, estima o valor da precipitação/vazão.
#'
#' @param dados Data frame com a série hidrológica a ser utilizada.
#' @param col_valores O nome da coluna com os valores a serem utilizados.
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
#' @details Para que as distribuições sejam ajustadas é necessário que existam
#'   pelo menos 10 observações por estação.
#'
#' @return A função retornará uma data frame com as seguintes colunas:
#'   \code{Est}, o código da estação; \code{Dist} a distribuição utilizada;
#'   \code{Coef1} e \code{Coef2}, os parâmetros da distribuição; \code{KS} o
#'   valor obtido do teste de Kolmogorov-Smirnov; e \code{Estimado}, o valor
#'   estimado para a dada probabilidade.
#'
#' @examples
#' # Cálculo de vazões máximas:
#' qmax <- maxAnuais(fluviopolis, "Q")
#'
#' # Estimativa da vazão máxima para o período de retorno de 10 anos com a
#' # distribuição log-normal:
#' distprob(dados = qmax, col_valores = "Maxima", dist = "lnorm", prob = 0.1)
#'
#' @export
distprob <- function(dados, col_valores, dist, prob){
  for (i in 1:length(dist)){
    if(all(dist[i] != c("norm", "lnorm", "gumbel", "weibull", "gamma3", "lgamma3")))
      stop("Valores incorretos para o parâmetro dist")
  }

  if (any(dist == "gumbel") || any(dist == "gamma3") || any(dist == "lgamma3"))
    if("package:FAdist" %in% search() == FALSE)
      stop("Algumas distribuições escolhidas requerem o pacote FAdist carregado.")

  distribuicao <- data.frame()
  for (i in unique(dados$Est)){
    valores <- dplyr::filter(dados, Est == i)[col_valores]
    valores <- valores[!is.na(valores)]
    if(length(valores) > 10){
      nomesDist <- c()

      if(any(dist == "norm")){
        fit <- try(fitdistrplus::fitdist(valores, "norm"))
        if(!assertthat::is.error(fit)){
          gof <- fitdistrplus::gofstat(fit, fitnames = "norm")
          estimado <- qnorm(prob, mean = coef(fit)[1],
                            sd = coef(fit)[2])
          norm <- data.frame(i, "norm", as.numeric(coef(fit)[1]),
                             as.numeric(coef(fit)[2]), as.numeric(gof$ks),
                             as.numeric(estimado))
        } else {
          norm <- data.frame(i, "norm", NA, NA, NA, NA)
        }
        names(norm) <- c("Est", "Dist", "Coef1", "Coef2", "KS", "Estimado")
        distribuicao <- rbind(distribuicao, norm)
      }

      if(any(dist == "lnorm")){
        fit <- try(fitdistrplus::fitdist(valores, "lnorm"))
        if(!assertthat::is.error(fit)){
          gof <- fitdistrplus::gofstat(fit, fitnames = "lnorm")
          estimado <- qlnorm(prob, meanlog = coef(fit)[1],
                             sdlog = coef(fit)[2])
          lnorm <- data.frame(i, "lnorm", as.numeric(coef(fit)[1]),
                              as.numeric(coef(fit)[2]), as.numeric(gof$ks),
                              as.numeric(estimado))
        } else {
          lnorm <- data.frame(i, "lnorm", NA, NA, NA, NA)
        }
        names(lnorm) <- c("Est", "Dist", "Coef1", "Coef2", "KS", "Estimado")
        distribuicao <- rbind(distribuicao, lnorm)
      }

      if(any(dist == "gumbel")){
        fit <- try(fitdistrplus::fitdist(valores, "gumbel",
                                         start=list(scale=10, location=10)))
        if(!assertthat::is.error(fit)){
          gof <- fitdistrplus::gofstat(fit, fitnames = "gumbel")
          estimado <- FAdist::qgumbel(prob, scale = coef(fit)[1],
                                      location = coef(fit)[2])
          gumbel <- data.frame(i, "gumbel", as.numeric(coef(fit)[1]),
                               as.numeric(coef(fit)[2]), as.numeric(gof$ks),
                               as.numeric(estimado))
        } else {
          gumbel <- data.frame(i, "gumbel", NA, NA, NA, NA)
        }
        names(gumbel) <- c("Est", "Dist", "Coef1", "Coef2", "KS", "Estimado")
        distribuicao <- rbind(distribuicao, gumbel)
      }

      if(any(dist == "weibull")){
        fit <- try(fitdistrplus::fitdist(valores, "weibull"))
        if(!assertthat::is.error(fit)){
          gof <- fitdistrplus::gofstat(fit, fitnames = "weibull")
          estimado <- qweibull(prob, shape = coef(fit)[1],
                               scale = coef(fit)[2])
          weibull <- data.frame(i, "weibull", as.numeric(coef(fit)[1]),
                                as.numeric(coef(fit)[2]), as.numeric(gof$ks),
                                as.numeric(estimado))
        } else {
          weibull <- data.frame(i, "weibull", NA, NA, NA, NA)
        }
        names(weibull) <- c("Est", "Dist", "Coef1", "Coef2", "KS", "Estimado")
        distribuicao <- rbind(distribuicao, weibull)
      }

      if(any(dist == "gamma3")){
        fit <- try(fitdistrplus::fitdist(valores, "gamma3",
                                         start=list(shape=10, scale=10)))
        if(!assertthat::is.error(fit)){
          gof <- fitdistrplus::gofstat(fit, fitnames = "gamma3")
          estimado <- FAdist::qgamma3(prob, shape = coef(fit)[1],
                                      scale = coef(fit)[2])
          gamma3 <- data.frame(i, "gamma3", as.numeric(coef(fit)[1]),
                               as.numeric(coef(fit)[2]), as.numeric(gof$ks),
                               as.numeric(estimado))
        } else {
          gamma3 <- data.frame(i, "gamma3", NA, NA, NA, NA)
        }
        names(gamma3) <- c("Est", "Dist", "Coef1", "Coef2", "KS", "Estimado")
        distribuicao <- rbind(distribuicao, gamma3)
      }

      if(any(dist == "lgamma3")){
        fit <- try(fitdistrplus::fitdist(valores, "lgamma3",
                                         start=list(shape=10, scale=10)))
        if(!assertthat::is.error(fit)){
          gof <- fitdistrplus::gofstat(fit, fitnames = "lgamma3")
          estimado <- FAdist::qlgamma3(prob, shape = coef(fit)[1],
                                       scale = coef(fit)[2])
          lgamma3 <- data.frame(i, "lgamma3", as.numeric(coef(fit)[1]),
                                as.numeric(coef(fit)[2]), as.numeric(gof$ks),
                                as.numeric(estimado))
        } else {
          lgamma3 <- data.frame(i, "lgamma3", NA, NA, NA, NA)
        }
        names(lgamma3) <- c("Est", "Dist", "Coef1", "Coef2", "KS", "Estimado")
        distribuicao <- rbind(distribuicao, lgamma3)
      }
    } else {
      noDist <- data.frame(i, "número de observações insuficientes", NA, NA, NA, NA)
      names(noDist) <- c("Est", "Dist", "Coef1", "Coef2", "KS", "Estimado")
      distribuicao <- rbind(distribuicao, noDist)
    }
  }
  return(distribuicao)
}
