#' Resumo dos resultados de \code{distprob}
#'
#' @description Resume os resultados da função \code{\link{distprob}},
#'   retornando apenas a melhor distribuição (aquela com menor valor no teste de
#'   Kolmogorov-Smirnov) e o valor estimado.
#'
#' @param distProb Data frame obtida pela função \code{\link{distprob}}.
#'
#' @examples
#' # Cálculo de vazões máximas:
#' qmax <- maxAnuais(fluviopolis, "Q")
#'
#' # Estimativa da vazão máxima para o período de retorno de 10 anos com a
#' # distribuição log-normal:
#' distProb <- distprob(qmax, "Maxima", dist = c("norm", "lnorm"), prob = 0.1)
#'
#' #Resumo
#' resumoDist(distProb)
#'
#' @export
resumoDist <- function(distProb){
  distProb <- dplyr::summarise(dplyr::group_by(distProb, Est),
                               Dist = Dist[which.min(KS)],
                               Estimado = Estimado[which.min(KS)])
  distProb$Estimado <- round(distProb$Estimado, 2)
  return(as.data.frame(distProb))
}
