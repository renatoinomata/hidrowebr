#' Função de vazões médias em dias consecutivos.
#'
#' @param dados Data frame obtida após o uso da função \code{\link{organizar}}
#' @param x Número de dias consecutivos. O valor padrão é 7, correspondendo a
#'   série de vazões Q7.
#'
#' @details A função retorna uma data frame com as colunas de \code{Estacao},
#'   \code{Data} e \code{Qx}. As médias só serão realizadas caso existam
#'   \code{x} dias consecutivos.
#'
#' @export
qx <- function(dados, x = 7){
  est <- unique(dados$Estacao)
  dadosQx <- data.frame()
  for (i in est){
    auxEst <- dplyr::filter(dados, Estacao == i)
    periodo <- seq(min(auxEst$Data), max(auxEst$Data), by = "days")
    aux <- data.frame(i, periodo)
    aux$Q <- NA
    colnames(aux) <- c("Estacao", "Data", "Q")
    aux <- rbind(auxEst, aux)
    aux <- aux[!duplicated(aux$Data), ]
    aux <- dplyr::arrange(aux, Data)
    aux$Qx <- zoo::rollapply(aux$Q, x, mean,  fill = NA, align = "right")
    aux$Q <- NULL
    aux <- aux[!is.na(aux$Qx), ]
    dadosQx <- rbind(dadosQx, aux)
  }
  return(dadosQx)
}
