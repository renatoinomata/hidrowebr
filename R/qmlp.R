#' Vazão média de longo período
#'
#' @description Calcula a vazão média de longo período (longa permanência) por
#'   estação.
#'
#' @param dados Data frame com a série hidrológica a ser utilizada.
#' @param col_valores O nome da coluna com os valores a serem utilizados.
#'
#' @examples # Cálculo da vazão média de longa permanência de fluviópolis:
#' qmlp(fluviopolis, col_valores = "Q")
#'
#' @export
qmlp <- function(dados, col_valores){
  Qmlp <- dplyr::group_by(dados, Est)
  Qmlp <- dplyr::summarise(Qmlp, Qmlp = round(mean(Q), 2))
  Qmlp <- data.frame(Qmlp)
  return(Qmlp)
}
