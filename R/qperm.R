#' Função para cálculo de vazões de permanência
#'
#' @description Calcula as vazões de permanência obtida após a função de
#'   \code{\link{cperm}}.
#'
#' @param cperm Data frame obtida pela função de \code{cperm}.
#' @param freq Vetor numérico com as frequências para as vazões de permanência.
#'
#' @details As vazões de permanência são calculadas como a média do valor
#'   imediatamente superior e imediatamente inferior à frequência especificada.
#'
#' @examples
#' # Cálculo da curva de permanência:
#' cPerm <- cperm(fluviopolis)
#'
#' # Cálculo das vazões de permanência:
#' qperm(cPerm, 95)
#'
#' @export
qperm <- function(cperm, freq){
  est <- unique(cperm$Est)
  qperms <- data.frame(est)
  for (i in freq){
    aux <- c()
    for (j in est){
      inferior <- min(dplyr::filter(dplyr::filter(cperm, Est == j),
                                    Freq <= i)$Q)
      superior <- max(dplyr::filter(dplyr::filter(cperm, Est == j),
                                    Freq >= i)$Q)
      aux <- c(aux, round((superior + inferior)/2, 2))
    }
    qperms <- data.frame(qperms, aux)
  }
  names(qperms) <- c("Est", paste0("Q", freq))
  return(qperms)
}
# qperm <- function(cperm, freq){
#   est <- unique(cperm$Est)
#   qperms <- data.frame()
#   for (i in  1:length(est)){
#     cperm_aux <- dplyr::filter(cperm, Est == est[i])
#     for (j in 1:length(freq)){
#       inferior <- min(dplyr::filter(dplyr::filter(cperm, Est == est[i]),
#                                     Freq <= freq[j])$Q)
#       superior <- max(dplyr::filter(dplyr::filter(cperm, Est == est[i]),
#                                     Freq >= freq[j])$Q)
#       aux <- data.frame(est[i], (superior + inferior)/2, freq[j])
#       qperms <- rbind(qperms, aux)
#     }
#   }
#   names(qperms) <- c("Est", "Q", "Freq")
#   qperms$Q <- round(qperms$Q, 2)
#   return(qperms)
# }


