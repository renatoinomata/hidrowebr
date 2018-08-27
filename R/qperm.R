#' Função para cálculo de vazões de permanência
#'
#' @description Calcula as vazões de permanência obtida após a função de
#'   \code{\link{cperm}}.
#'
#' @param cperm Data frame obtida pela função de \code{cperm}.
#' @param ref Vetor numérico com as frequências para as vazões de permanência.
#'
#' @details As vazões de referência são calculadas como a média do valor
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
qperm <- function(cperm, ref){
  est <- unique(cperm$Est)
  qperms <- data.frame()
  for (i in  1:length(est)){
    cperm_aux <- dplyr::filter(cperm, Est == est[i])
    for (j in 1:length(ref)){
      inferior <- min(dplyr::filter(cperm, Freq <= ref[j])$Q)
      superior <- max(dplyr::filter(cperm, Freq >= ref[j])$Q)
      aux <- data.frame(est[i], (superior + inferior)/2, ref[j])
      qperms <- rbind(qperms, aux)
    }
  }
  names(qperms) <- c("Est", "Q", "Ref")
  return(qperms)
}
