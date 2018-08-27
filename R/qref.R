#' Função para cálculo de vazões de referência
#'
#' @description Calcula as vazões de referência obtida após a função de
#'   \code{\link{cperm}}.
#'
#' @param cperm Data frame obtida pela função de \code{cperm}.
#' @param ref Vetor numérico com as frequências para as vazões de referência.
#'
#' @details As vazões de referência são calculadas como a média do valor
#'   imediatamente superior e imediatamente inferior à frequência especificada.
#'
#' @export
qref <- function(cperm, ref){
  est <- unique(cperm$Est)
  qrefs <- data.frame()
  for (i in  1:length(est)){
    cperm_aux <- dplyr::filter(cperm, Est == est[i])
    for (j in 1:length(ref)){
      inferior <- min(dplyr::filter(cperm, Freq <= ref[j])$Q)
      superior <- max(dplyr::filter(cperm, Freq >= ref[j])$Q)
      aux <- data.frame(est[i], (superior + inferior)/2, ref[j])
      qrefs <- rbind(qrefs, aux)
    }
  }
  names(qrefs) <- c("Est", "Q", "Ref")
  return(qrefs)
}
