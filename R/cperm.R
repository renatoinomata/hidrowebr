#'Função para a construção da curva de permanência
#'
#'@description A função elabora a curva de permanência de um conjunto de dados.
#'
#'@return A função retornará uma data frame com  as colunas de \code{Est},
#'  \code{Data}, \code{Q}, \code{Ordem} e \code{Freq}. A coluna \code{Ordem}
#'  corresponde a ordem decrescente que o valor de vazão se encontra na série,
#'  enquanto que \code{Freq} corresponde ao seu valor de frequência acumulada.
#'
#'@param  dados Corresponde à data frame obtida após o uso da função
#'  \code{organizar}.
#'@param posPlot Variável do tipo \code{chr} que representa a fórmula de posição
#'  de plotagem a ser utilizada. Tem como padrão a fórmula de Weibull.
#'@param pad Variável do tipo \code{chr} que refere-se a padronização das
#'  vazões. Tem como padrão o valor \code{NULL}.
#'
#'@details Para a utilização desta função, deve-se utilizar os dados obtidos
#'  após a função de \code{organizar}, com as colunas de \code{Q}, \code{Data} e
#'  \code{Est}.
#'
#'  No argumento posição de plotagem, as opções para fórmulas de posição de
#'  plotagem são: \code{weibull}, \code{gringorten}, \code{blom}, \code{hazen} e
#'  \code{cunnane}.
#'
#'  No argumento \code{pad}, define-se o método para realizar a padronização das
#'  vazões, sendo as opções \code{media} ou \code{mediana}. Caso não seja
#'  necessário realizar essa padronização, a variável \code{pad} deve receber o
#'  valor \code{NULL}.
#'
#' @examples
#' # Cálculo da curva de permanência:
#' cperm(fluviopolis)
#'
#' # Curva de permanência padronizada (média):
#' cperm(fluviopolis, pad = "media")
#'
#' # Curva de permanência pela fórmula de gringorten:
#' cperm(fluviopolis, posPlot = "gringorten")
#'
#'@export
cperm <- function(dados, posPlot = "weibull", pad = NULL){
  if (posPlot != "weibull" & posPlot != "gringorten" & posPlot != "blom" &
      posPlot != "hazen" & posPlot != "cunnane"){
    warning("Fórrmula de posição de plotagem inválida. Utilizando a fórmula de
            Weibull.")
    posPlot <- "weibull"
  }
  if(is.null(pad) == FALSE){
    if(pad != "media" & pad != "mediana"){
      warning("Valor para 'pad' inválido. Não será realizado a
              padronização das vazões")
      pad <- NULL
    }
  }

  cPerm <- data.frame()
  for (i in unique(dados$Est)){
    aux <- dplyr::filter(dados, Est == i)
    aux <- dplyr::arrange(aux, desc(Q))
    Ordem <- 1:nrow(aux)
    aux <- cbind(aux, Ordem)
    # Cálculo da frequência
    if(posPlot == "weibull"){
      aux <- cbind(aux, 100 * aux[, "Ordem"] / (nrow(aux) + 1))
    }
    if(posPlot == "gringorten"){
      aux <- cbind(aux, 100 * (aux[, "Ordem"] - 0.44) / (nrow(aux) + 0.12))
    }
    if(posPlot == "blom"){
      aux <- cbind(aux, 100 * (aux[, "Ordem"] - 0.375) / (nrow(aux) + 0.25))
    }
    if(posPlot == "hazen"){
      aux <- cbind(aux, 100 * (aux[, "Ordem"] - 0.5) / (nrow(aux)))
    }
    if(posPlot == "cunnane"){
      aux <- cbind(aux, 100 * (aux[, "Ordem"] - 0.40) / (nrow(aux) + 0.20))
    }
    # Curva padronizada
    if(is.null(pad) == FALSE){
      if(pad == "media"){
        aux$Q <- aux$Q / mean(aux$Q) * 100
      }
      if(pad == "mediana"){
        aux$Q <- aux$Q / median(aux$Q) * 100
      }
    }

    names(aux) <- c("Est", "Data", "Q", "Ordem", "Freq")
    aux <- aux[c(1, 4, 2, 3, 5)]
    cPerm <- rbind(cPerm, aux)
  }
  return(cPerm)
}
