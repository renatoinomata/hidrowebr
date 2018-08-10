cperm <- function(dados, posPlot = "weibull", pad = NULL){
  if (posPlot != "weibull" & posPlot != "gringorten" & posPlot != "blom" &
      posPlot != "hazen" & posPlot != "cunnane"){
    warning("Fórmula de posição de plotagem inválida. Utilizando a fórmula de
            Weibull.")
    posPlot <- "weibull"
  }
  if(is.null(pad) == FALSE){
    if(pad != "media" & pad != "mediana"){
      warning("Valor para 'pad' inválido. Não será realizado a
              padronização das vazÃµes")
      pad <- NULL
    }
  }

  cPerm <- data.frame()
  for (i in unique(dados$Estacao)){
    aux <- dplyr::filter(dados, Estacao == i)
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

    names(aux) <- c("Estacao", "Data", "Q", "Ordem", "Freq")
    aux <- aux[c(1, 4, 2, 3, 5)]
    cPerm <- rbind(cPerm, aux)
  }
  return(cPerm)
}
