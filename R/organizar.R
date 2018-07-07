organizar <- function(dados){
  # Limpando os dados
  dados <- dados[c(1, 3, 17:47)]
  names(dados) <- c("Estacao", "Data", 1:31)
  dados <- tidyr::separate(dados, Data, into = c("Ano", "Mes", "Dia"))
  dados["Dia"] <- NULL

  # Organizando
  dados <- tidyr::gather(dados, "Dia", "Q", 4:34)
  dados <- dplyr::arrange(dados, Ano, Mes)
  dados <- tidyr::unite(dados, Dia, Mes, Ano, col = "Data", sep = "/")
  dados <- dplyr::filter(dados, !is.na(Q))
  dados$Data <- as.Date(dados$Data, "%d/%m/%Y")
  if(length(unique(dados$Estacao)) > 1){
    dados_bkp <- dados
    dados <- NULL
    for(i in unique(dados_bkp$Estacao)){
      dados_aux <- dplyr::filter(dados_bkp, Estacao == i)
      dados_aux <- dados_aux[!duplicated(dados_aux$Data), ]
      dados <- rbind(dados, dados_aux)
    }
  }
  return(dados)
}
