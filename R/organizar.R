#' Função de organização dos dados
#'
#' @description Organiza os dados em colunas de Estação, Data e
#'   Vazão/Precipitação.
#'
#' @param dados Os dados brutos obtidos após a função de \code{\link{importar}}.
#' @param formato_data Formatação das datas no arquivo.
#'
#' @details Os dados brutos são organizados em uma data frame com: colunas de
#'   \code{Estacao}, que representa o código da estação; \code{Data}, que é a
#'   data da leitura; e \code{Q} ou \code{P}, valores de vazão (m³/s) ou
#'   precipitação (mm), respectivamente.
#'
#'   O valor padrão para a variável \code{formato_data} é %d/%m/%Y, ou seja,
#'   dd/mm/AAAA. Para mais informações consulte a classe
#'   \code{\link[base]{Dates}}.
#'
#' @export
organizar <- function(dados, formato_data = "%d/%m/%Y"){
  # Limpando os dados
  if(grepl("Vazao", names(dados)[17]))
    valor <- "Q"
  else
    valor <- "P"
  dados <- dados[c(1, 3, 17:47)]
  names(dados) <- c("Estacao", "Data", 1:31)
  dados <- tidyr::separate(dados, Data, into = c("Ano", "Mes", "Dia"))
  dados["Dia"] <- NULL

  # Organizando
  if(valor == "Q")
    dados <- tidyr::gather(dados, "Dia", "Q", 4:34)
  else
    dados <- tidyr::gather(dados, "Dia", "P", 4:34)
  dados <- dplyr::arrange(dados, Ano, Mes)
  dados <- tidyr::unite(dados, Dia, Mes, Ano, col = "Data", sep = "/")
  if(valor == "Q")
    dados <- dplyr::filter(dados, !is.na(Q))
  else
    dados <- dplyr::filter(dados, !is.na(P))
  dados$Data <- as.Date(dados$Data, format = formato_data)
  if(length(unique(dados$Estacao)) > 1){
    dados_bkp <- dados
    dados <- NULL
    for(i in unique(dados_bkp$Estacao)){
      dados_aux <- dplyr::filter(dados_bkp, Estacao == i)
      dados_aux <- dados_aux[!duplicated(dados_aux$Data), ]
      dados <- rbind(dados, dados_aux)
    }
  }
  dados$Estacao <- as.factor(dados$Estacao)
  return(dados)
}
