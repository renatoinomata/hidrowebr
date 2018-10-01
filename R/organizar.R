#' Função de organização dos dados
#'
#' @description Organiza os dados em colunas de Estação, Data e
#'   Vazão/Precipitação.
#'
#' @param dados Os dados brutos obtidos após a função de \code{\link{importar}}.
#' @param formato_data Formatação das datas no arquivo.
#'
#' @details Os dados brutos são organizados em uma data frame com: colunas de
#'   \code{Est}, que representa o código da estação; \code{Data}, que é a data
#'   da leitura; e \code{Q} ou \code{P}, valores de vazão (m³/s) ou precipitação
#'   (mm), respectivamente.
#'
#'   O valor padrão para a variável \code{formato_data} é \code{Dia}, \code{Mes}
#'   e \code{Ano}. Deve-se ordernar esses três componentes conforme a formatação
#'   que estiver presente no arquivo do portal Hidroweb. A partir da versão
#'   2.0.0, o Hidroweb tem como padrão as datas na forma dd/mm/YYYY.
#'
#'   A função também exclui os valores repetidos. A preferência é para aqueles
#'   que possuem nível de consistência 2 (nível consistido).
#'
#' @export
organizar <- function(dados, formato_data =  c("Dia", "Mes", "Ano")){
  # Limpando os dados
  if(grepl("Vazao", names(dados)[17])){
    valor <- "Q"
  } else {
    valor <- "P"
  }
  dados <- dplyr::arrange(dados, desc(NivelConsistencia))
  dados <- dados[c(1, 3, 17:47)]
  names(dados) <- c("Est", "Data", 1:31)
  dados <- tidyr::separate(dados, Data, into = formato_data)
  dados["Dia"] <- NULL

  # Organizando
  if(valor == "Q")
    dados <- tidyr::gather(dados, "Dia", "Q", 4:34)
  else
    dados <- tidyr::gather(dados, "Dia", "P", 4:34)
  dados <- dplyr::arrange(dados, Ano, Mes)
  dados <- tidyr::unite(dados, Dia, Mes, Ano, col = "Data", sep = "/")
  if(valor == "Q"){
    dados <- dplyr::filter(dados, !is.na(Q))
  } else{
    dados <- dplyr::filter(dados, !is.na(P))
  }
  dados$Data <- as.Date(dados$Data, format = "%d/%m/%Y")
  if(length(unique(dados$Est)) > 1){
    dados_bkp <- dados
    dados <- NULL
    for(i in unique(dados_bkp$Est)){
      dados_aux <- dplyr::filter(dados_bkp, Est == i)
      dados_aux <- dados_aux[!duplicated(dados_aux$Data), ]
      dados <- rbind(dados, dados_aux)
    }
  }
  dados$Est <- as.factor(dados$Est)
  return(dados)
}
