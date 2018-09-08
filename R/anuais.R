#' Funções para determinar os valores máximos, mínimos ou médios anuais
#'
#' @description Determina os valores máximos, mínimos ou médios de cada período
#'   e estação. O período pode ser tanto ano civil quanto ano hidrológico
#'   definido pelo usuário.
#'
#' @param dados Data frame com a série hidrológica a ser utilizada.
#' @param col_valores O nome da coluna com os valores a serem utilizados.
#' @param ano_hidro Variável do tipo \code{chr} que contém o dia e mês a ser
#'   utilizado como início do ano hidrológico. Tem como padrão o dia primeiro de
#'   janeiro ("01-01").
#'
#' @details No argumento \code{ano_hidro} deve-se repassar uma variável do tipo
#'   \code{chr} com o dia e o mês a ser utilizado para iniciar o ano
#'   hidrológico. Dessa maneira, a data ("15-02") corresponderia ao dia 15 de
#'   fevereiro, por exemplo. Caso o valor para o ano hidrológico seja uma data
#'   inválida, a função utilizará o dia "01-01" para determinar os valores
#'   anuais.
#'
#' @examples # Cálculo das vazões máximas de fluviópolis:
#' maxAnuais(fluviopolis, col_valores = "Q")
#'
#' # Especificando data para começo do ano hidrológico (26 de junho):
#' maxAnuais(fluviopolis, col_valores = "Q", ano_hidro = "26-06")
#'
#' @export
maxAnuais <- function(dados, col_valores, ano_hidro = "01-01"){
  if (is.na(as.Date(paste0(ano_hidro, "-", 2000), "%d-%m-%Y"))){
    warning("Data para ano hidrológico inválida.")
    ano_hidro <- "01-01"
  }
  # Conferindo valores para col_valores
  if (is.null(col_valores))
    stop("Inserir argumento 'col_valores'.")

  if (!is.character(col_valores))
    stop("Inserir variável do tipo 'chr'.")

  if (col_valores == "Data" || col_valores == "Est")
    stop("Variável incorreta para 'col_valores'.")

  if (!any(colnames(dados) == col_valores))
    stop("Não há colunas com valores iguais a 'col_valores'.")

  num_col_valores <- which(colnames(dados) == col_valores)
  anuais <- data.frame()
  for(i in unique(dados$Est)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Est == i)
    anos <- unique(lubridate::year(auxEst$Data))
    datasAux <- as.Date(paste0(ano_hidro,"-",anos), "%d-%m-%Y")
    datasAux <- data.frame(datasAux, datasAux + lubridate::years(1) - 1)
    colnames(datasAux) <- c("inicio", "fim")
    # Criando os valores máximos e períodos
    max_ano <- c()
    periodo <- c()
    for(j in 1:nrow(datasAux)){
      auxAno <- dplyr::filter(auxEst, Data >= datasAux$inicio[j] &
                                Data <= datasAux$fim[j])
      periodo <- rbind(periodo,
                       ifelse(ano_hidro != "01-01",
                              paste0(lubridate::year(datasAux$inicio[j]),
                                     "-",
                                     lubridate::year(datasAux$fim[j])),
                              lubridate::year(datasAux$inicio[j])))
      if (nrow(auxAno) != 0){
        max_ano <- rbind(max_ano, max(auxAno[,num_col_valores]))
      } else {
        max_ano <- rbind(max_ano, NA)
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, max_ano))
  }
  names(anuais) <- c("Est", "Periodo", "Maxima")
  anuais <- anuais[!is.na(anuais$Maxima), ]
  return(anuais)
}

#' @describeIn maxAnuais Função para determinar os valores mínimos anuais.
#'
#' @export
minAnuais <- function(dados, col_valores, ano_hidro = "01-01"){
  if (is.na(as.Date(paste0(ano_hidro, "-", 2000), "%d-%m-%Y"))){
    warning("Data para ano hidrológico inválida.")
    ano_hidro <- "01-01"
  }
  # Conferindo valores para col_valores
  if (is.null(col_valores))
    stop("Inserir argumento 'col_valores'.")

  if (!is.character(col_valores))
    stop("Inserir variável do tipo 'chr'.")

  if (col_valores == "Data" || col_valores == "Est")
    stop("Variável incorreta para 'col_valores'.")

  if (!any(colnames(dados) == col_valores))
    stop("Não há colunas com valores iguais a 'col_valores'.")

  num_col_valores <- which(colnames(dados) == col_valores)
  anuais <- data.frame()
  for(i in unique(dados$Est)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Est == i)
    anos <- unique(lubridate::year(auxEst$Data))
    datasAux <- as.Date(paste0(ano_hidro,"-",anos), "%d-%m-%Y")
    datasAux <- data.frame(datasAux, datasAux + lubridate::years(1) - 1)
    colnames(datasAux) <- c("inicio", "fim")
    # Criando os valores máximos e períodos
    min_ano <- c()
    periodo <- c()
    for(j in 1:nrow(datasAux)){
      auxAno <- dplyr::filter(auxEst, Data >= datasAux$inicio[j] &
                                Data <= datasAux$fim[j])
      periodo <- rbind(periodo,
                       ifelse(ano_hidro != "01-01",
                              paste0(lubridate::year(datasAux$inicio[j]),
                                     "-",
                                     lubridate::year(datasAux$fim[j])),
                              lubridate::year(datasAux$inicio[j])))
      if (nrow(auxAno) != 0){
        min_ano <- rbind(min_ano, min(auxAno[,num_col_valores]))
      } else {
        min_ano <- rbind(min_ano, NA)
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, min_ano))
  }
  names(anuais) <- c("Est", "Periodo", "Minima")
  anuais <- anuais[!is.na(anuais$Minima), ]
  return(anuais)
}
#' @describeIn maxAnuais Função para determinar os valores médios anuais.
#'
#' @export
medAnuais <- function(dados, col_valores, ano_hidro = "01-01"){
  if (is.na(as.Date(paste0(ano_hidro, "-", 2000), "%d-%m-%Y"))){
    warning("Data para ano hidrológico inválida.")
    ano_hidro <- "01-01"
  }
  # Conferindo valores para col_valores
  if (is.null(col_valores))
    stop("Inserir argumento 'col_valores'.")

  if (!is.character(col_valores))
    stop("Inserir variável do tipo 'chr'.")

  if (col_valores == "Data" || col_valores == "Est")
    stop("Variável incorreta para 'col_valores'.")

  if (!any(colnames(dados) == col_valores))
    stop("Não há colunas com valores iguais a 'col_valores'.")

  num_col_valores <- which(colnames(dados) == col_valores)
  anuais <- data.frame()
  for(i in unique(dados$Est)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Est == i)
    anos <- unique(lubridate::year(auxEst$Data))
    datasAux <- as.Date(paste0(ano_hidro,"-",anos), "%d-%m-%Y")
    datasAux <- data.frame(datasAux, datasAux + lubridate::years(1) - 1)
    colnames(datasAux) <- c("inicio", "fim")
    # Criando os valores máximos e períodos
    med_ano <- c()
    periodo <- c()
    for(j in 1:nrow(datasAux)){
      auxAno <- dplyr::filter(auxEst, Data >= datasAux$inicio[j] &
                                Data <= datasAux$fim[j])
      periodo <- rbind(periodo,
                       ifelse(ano_hidro != "01-01",
                              paste0(lubridate::year(datasAux$inicio[j]),
                                     "-",
                                     lubridate::year(datasAux$fim[j])),
                              lubridate::year(datasAux$inicio[j])))
      if (nrow(auxAno) != 0){
        med_ano <- rbind(med_ano, mean(auxAno[,num_col_valores]))
      } else {
        med_ano <- rbind(med_ano, NA)
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, med_ano))
  }
  names(anuais) <- c("Est", "Periodo", "Media")
  anuais <- anuais[!is.na(anuais$Media), ]
  return(anuais)
}
