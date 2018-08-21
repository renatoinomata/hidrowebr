#' Funções para determinar os valores máximos, mínimos ou médios anuais.
#'
#' @param valores Vetor numérico com os valores de vazão ou precipitação.
#' @param datas Vetor de datas correspondentes aos dados.
#' @param estacoes Vetor com as correspondentes estações de cada um dos
#'   elementos dos vetores anteriores.
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
maxAnuais <- function(valores, datas, estacoes, ano_hidro = "01-01"){
  if (is.na(as.Date(paste0(ano_hidro, "-", 2000), "%d-%m-%Y"))){
    warning("Data para ano hidrológico inválida.")
    ano_hidro <- "01-01"
  }
  dados <- data.frame(estacoes, datas, valores)
  colnames(dados) <- c("Estacao", "Data", "Valores")
  anuais <- data.frame()
  for(i in unique(dados$Estacao)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Estacao == i)
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
        max_ano <- rbind(max_ano, max(auxAno$Valores))
      } else {
        max_ano <- rbind(max_ano, NA)
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, max_ano))
  }
  names(anuais) <- c("Estacao", "Periodo", "Maxima")
  return(anuais)
}

minAnuais <- function(valores, datas, estacoes, ano_hidro = "01-01"){
  if (is.na(as.Date(paste0(ano_hidro, "-", 2000), "%d-%m-%Y"))){
    warning("Data para ano hidrológico inválida.")
    ano_hidro <- "01-01"
  }
  dados <- data.frame(estacoes, datas, valores)
  colnames(dados) <- c("Estacao", "Data", "Valores")
  anuais <- data.frame()
  for(i in unique(dados$Estacao)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Estacao == i)
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
        min_ano <- rbind(min_ano, min(auxAno$Valores))
      } else {
        min_ano <- rbind(min_ano, NA)
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, min_ano))
  }
  names(anuais) <- c("Estacao", "Periodo", "Minima")
  return(anuais)
}

medAnuais <- function(valores, datas, estacoes, ano_hidro = "01-01"){
  if (is.na(as.Date(paste0(ano_hidro, "-", 2000), "%d-%m-%Y"))){
    warning("Data para ano hidrológico inválida.")
    ano_hidro <- "01-01"
  }
  dados <- data.frame(estacoes, datas, valores)
  colnames(dados) <- c("Estacao", "Data", "Valores")
  anuais <- data.frame()
  for(i in unique(dados$Estacao)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Estacao == i)
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
        med_ano <- rbind(med_ano, mean(auxAno$Valores))
      } else {
        med_ano <- rbind(med_ano, NA)
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, med_ano))
  }
  names(anuais) <- c("Estacao", "Periodo", "Media")
  return(anuais)
}
