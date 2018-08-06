maxAnuais <- function(valores, datas, estacoes, ano_hidro = "01-01"){
  dados <- data.frame(estacoes, datas, valores)
  colnames(dados) <- c("Estacao", "Data", "Valores")
  anuais <- data.frame()
  for(i in unique(dados$Estacao)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Estacao == i)
    anos <- unique(lubridate::year(auxEst$Data))
    datas <- as.Date(paste0(ano_hidro,"-",anos), "%d-%m-%Y")
    datas <- data.frame(datas, datas + lubridate::years(1) - 1)
    colnames(datas) <- c("inicio", "fim")
    # Criando os valores máximos e períodos
    max_ano <- c()
    periodo <- c()
    for(j in 1:nrow(datas)){
      auxAno <- dplyr::filter(auxEst, Data >= datas$inicio[j] &
                                Data <= datas$fim[j])
      periodo <- rbind(periodo,
                       ifelse(ano_hidro != "01-01",
                              paste0(lubridate::year(datas$inicio[j]),
                                     "-",
                                     lubridate::year(datas$fim[j])),
                              lubridate::year(datas$inicio[j])))
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
  dados <- data.frame(estacoes, datas, valores)
  colnames(dados) <- c("Estacao", "Data", "Valores")
  anuais <- data.frame()
  for(i in unique(dados$Estacao)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Estacao == i)
    anos <- unique(lubridate::year(auxEst$Data))
    datas <- as.Date(paste0(ano_hidro,"-",anos), "%d-%m-%Y")
    datas <- data.frame(datas, datas + lubridate::years(1) - 1)
    colnames(datas) <- c("inicio", "fim")
    # Criando os valores máximos e períodos
    min_ano <- c()
    periodo <- c()
    for(j in 1:nrow(datas)){
      auxAno <- dplyr::filter(auxEst, Data >= datas$inicio[j] &
                                Data <= datas$fim[j])
      periodo <- rbind(periodo,
                       ifelse(ano_hidro != "01-01",
                              paste0(lubridate::year(datas$inicio[j]),
                                     "-",
                                     lubridate::year(datas$fim[j])),
                              lubridate::year(datas$inicio[j])))
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
  dados <- data.frame(estacoes, datas, valores)
  colnames(dados) <- c("Estacao", "Data", "Valores")
  anuais <- data.frame()
  for(i in unique(dados$Estacao)){
    # Criando os intervalos de datas
    auxEst <- dplyr::filter(dados, Estacao == i)
    anos <- unique(lubridate::year(auxEst$Data))
    datas <- as.Date(paste0(ano_hidro,"-",anos), "%d-%m-%Y")
    datas <- data.frame(datas, datas + lubridate::years(1) - 1)
    colnames(datas) <- c("inicio", "fim")
    # Criando os valores máximos e períodos
    med_ano <- c()
    periodo <- c()
    for(j in 1:nrow(datas)){
      auxAno <- dplyr::filter(auxEst, Data >= datas$inicio[j] &
                                Data <= datas$fim[j])
      periodo <- rbind(periodo,
                       ifelse(ano_hidro != "01-01",
                              paste0(lubridate::year(datas$inicio[j]),
                                     "-",
                                     lubridate::year(datas$fim[j])),
                              lubridate::year(datas$inicio[j])))
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
