maxAnuais <- function(dados, ano_hidro = "01-01", min_dias = 365){
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
      # Caso o ano tenha menos observações do que o especificado por min_dias
      if(nrow(auxAno) < min_dias){
        warning(paste("Período de", datas$inicio[j], "a", datas$fim[j],
                      "não possui dados para todos os dias do ano."))
        periodo <- rbind(periodo,
                         ifelse(ano_hidro != "01-01",
                                paste0(lubridate::year(datas$inicio[j]),
                                       "-",
                                       lubridate::year(datas$fim[j])),
                                lubridate::year(datas$inicio[j])))
        max_ano <- rbind(max_ano, NA)
      } else {
        periodo <- rbind(periodo,
                         ifelse(ano_hidro != "01-01",
                                paste0(lubridate::year(datas$inicio[j]),
                                       "-",
                                       lubridate::year(datas$fim[j])),
                                lubridate::year(datas$inicio[j])))
        if(any(colnames(dados) == "Q")){
          max_ano <- rbind(max_ano, max(auxAno$Q))
        } else {
          max_ano <- rbind(max_ano, max(auxAno$P))
        }
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, max_ano))
  }
  return(anuais)
}

minAnuais <- function(dados, ano_hidro = "01-01"){
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
      # Caso o ano tenha menos observações do que o especificado por min_dias
      if(nrow(auxAno) < min_dias){
        warning(paste("Período de", datas$inicio[j], "a", datas$fim[j],
                      "não possui dados para todos os dias do ano."))
        periodo <- rbind(periodo,
                         ifelse(ano_hidro != "01-01",
                                paste0(lubridate::year(datas$inicio[j]),
                                       "-",
                                       lubridate::year(datas$fim[j])),
                                lubridate::year(datas$inicio[j])))
        min_ano <- rbind(min_ano, NA)
      } else {
        periodo <- rbind(periodo,
                         ifelse(ano_hidro != "01-01",
                                paste0(lubridate::year(datas$inicio[j]),
                                       "-",
                                       lubridate::year(datas$fim[j])),
                                lubridate::year(datas$inicio[j])))
        if(any(colnames(dados) == "Q")){
          min_ano <- rbind(min_ano, min(auxAno$Q))
        } else {
          min_ano <- rbind(min_ano, min(auxAno$P))
        }
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, min_ano))
  }
  return(anuais)
}

medAnuais <- function(dados, ano_hidro = "01-01"){
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
      # Caso o ano tenha menos observações do que o especificado por min_dias
      if(nrow(auxAno) < min_dias){
        warning(paste("Período de", datas$inicio[j], "a", datas$fim[j],
                      "não possui dados para todos os dias do ano."))
        periodo <- rbind(periodo,
                         ifelse(ano_hidro != "01-01",
                                paste0(lubridate::year(datas$inicio[j]),
                                       "-",
                                       lubridate::year(datas$fim[j])),
                                lubridate::year(datas$inicio[j])))
        med_ano <- rbind(med_ano, NA)
      } else {
        periodo <- rbind(periodo,
                         ifelse(ano_hidro != "01-01",
                                paste0(lubridate::year(datas$inicio[j]),
                                       "-",
                                       lubridate::year(datas$fim[j])),
                                lubridate::year(datas$inicio[j])))
        if(any(colnames(dados) == "Q")){
          med_ano <- rbind(med_ano, mean(auxAno$Q))
        } else {
          med_ano <- rbind(med_ano, mean(auxAno$P))
        }
      }
    }
    anuais <- rbind(anuais, data.frame(i, periodo, med_ano))
  }
  return(anuais)
}
