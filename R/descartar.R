#' Função de descarte de anos ou meses que apresentam falhas.
#'
#' @description Descarta anos ou meses que apresentarem porcentagem de falhas
#'   superiores às especificadas pelo usuário.
#'
#' @param dados Corresponde à data frame obtida após o uso da função
#'   \code{organizar}.
#' @param falhas Valor numérico correspondente à porcentagem de falhas
#'   permitidas no mês/ano.
#' @param modo Define se o modo de descarte é \code{anual} ou \code{mensal}.
#'
#' @details Existem dois modos de descartes realizados: \code{anual} ou
#'   \code{mensal}. Serão descartados os anos/meses que possuirem valores acima
#'   do indicado pela variável \code{falhas}. O valor padrão para \code{falhas}
#'   é de 5\%.
#'
#' @export
descartar <- function(dados, falhas = 5, modo = "anual"){
  if(falhas < 0 | falhas >= 100){
    stop("Valor para porcentagem de falhas incorreto", call. = FALSE)
  } else {
    dadosAux <- data.frame()
    est <- unique(dados$Estacao)
  for(i in est){
      auxEst <- dplyr::filter(dados, Estacao == i)

      # Para descartar por ano
      if(modo == "anual"){
        anos  <- unique(lubridate::year(auxEst$Data))
        ndias <- 365 - ceiling(falhas / 100 * 365)
        for(j in anos) {
          aux <- dplyr::filter(auxEst, lubridate::year(Data) == j)
          if(nrow(aux) < ndias){
            auxEst <- auxEst[-which(lubridate::year(auxEst$Data) == j),]
          }
        }
        dadosAux <- rbind(dadosAux, auxEst)
      }

      # Para descartar por mes
      if(modo == "mensal"){
        aux <- dplyr::group_by(auxEst,
                                lubridate::year(Data),
                                lubridate::month(Data))
        aux <- dplyr::summarise(aux, obs = n())
        colnames(aux) <- c("ano", "mes", "obs")
        aux <- dplyr::ungroup(aux)
        aux$ndias <- paste0(aux$ano, "-", aux$mes, "-", aux$obs)
        aux$ndias <- as.Date(aux$ndias, "%Y-%m-%d")
        aux <- data.frame(aux)
        for(j in 1:nrow(aux)){
          while(lubridate::month(aux[j, "ndias"]) == aux[j, "mes"]){
            aux[j, "ndias"] <- aux[j, "ndias"] + 1
          }
        }
        aux$ndias <- aux$ndias - 1
        aux$ndias <- as.numeric(lubridate::day(aux$ndias))
        aux$mindias <- aux$ndias - ceiling(aux$ndias * falhas / 100)
        falhas_datas <- aux[which(aux$mindias > aux$obs), ]
        falhas_datas$inicio <- as.Date(paste0(falhas_datas$ano, "-",
                                              falhas_datas$mes, "-",
                                      "01"),
                               "%Y-%m-%d")
        falhas_datas$fim <- as.Date(paste0(falhas_datas$ano, "-",
                                    falhas_datas$mes, "-",
                                    falhas_datas$ndias),
                            "%Y-%m-%d")
        for(j in 1:nrow(falhas_datas)){
          auxEst <- dplyr::filter(auxEst, Data < falhas_datas[j, "inicio"] |
                                    Data > falhas_datas[j, "fim"])
        }
        dadosAux <- rbind(dadosAux, auxEst)
      }
    }
  }
  return(dadosAux)
}
