#' Plotagem de hidrogramas
#'
#' @description Função de plotagem de hidrogramas.
#'
#' @param dados Data frame com a série hidrológica a ser utilizada.
#' @param col_valores O nome da coluna com os valores a serem utilizados.
#'
#' @details O hidrograma gerado separa as diferentes estações por cores. Os
#'   gráficos são feitos com auxílio do pacote
#'   \code{\link[ggplot2]{ggplot2-package}}.
#'
#' @examples
#' # Hidrograma da estação de Fluviópolis:
#' plot_hidrograma(fluviopolis, col_valores = "Q")
#'
#' @export
plot_hidrograma <- function(dados, col_valores){
  # Conferindo valores para col_valores
  if (is.null(col_valores))
    stop("Inserir argumento 'col_valores'.")

  if (!is.character(col_valores))
    stop("Inserir variável do tipo 'chr'.")

  if (col_valores == "Data" || col_valores == "Est")
    stop("Variável incorreta para 'col_valores'.")

  if (!any(colnames(dados) == col_valores))
    stop("Não há colunas com valores iguais a 'col_valores'.")

  colnames(dados)[which(colnames(dados) == col_valores)] <- "Q"
  Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = Data, y = Q)) +
    ggplot2::geom_line(ggplot2:: aes(color = Est)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Hidrograma", x = "Data", y = "Q (m³/s)")
  return(Plot)
}

#' Plotagem de curvas de permanência
#'
#' @description Função de plotagem de curvas de permanência.
#'
#' @param cperm Data frame obtida após a função \code{\link{cperm}}.
#' @param pad Variável booleana para indicar se há ou não a padronização das
#'   vazões.
#'
#' @details A curva de permanência gerada separa as diferentes estações por
#'   cores. Os gráficos são feitos com auxílio do pacote
#'   \code{\link[ggplot2]{ggplot2-package}}. A curva de permanência é plotada em
#'   escala logarítmica.
#'
#' @examples
#' # Curva de permanência da estação de Fluviópolis:
#' curvaPerm <- cperm(fluviopolis)
#' plot_cperm(curvaPerm)
#'
#' # Curva de permanência padronizada:
#' cPad <- cperm(fluviopolis, pad = "media")
#' plot_cperm(cPad, pad = TRUE)
#'
#' @export
plot_cperm <- function(cperm, pad = FALSE){
  names(cperm)[1] <- "Est"
  if(pad == FALSE) {
    Plot <- ggplot2::ggplot(dplyr::filter(cperm, Q>0), ggplot2::aes(x = Freq, y = Q)) +
      ggplot2::geom_line(ggplot2::aes(color = Est)) +
      ggplot2::coord_trans(y = "log10", limy = c(0.01, max(cperm$Q)),
                           limx = c(0, 100)) +
      ggplot2::scale_y_continuous(breaks = scales::trans_breaks("log10",
                                                                function(x) 10^x),
                                  labels = scales::trans_format("log10",
                                                                scales::math_format(10^.x))) +
      ggplot2::labs(x = "Frequência (%)", y = "Q (m³/s)",
                    title = paste0("Curva de permanência")) +
      ggplot2::theme_bw() +
      ggplot2::annotation_logticks(sides = "l", scaled = FALSE)
  } else {
    Plot <- ggplot2::ggplot(dplyr::filter(cperm, Q>0), ggplot2::aes(x = Freq, y = Q)) +
      ggplot2::geom_line(ggplot2::aes(color = Est)) +
      ggplot2::coord_trans(y = "log10", limy = c(0.01, max(cperm$Q)),
                           limx = c(0, 100)) +
      ggplot2::scale_y_continuous(breaks = scales::trans_breaks("log10",
                                                                function(x) 10^x),
                                  labels = function(x) format(x,
                                                              big.mark = ".",
                                                              decimal.mark = ",",
                                                              scientific = FALSE)) +
      ggplot2::labs(x = "Frequência (%)", y = "Q (%)",
                    title = paste0("Curva de permanência")) +
      ggplot2::theme_bw() +
      ggplot2::annotation_logticks(sides = "l", scaled = FALSE)
  }
  return(Plot)
}

#' Plotagem de boxplots
#'
#' @description Função de plotagem de boxplots.
#'
#' @param dados Data frame com a série hidrológica a ser utilizada.
#' @param col_valores O nome da coluna com os valores a serem utilizados.
#'
#' @details O boxplot gerado separa os dados por estação. Os gráficos são feitos
#'   com auxílio do pacote \code{\link[ggplot2]{ggplot2-package}}.
#'
#' @examples
#' # Boxplot da estação de Fluviópolis:
#' plot_boxplot(fluviopolis, col_valores = "Q")
#'
#' @export
plot_boxplot <- function(dados, col_valores){
  # Conferindo valores para col_valores
  if (is.null(col_valores))
    stop("Inserir argumento 'col_valores'.")

  if (!is.character(col_valores))
    stop("Inserir variável do tipo 'chr'.")

  if (col_valores == "Data" || col_valores == "Est")
    stop("Variável incorreta para 'col_valores'.")

  if (!any(colnames(dados) == col_valores))
    stop("Não há colunas com valores iguais a 'col_valores'.")

  # Reconstruindo dados
  dados <- data.frame(dados$Est, dados$Data, dados[col_valores])
  colnames(dados) <- c("Est", "Data", "Valores")

  Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = Est, y = Valores)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Boxplot", x = "Est", y = col_valores)

  return(Plot)
}

#' Plotagem de histogramas
#'
#' @description Função de plotagem de histogramas.
#'
#' @param dados Data frame com a série hidrológica a ser utilizada.
#' @param col_valores O nome da coluna com os valores a serem utilizados.
#' @param colunas Número de colunas a serem plotadas.
#'
#' @details O histograma gerado separa por cores os dados por estação. Os
#'   gráficos são feitos com auxílio do pacote \code{\link[ggplot2]{ggplot2-package}}.
#'
#' @examples
#' # Histograma da estação de Fluviópolis:
#' plot_histograma(fluviopolis, col_valores = "Q", colunas = 10)
#'
#' @export
plot_histograma <- function(dados, col_valores, colunas = 10){
  # Conferindo valores para col_valores
  if (is.null(col_valores))
    stop("Inserir argumento 'col_valores'.")

  if (!is.character(col_valores))
    stop("Inserir variável do tipo 'chr'.")

  if (col_valores == "Data" || col_valores == "Est")
    stop("Variável incorreta para 'col_valores'.")

  if (!any(colnames(dados) == col_valores))
    stop("Não há colunas com valores iguais a 'col_valores'.")

  # Reconstruindo dados
  dados <- data.frame(dados$Est, dados$Data, dados[col_valores])
  colnames(dados) <- c("Est", "Data", "Valores")

  est <- unique(dados$Est)
  Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = Valores)) +
    ggplot2::geom_histogram(alpha = 1/length(est),
                              position = "identity",
                              ggplot2::aes(fill = Est),
                              bins = colunas) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Histograma", x = col_valores, y = "n")
  return(Plot)
}

#' Plotagem das distribuições de probabilidade empíricas e teóricas
#'
#' @description Função de plotagem das distribuições de probabilidade empíricas
#'   e teóricas.
#'
#' @param valores Vetor numérico com as leituras a serem plotadas.
#' @param dist Vetor com o nome das distribuições a serem utilizadas.
#' @param tipo Variável que corresponde ao tipo de leitura, as opções são:
#'   \code{Q} ou \code{P}, para vazões ou precipitações, respectivamente.
#'
#' @details São gerados: um gráfico com o histograma e densidades teóricas e um
#'   gráfico com as funções de probabilidade empíricas e teóricas. Os gráficos
#'   são feitos com auxílio do pacote \code{\link[ggplot2]{ggplot2-package}} e
#'   das funções \code{\link[fitdistrplus]{denscomp}} e
#'   \code{\link[fitdistrplus]{cdfcomp}}. As distribuições suportadas são as
#'   mesmas da função \code{\link{distprob}}, sendo necessário carregar o pacote
#'   \code{FAdist} para o uso de algumas distribuições.
#'
#' @examples
#' # Distribuições de probabilidade para os valores máximos de Fluviópolis:
#' qmax <- maxAnuais(fluviopolis, col_valores = "Q")
#' distribuicoes <- c("norm", "lnorm", "gamma3", "lgamma3")
#'
#' plot_dist(valores = qmax$Maxima, dist = distribuicoes)
#'
#' @export
plot_dist <- function(valores, dist, tipo = "Q"){
  for (i in 1:length(dist)){
    if(all(dist[i] != c("norm", "lnorm", "gumbel", "weibull", "gamma3", "lgamma3")))
      stop("Valores incorretos para o parâmetro dist")
  }

  if (any(dist == "gumbel") || any(dist == "gamma3") || any(dist == "lgamma3"))
    if("package:FAdist" %in% search() == FALSE)
      stop("Algumas distribuições escolhidas requerem o pacote FAdist carregado.")

  distr <- list()
  i <- 1

  if(any(dist == "norm")){
    distr[[i]] <- fitdistrplus::fitdist(valores, "norm")
    i <- i + 1
  }

  if(any(dist == "lnorm")){
    distr[[i]] <- fitdistrplus::fitdist(valores, "lnorm")
    i <- i + 1
  }

  if(any(dist == "gumbel")){
    distr[[i]] <- fitdistrplus::fitdist(valores, "gumbel",
                                        start = c(scale = 1, location = 0))
    i <- i + 1
  }

  if(any(dist == "weibull")){
    distr[[i]] <- fitdistrplus::fitdist(valores, "weibull")
    i <- i + 1
  }

  if(any(dist == "gamma3")){
    distr[[i]] <- try(fitdistrplus::fitdist(valores, "gamma3",
                                        start = list(shape = 2, scale = 2)))
    if(!assertthat::is.error(distr[[i]])){
    i <- i + 1
    } else {
      distr[[i]] <- NULL
      dist <- dist[-i]
    }
  }

  if(any(dist == "lgamma3")){
    distr[[i]] <- try(fitdistrplus::fitdist(valores, "lgamma3",
                                      start = list(shape = 2, scale = 2))
    if(!assertthat::is.error(distr[[i]])){
    i <- i + 1
    } else {
      distr[[i]] <- NULL
      dist <- dist[-i]
    }
  }

  densPlot <- fitdistrplus::denscomp(distr, legendtext = dist,
                                     plotstyle = "ggplot") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Histograma e densidades teóricas") +
    ggplot2::ylab("Densidade")

  if(tipo == "Q")
    densPlot <- densPlot + ggplot2::xlab("Q (m³/s)")
  if(tipo == "P")
    densPlot <- densPlot + ggplot2::xlab("P (mm)")

  cdfPlot <- fitdistrplus::cdfcomp(distr, legendtext = dist,
                                   plotstyle = "ggplot") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Funções de probabilidades empíricas e teóricas") +
    ggplot2::ylab("Probabilidade")

  if(tipo == "Q")
    cdfPlot <- cdfPlot + ggplot2::xlab("Q (m³/s)")
  if(tipo == "P")
    cdfPlot <- cdfPlot + ggplot2::xlab("P (mm)")

  Plot <- list(densPlot, cdfPlot)

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  print(Plot[[1]], vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(Plot[[2]], vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
}
