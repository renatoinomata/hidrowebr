plot.hidrograma <- function(valores, datas, estacoes){
  dados <- data.frame(estacoes ,datas, valores)
  colnames(dados) <- c("Estação", "Data", "Q")
  Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = Data, y = Q)) +
    ggplot2::geom_line(ggplot2:: aes(color = Estação)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Hidrograma", x = "Data", y = "Q (m³/s)")
  return(Plot)
}

plot.cperm <- function(cperm, pad = FALSE){
  if(pad == FALSE) {
    Plot <- ggplot2::ggplot(dplyr::filter(cperm, Q>0), ggplot2::aes(x = Freq, y = Q)) +
      ggplot2::geom_line(ggplot2::aes(color = Estacao)) +
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
      ggplot2::geom_line(ggplot2::aes(color = Estacao)) +
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

plot.boxplot <- function(valores, estacoes, tipo = "Q"){
  if (tipo != "Q" & tipo != "P"){
    warning("Valor incorreto para a variável 'tipo'.")
    tipo <- "Q"
  }
  dados <- data.frame(valores, estacoes)
  colnames(dados) <- c(tipo, "Estação")
  if(any(colnames(dados) == "Q")){
    Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = Estação, y = Q)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Boxplot", x = "Estação", y = "Q (m³/s)")
  }
  if(any(colnames(dados) == "P")){
    Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = Estação, y = P)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Boxplot", x = "Estação", y = "P (mm)")
  }
  return(Plot)
}

plot.histograma <- function(valores, estacoes, tipo = "Q", colunas = 10){
  if (tipo != "Q" & tipo != "P"){
    warning("Valor incorreto para a variável 'tipo'.")
    tipo <- "Q"
  }
  dados <- data.frame(valores, estacoes)
  colnames(dados) <- c(tipo, "Estação")
  est <- unique(dados$Estação)
  if(any(colnames(dados) == "Q")){
    Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = Q)) +
      ggplot2::geom_histogram(alpha = 1/length(est),
                              position = "identity",
                              ggplot2::aes(fill = Estação),
                              bins = colunas) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Histograma", x = "Q (m³/s)", y = "n")
  }
  if(any(colnames(dados) == "P")){
    Plot <- ggplot2::ggplot(dados, ggplot2::aes(x = P)) +
      ggplot2::geom_histogram(alpha = 1/length(est),
                              position = "identity",
                              ggplot2::aes(fill = Estação),
                              bins = colunas) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Histograma", x = "P (mm)", y = "n")
  }
  return(Plot)
}

plot.dist <- function(valores, dist, tipo = "Q"){
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
    distr[[i]] <- fitdistrplus::fitdist(valores, "gamma3",
                                        start = c(shape = 2, scale = 2))
    i <- i + 1
  }

  if(any(dist == "lgamma3")){
    distr[[i]] <- fitdistrplus::fitdist(valores, "lgamma3",
                                      start = c(shape = 1, scale = 1))
    i <- i + 1
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
