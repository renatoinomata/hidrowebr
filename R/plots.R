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

# plot.dist <- function(valores, estacoes, dist){
#   dados <- data.frame(estacoes, valores)
#   fitnorm <- fitdistrplus::fitdist(valores, "norm")
#   dados$norm <-
# }
