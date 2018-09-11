#' Função de importação dos dados de arquivos \code{txt} e \code{csv}.
#'
#' @description Importa os dados dos arquivos disponibilizados pela Hidroweb
#'   para o R.
#'
#' @param arq Variável do tipo \code{chr}. Corresponde ao nome ou caminho do
#'   arquivo a ser importado.
#' @param skip Número de linhas a serem puladas.
#'
#' @return Por meio dessa função serão importados os dados tais quais estão
#'   dispostos no arquivo original do portal Hidroweb. Na versão 2.0.0 do
#'   Hidroweb, o número de linhas a serem puladas para arquivos de vazão é 13,
#'   valor padrão do argumento \code{skip}.
#'
#' @export
importar <- function(arq, skip = 13){
  # Verificação do tipo de arquivo
  if(!is.character(arq))
    stop("Variável não é do tipo chr.", call. = FALSE)

  # Verificação da existência dos arquivos
  remove_arq <- c()
  for(i in 1:length(arq)){
    if(file.exists(arq[i]) == FALSE){
      warning(paste0("Arquivo '", arq[i], "' não existe"))
      remove_arq <- c(remove_arq, i)
    }
  }
  if(is.null(remove_arq) == FALSE){
    arq <- arq[-remove_arq]
  }
    # Importação dos dados
    dados <- data.frame()
    for(i in 1:length(arq)){
      dados_aux <- readr::read_delim(arq[i],
                                     ";", escape_double = FALSE,
                                      locale = readr::locale(decimal_mark = ",",
                                                             grouping_mark = "."),
                                      trim_ws = TRUE,
                                      skip = skip)
      dados <- rbind(dados, dados_aux)
    }
    return(dados)
}
