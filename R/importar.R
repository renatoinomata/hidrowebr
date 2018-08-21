#' Função de importação dos dados de arquivos \code{txt} e \code{csv}.
#'
#' @param arq Variável do tipo \code{chr}. Corresponde ao nome ou caminho do
#'   arquivo a ser importado.
#'
#' @details Por meio dessa função serão importados os dados tais quais estão
#'   dispostos no arquivo original do portal Hidroweb.
#'
#' @export
importar <- function(arq){
  # Verificação do tipo de arquivo
  if(is.character(arq) == FALSE){
    stop("Variável não é do tipo chr", call. = FALSE)
  } else {
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
        if(grepl("csv", arq[i]))
          dados_aux <- read.csv(arq[i],
                                skip = 15,
                                header = TRUE,
                                sep = ";",
                                dec = ",")
        else
          dados_aux <- read.table(arq[i],
                                  skip = 15,
                                  header = TRUE,
                                  sep = ";",
                                  dec = ",")
        dados <- rbind(dados, dados_aux)
      }
      return(dados)
  }
}
