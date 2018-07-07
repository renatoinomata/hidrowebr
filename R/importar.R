importar <- function(txt){
  # Verificação do tipo de arquivo
  if(is.character(txt) == FALSE){
    warning("Variável não é do tipo chr", call. = FALSE)
  } else {
    # Verificação da existência dos arquivos
    remove_txt <- c()
    for(i in 1:length(txt)){
      if(file.exists(txt[i]) == FALSE){
        warning(paste0("Arquivo '", txt[i], "' não existe"))
        remove_txt <- c(remove_txt, i)
      }
    }
    if(is.null(remove_txt) == FALSE){
      txt <- txt[-remove_txt]
    }
      # Importação dos dados
      dados <- data.frame()
      for(i in 1:length(txt)){
        dados_aux <- read.table(txt[i],
                                skip = 15,
                                header = TRUE,
                                sep = ";",
                                dec = ",")
        dados <- rbind(dados, dados_aux)
      }
      return(dados)
  }
}
