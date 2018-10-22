devtools::install_github("renatoinomata/hidrowebr")
library(hidrowebr)

# # Definindo diretório de trabalho
# oldwd <- getwd() #armazena o diretório de trabalho antigo
# setwd("Dados")
# est <- list.files() #vetor com todos os arquivos presentes no diretório de trabalho

est <- c("vazoes_T_64625000.TXT",
         "vazoes_T_64645000.TXT",
         "vazoes_T_64652000.TXT",
         "vazoes_T_64655000.TXT",
         "vazoes_T_64659000.TXT",
         "vazoes_T_64675002.TXT",
         "vazoes_T_64682000.TXT",
         "vazoes_T_64685000.TXT")

# Pré-processamento
# # Importa os dados brutos
dados <- importar(est) 
# # Organiza os dados em est, data e Q
dados <- organizar(dados) 
# # Descarta os meses que apresentarem mais de 5% de falhas
dados <- descartar(dados, falhas = 5, modo = "mensal") 

# Processamento e análise
# # Plotagem de gráficos
plot_hidrograma(dados, "Q")
plot_histograma(dados, "Q")
plot_boxplot(dados, "Q")
# # Determinação da Q710
# # # Cálculo das vazões q7
q7 <- qx(dados)
# # # Cálculo das mínimas anuais
q7min <- minAnuais(q7, "Qx")
# # # Distribuição de probabilidade
library(FAdist)
dist <- c("lnorm", "weibull", "gamma3", "lgamma3")
distribuicao <- distprob(q7min, "Minima", dist, 0.1)
resumo <- resumoDist(distribuicao)
# # Determinação da qmlp
Qmlp <- qmlp(dados, "Q")
# # Determinação da Q90 e Q95
# # # Construção da curva de permanência
cPerm <- cperm(dados)
# # # Cálculo da Q90 e Q95
qPerm <- qperm(cPerm, c(90, 95))

# Voltando para o diretório antigo
# setwd(oldwd)