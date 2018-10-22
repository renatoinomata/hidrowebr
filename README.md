# hidrowebr

## Descrição
O hidrowebr é um pacote de funções para a análise estatísticas de séries históricas disponibilizadas no portal Hidroweb da Agência Nacional de Águas (ANA). Com ele é possível realizar: a importação e organização dos dados do portal Hidroweb; o cálculo de eventos de precipitações (ex: precipitação máxima associada a um período de retorno) e vazões (ex: Q7,10, vazões de permanência, vazão média de longa permanência); e plotagem de gráficos.

## Organização
As funções que compõem o pacote são divididas em três categorias: pré-processamento, processamento e análise.

### Pré-processamento
Na etapa de pré-processamento o usuário insere as séries históricas no R para que possam ser organizadas na etapa de processamento. Fazem parte desse grupo as funções _importar_, _organizar_ e _descartar_.

### Processamento
Nessa etapa, os dados pré-processados são reorganizados para os processos de análise. fazem parte desse grupo as funções _cperm_, _qx_, _maxAnuais_, _medAnuais_ e _minAnuais_.

### Análise 
Nessa etapa, são extraídas informações numéricas e gráficas acerca do conjunto de dados. Fazem parte desse grupo as funções _distprob_, _resumoDist_, _qperm_, _qmlp_, _plot_hidrograma_, _plot_histograma_, _plot_boxplot_, _plot_cperm_ e _plot_dist_.

## Informações adicionais
O hidrowebr utiliza os pacotes:
* _readr_ para a importação dos dados;
* _ggplot2_ para a plotagem dos gráficos;
* _FAdist_ e _fitdistrplus_ para os ajustes de probabilidade.

Neste repositório, na pasta _Exemplos_, encontram-se um script e dados de 8 estações diferentes. Tal script demonstra uma aplicação do hidrowebr para o cálculo de vazões Q7,10, Qmlp, Q90 e Q95.

Outro tutorial pode ser encontrado dentro do próprio _R_, bastando-se apenas o uso do comando _vignette("hidrowebr")_. Nele é utilizado como exemplo a série histórica da estação de Fluviópolis (código: 65220000).
