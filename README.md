# proposicoes-congresso-nacional

## Contexto
Este repositório é responsável por mapear proposições na Câmara e no Senado.

As proposições no Brasil ptramitam nas duas casas legislativas (Câmara e Senado) e recebem em cada uma das casas um id diferente. O objetivo desse repositório é facilitar esse mapeamento de ids. Para isso são observados alguns critérios de cruzamento e mapeamento.

Critérios para mapeamento:

1. Temos uma base de dados com proposições de 2011 até parte de 2020 com o mapeamento entre as proposições antigas nas duas casas. Essa base de dados é usada para obter os ids nas duas casas num primeiro momento. O arquivo com esses dados está em: `data/proposicoes_camara_senado_desde_2011.csv`.

2. Como a base mencionada acima não é completa, usamos outra estratégia para recuperar os ids nas duas casas. A partir de 2019 a câmara e o senado passaram a unificar o nome formal das proposições. Mais informações [aqui](https://www12.senado.leg.br/radio/1/noticia/2019/02/06/senado-e-camara-unificam-a-identificacao-das-proposicoes). Com isso é possível que a partir da sigla os ids das proposições nas duas casas sejam recuperados.

## Exemplo de uso

Um caso de uso desse mapeamento é descobrir os ids nas duas casas para as proposições principais ainda não monitoradas. O csv em `data/proposicoes_apensadas_nao_monitoradas.csv` é um exemplo de csv gerado pelo Projeto Parlametria que necessita da captura dos ids correspondentes na casa revisora e não somente na casa de origem.

Para recuperar os ids completos nas duas casas é possível executar o comando:

```
Rscript code/export_apensadas_nao_monitoradas.R -a data/props_apensadas_nao_monitoradas.csv -p data/proposicoes_camara_senado_desde_2011.csv -e data/ -f 1
```

## Sobre o mapeamento

### Função
Uma função importante para o processamento da recuperação dos ids nas duas casas é a `process_ids_proposicoes` presente no arquivo `code/fetcher_proposicao.R`.

Ela é responsável por fazer o mapeamento pela sigla dos ids das proposições na Câmara e no Senado. A partir de um dataframe de entrada mapeia as proposições nas casas correspondentes. Se a proposição tiver apenas o id da câmara então essa função irá buscar e adicionar o id no senado caso seja encontrado, e vice-versa. Como entrada temos o dataframe com a lista de proposições e seus ids conhecidos e como retorno a mesma lista de proposições e o id na outra casa, caso esse id seja encontrado. 

### Parâmetros

O primeiro parâmetro é o `proposicoes_input` que é o dataframe com as proposições de entrada. Esse dataframe deve ter pelo menos duas colunas: id_ext, casa. Id_ext é o id conhecido da proposição na câmara ou no senado, casa é a casa no qual o id é conhecido.

O segundo parâmetro é o proposicoes_filepath que é o caminho para o CSV de proposições com mapeamento entre câmara e senado. Esse é um dado externo que mapeia as proposições de 2011 até 2020. O csv está disponível em data/proposicoes_camara_senado_desde_2011.csv.

### Retorno

O retorno é um Dataframe com a mesma quantidade de linhas/proposições do proposicoes_input e com a informação dos ids e da siglas tanto na câmara quanto no senado. Colunas: id_camara, id_senado, casa, sigla_camara, sigla_senado. Caso sejam passadas mais colunas no proposicoes_input essas colunas também estarão presentes no retorno final

Não passe um dataframe com colunas: sigla, ano, sigla_prop, id_prop. Pois estas estão reservadas para o processamento das proposições.

