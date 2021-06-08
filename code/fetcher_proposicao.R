library(tidyverse)
library(rcongresso)
library(futile.logger)

#' @title Mapeia siglas e ids das proposições na Câmara e no Senado
#' @description A partir de um dataframe de entrada mapeia as proposições nas casas correspondentes.
#' Se a proposição tiver apenas o id da câmara então essa função irá buscar e adicionar o id no senado caso seja encontrado.
#' Como entrada temos o dataframe com a lista de proposições e seus ids conhecidos e como retorno a mesma lista de proposições e
#' o id na outra casa, caso esse id seja encontrado.
#' @param proposicoes_input Dataframe com as proposições de entrada. Esse dataframe deve ter pelo menos duas colunas:
#' id_ext, casa. Id_ext é o id conhecido da proposição na câmara ou no senado, casa é a casa no qual o id é conhecido.
#' @param proposicoes_filepath Caminho para o CSV de proposições com mapeamento entre câmara e senado. Esse é um dado externo
#' que mapeia as proposições de 2011 até 2020. O csv está disponível em data/proposicoes_camara_senado_desde_2011.csv.
#' @return Dataframe com a mesma quantidade de linhas/proposições do proposicoes_input e com a informação dos ids e da siglas tanto
#' na câmara quanto no senado. Colunas: id_camara, id_senado, casa, sigla_camara, sigla_senado. Caso sejam passadas mais colunas no 
#' proposicoes_input essas colunas também estarão presentes no retorno final
#' Não passe um dataframe com colunas: sigla, ano. Pois estas estão reservadas para o processamento.
process_ids_proposicoes <-
  function(proposicoes_input,
           proposicoes_filepath = here::here("data/proposicoes_camara_senado_desde_2011.csv")) {
    
    proposicoes <-
      read_csv(proposicoes_filepath, col_types = cols(id_senado = "c", id_camara = "c"))
    
    proposicoes_mapeadas <- proposicoes %>%
      mutate(sigla_camara = ifelse(
        !is.na(id_camara),
        paste0(sigla_tipo_camara, " ", numero_camara, "/", ano_camara),
        NA_character_
      )) %>%
      mutate(sigla_senado = ifelse(
        !is.na(id_senado),
        paste0(sigla_tipo_senado, " ", numero_senado, "/", ano_senado),
        NA_character_
      )) %>%
      distinct(id_senado, id_camara, .keep_all = TRUE) %>%
      select(id_senado, id_camara, sigla_senado, sigla_camara, ano_senado, ano_camara)
    
    # Recupera sigla das proposições da Câmara
    proposicoes_input_camara <- proposicoes_input %>%
      filter(casa == 'camara') %>%
      left_join(proposicoes_mapeadas, by = c("id_ext" = "id_camara")) %>%
      mutate(dados = pmap(list(id_ext, casa, sigla_camara),
                          ~ fetch_sigla_ano(..1, ..2, ..3))) %>% 
      unnest(dados, keep_empty = TRUE) %>% 
      mutate(sigla_camara = if_else(is.na(sigla_camara), sigla, sigla_camara)) %>%
      mutate(ano_camara = if_else(is.na(ano_camara), ano, ano_camara)) %>%
      select(-c(sigla, ano)) %>% 
      select(id_camara = id_ext, casa, sigla_camara, ano_camara, id_senado, 
             sigla_senado, ano_senado, dplyr::everything())
    
    # Recupera ids das proposições na outra casa do congresso
    proposicoes_camara <- proposicoes_input_camara %>% 
      mutate(dados = pmap(list(sigla_camara, "senado", ano_camara),
                          ~ fetch_id_pelo_nome_formal(..1, ..2, ..3))) %>% 
      unnest(dados, keep_empty = TRUE) %>% 
      mutate(id_senado = if_else(is.na(id_senado), id_prop, id_senado),
             sigla_senado = if_else(is.na(sigla_senado), sigla_prop, sigla_senado)) %>%
      select(-c(sigla_prop, id_prop, ano_camara, ano_senado)) %>% 
      select(id_camara, id_senado, casa, sigla_camara, sigla_senado, dplyr::everything())
    
    # Recupera sigla das proposições do Senado
    proposicoes_input_senado <- proposicoes_input %>%
      filter(casa == 'senado') %>%
      left_join(proposicoes_mapeadas, by = c("id_ext" = "id_senado")) %>%
      mutate(dados = pmap(list(id_ext, casa, sigla_senado),
                          ~ fetch_sigla_ano(..1, ..2, ..3))) %>% 
      unnest(dados, keep_empty = TRUE) %>% 
      mutate(sigla_senado = if_else(is.na(sigla_senado), sigla, sigla_senado)) %>%
      mutate(ano_senado = if_else(is.na(ano_senado), ano, ano_senado)) %>%
      select(-c(sigla, ano)) %>% 
      select(id_camara, casa, sigla_camara, ano_camara, id_senado = id_ext, 
             sigla_senado, ano_senado, dplyr::everything())
    
    # Recupera ids das proposições na outra casa do congresso
    proposicoes_senado <- proposicoes_input_senado %>% 
      mutate(dados = pmap(list(sigla_senado, "camara", ano_senado),
                          ~ fetch_id_pelo_nome_formal(..1, ..2, ..3))) %>% 
      unnest(dados, keep_empty = TRUE) %>% 
      mutate(id_camara = if_else(is.na(id_camara), id_prop, id_camara),
             sigla_camara = if_else(is.na(sigla_camara), sigla_prop, sigla_camara)) %>%
      select(-c(sigla_prop, id_prop, ano_camara, ano_senado)) %>% 
      select(id_camara, id_senado, casa, sigla_camara, sigla_senado, dplyr::everything())
    
    props_finais <- proposicoes_camara %>%
      bind_rows(proposicoes_senado) %>%
      distinct(id_camara, id_senado, interesse, .keep_all = TRUE) %>%
      select(id_camara,
             id_senado,
             sigla_camara,
             sigla_senado,
             interesse)
    
    return(props_finais)
    
  }

#' @title Recupera sigla e ano das proposições
#' @description Recebe um id e casa de proposição e retorna sua sigla e seu ano de apresentação.
#' @param id Id da proposição
#' @param casa Casa da proposição
#' @param sigla Se diferente de NA ou NULL então a proposição já tem sigla capturada/conhecida
#' @return Sigla da proposição e o Ano de apresentação
fetch_sigla_ano <- function(id, casa, sigla = NULL) {
  
  flog.info(str_glue("processando {id} - {casa} {sigla}..."))
  
  sigla_ano <- tibble(
    sigla = character(),
    ano = numeric()
  )

  if (!is.na(sigla) && !is.null(sigla)) {
    return(sigla_ano)
  }
  
  if (casa == "senado") {
    sigla_ano <- rcongresso::fetch_proposicao(id, casa) %>%
      mutate(ano_materia = as.integer(ano_materia)) %>% 
      select(sigla = descricao_identificacao_materia, ano = ano_materia)
  } else if (casa == "camara") {
    sigla_ano <- rcongresso::fetch_proposicao(id, casa) %>% 
      mutate(sigla = paste0(siglaTipo, " ", as.numeric(numero), "/", ano)) %>% 
      select(sigla, ano)
  } else {
    stop("Parâmetro casa deve ser: 'camara' ou 'senado'")
  }
  
  return(sigla_ano)
}

#' @title Recupera id das proposições a partir do Nome Formal da proposição
#' @description Recebe o nome formal da proposição e retorna o id da mesma
#' @param sigla Nome formal da proposição
#' @param casa Casa para pesquisa a sigla
#' @return Id da proposição na respectiva casa e a sigla usada na pesquisa
fetch_id_pelo_nome_formal <- function(sigla = "PEC 6/2019", casa = "camara", ano_proposicao = NULL) {
  
  flog.info(str_glue("processando {sigla} - {casa} {ano_proposicao}..."))
  
  id_prop <- tibble(
    sigla_prop = character(),
    id_prop = character()
  )
  
  # A partir de 2019 as siglas foram padronizadas nas duas casas
  # https://www12.senado.leg.br/radio/1/noticia/2019/02/06/senado-e-camara-unificam-a-identificacao-das-proposicoes
  if (as.numeric(ano_proposicao) < 2019) {
    return(id_prop)
  }
  
  
  sep <- str_split_fixed(sigla, "/", 2)
  
  tipo <- str_split_fixed(sep[1] , " ", 2)[1]
  numero <- str_split_fixed(sep[1] , " ", 2)[2]
  ano <- sep[2]
  

  if (casa == "senado") {
    id <- rcongresso::fetch_proposicao_senado_sigla(sigla = tipo, numero = numero, ano = ano)
    if (nrow(id) > 0) {
      id <- id %>% pull(codigo_materia)
      id_prop <- tibble(
        sigla_prop = sigla,
        id_prop = as.character(id)
      )
    } 
  } else if (casa == "camara") {
    id <- rcongresso::fetch_id_proposicao_camara(tipo = tipo, numero = numero, ano = ano)
    id_prop <- tibble(
      sigla_prop = sigla,
      id_prop = as.character(id)
    )
  } else {
    stop("Parâmetro casa deve ser: 'camara' ou 'senado'")
  }
  
  return(id_prop)
}


#' @title Ano maior que 2019
#' @description Recebe uma sigla de proposição e informa
#' se o ano de ocorrencia é posterior a 2019
#' @param sigla sigla da proposição
#' @return Boolean
ano_maior_que_2019 <- function(sigla) {
  if (as.numeric(str_sub(sigla, -4, -1)) >= 2019) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
