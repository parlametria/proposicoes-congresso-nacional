library(tidyverse)
source(here::here("utils.R"))

#' @title Lista proposições apensadas não monitoradas 
#' @description Realiza o processamento das proposições apensadas não monitoradas para o formato usado pelo Parlametria
#' @param proposicoes_apensadas_filepath Caminho para o CSV de proposições apensadas não monotiradas
#' @param proposicoes_filepath Caminho para o CSV de proposições
#' @param export_folder Folder para exportar CSV de proposições 
#' @return 
casa_proposicoes_apensadas_nao_monitoradas <-
  function(proposicoes_apensadas_filepath,
           proposicoes_filepath,
           export_folder,
           flag_separa_csv) {

    proposicoes <-
      read_csv(proposicoes_filepath, col_types = cols(id_ext = "c"))
    proposicoes_apensadas_nao_monitoradas <-
      read_csv(props_apensadas_filepath, col_types = cols(id_ext = "c"))
    
    sigla_props <- proposicoes %>%
      mutate(sigla_camara = ifelse(
        !is.na(id_camara),
        paste0(
          sigla_tipo_camara,
          " ",
          as.numeric(numero_camara),
          "/",
          ano_camara
        ),
        NA_character_
      )) %>%
      mutate(sigla_senado = ifelse(
        !is.na(id_senado),
        paste0(
          sigla_tipo_senado,
          " ",
          as.numeric(numero_senado),
          "/",
          ano_senado
        ),
        NA_character_
      )) %>%
      distinct(id_senado, id_camara, .keep_all = TRUE) %>%
      select(id_senado,
             id_camara,
             sigla_senado,
             sigla_camara,
             ano_senado,
             ano_camara)
    
    proposicoes_apensadas <- proposicoes_apensadas_nao_monitoradas %>%
      mutate(id_ext = as.numeric(id_ext)) %>%
      select(id_ext, casa, interesse)
    
    props_apensadas_camara <- proposicoes_apensadas %>%
      filter(casa == 'camara') %>%
      left_join(sigla_props, by = c("id_ext" = "id_camara")) %>%
      rowwise(.) %>%
      mutate(sigla_camara = ifelse(
        is.na(sigla_camara),
        fetch_sigla(id_ext, casa),
        sigla_camara
      )) %>%
      select(id_camara = id_ext,
             casa,
             interesse,
             id_senado,
             sigla_senado,
             sigla_camara)
    
    props_apensadas_senado <- proposicoes_apensadas %>%
      filter(casa == 'senado') %>%
      left_join(sigla_props, by = c("id_ext" = "id_senado")) %>%
      rowwise(.) %>%
      mutate(sigla_senado = ifelse(
        is.na(sigla_senado),
        fetch_sigla(id_ext, casa),
        sigla_senado
      )) %>%
      select(id_senado = id_ext,
             casa,
             interesse,
             id_camara,
             sigla_senado,
             sigla_camara)
    
    props_finais <- props_apensadas_camara %>%
      full_join(props_apensadas_senado) %>%
      select(id_camara,
             id_senado,
             casa,
             sigla_camara,
             sigla_senado,
             interesse)
    
    export_file <-
      paste0(export_folder, "apensadas_nao_monitoradas.csv")
    
    write_csv(props_finais, export_file)
    
    if (flag_separa_csv) {
      interesses <- props_finais %>%
        filter(!is.na(interesse)) %>%
        select(interesse) %>%
        distinct() %>%
        rowwise() %>%
        pull()
      
      for (interesse in interesses) {
        lista_interesses(export_file, interesse, export_folder)
      }
      
    }
    
    
  }

#' @title Gera CSVs de acordo com o interesse
#' @description Gera CSVs de proposições apensadas de acordo com o interesse passado pelo parâmetro
#' @param proposicoes_apensadas_filepath Caminho para o CSV de proposições apensadas não monotiradas
#' @param interesse_csv Nome do interesse que deseja
#' @param export_folder Folder para exportar CSV
lista_interesses <-
  function(proposicoes_apensadas_filepath,
           interesse_csv ,
           export_folderpath) {
    
    props <- read.csv(proposicoes_apensadas_filepath)
    
    filtra_interesse <- props %>%
      filter(interesse == interesse_csv)
    
    write_csv(
      filtra_interesse,
      paste0(
        export_path,
        "props_apensadas_nao_monitoradas_",
        interesse_csv,
        ".csv"
      )
    )
    
  }
