library(tidyverse)
source(here::here("utils.R"))

#' @title Lista proposições apensadas não monitoradas 
#' @description Realiza o processamento das proposições apensadas não monitoradas para o formato usado pelo Parlametria
#' @param proposicoes_apensadas_filepath Caminho para o CSV de proposições apensadas não monotiradas
#' @param proposicoes_filepath Caminho para o CSV de proposições
#' @param export_folder Folder para exportar CSV de proposições 
#' @return 
casa_proposicoes_apensadas_nao_monitoradas <- function(proposicoes_apensadas_filepath, proposicoes_filepath, export_folder){
  
  proposicoes <- read_csv(proposicoes_filepath, col_types = cols(id_ext = "c"))
  proposicoes_apensadas_nao_monitoradas <- read_csv(props_apensadas_filepath, col_types = cols(id_ext = "c"))
  
  proposicoes <- read_csv("../proposicoes-congresso-nacional/proposicoes_camara_senado_desde_2011.csv")
  
  sigla_props <- proposicoes %>% 
    mutate(sigla_camara = ifelse(
      !is.na(id_camara),
      paste0(sigla_tipo_camara, " ", as.numeric(numero_camara), "/", ano_camara),
      NA_character_
    )) %>%
    mutate(sigla_senado = ifelse(
      !is.na(id_senado),
      paste0(sigla_tipo_senado, " ", as.numeric(numero_senado), "/", ano_senado),
      NA_character_
    )) %>% 
    distinct(id_senado, id_camara, .keep_all = TRUE) %>% 
    select(id_senado, id_camara, sigla_senado, sigla_camara)
  
  #TODO:
  # join dos dataframes de siglas camara e senado
  props_teste <- sigla_props %>% 
    rowwise(.) %>%
    mutate(
      id_camara = ifelse(
        !is.na(id_camara),
        id_camara,
        .fetch_id(
          link_casa = NA,
          nome = sigla,
          casa = "camara"
        )
      ),
      id_senado = ifelse(
        !is.na(id_senado),
        id_senado,
        .fetch_id(
          link_casa = NA,
          nome = sigla,
          casa = "senado"
        )
      )
    )
  
  proposicoes_apensadas <- proposicoes_apensadas_nao_monitoradas %>%
    mutate(id_ext = as.numeric(id_ext)) %>% 
    select(id_ext, casa, interesse)
  
  #TODO:
  # join com csv de proposicoes apensadas para retornar nome, casa e interesses por id
  
  props_apensadas_camara <- proposicoes_apensadas %>% 
    filter(casa == 'camara') %>% 
    left_join(sigla_props, by = c("id_ext" = "id_camara")) %>% 
    select(id_camara = id_ext, casa, interesse, id_senado, sigla_senado, sigla_camara)
  
  props_apensadas_senado <- proposicoes_apensadas %>% 
    filter(casa == 'senado') %>% 
    left_join(sigla_props, by = c("id_ext" = "id_senado")) %>% 
    select(id_senado = id_ext, casa, interesse, id_camara, sigla_senado, sigla_camara)
  
  #TODO:
  # criar função que resgata sigla pelo rcongresso a partir do id (checar ano > 2019)
  
}

#' @title Gera CSVs de acordo com o interesse 
#' @description Gera CSVs de proposições apensadas de acordo com o interesse passado pelo parâmetro
#' @param proposicoes_apensadas_filepath Caminho para o CSV de proposições apensadas não monotiradas
#' @param interesse_csv Nome do interesse que deseja 
#' @param export_folder Folder para exportar CSV 
lista_interesses <- function(proposicoes_apensadas_filepath, interesse_csv , export_folderpath){
  
  props <- read.csv(proposicoes_apensadas_filepath)
  
  filtra_interesse <- props %>% 
    filter(interesse==interesse_csv)
  
  write.csv(filtra_interesse, export_folderpath)
  
}