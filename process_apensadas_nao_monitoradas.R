library(tidyverse)


#' @title Lista proposições apensadas não monitoradas 
#' @description Realiza o processamento das proposições apensadas não monitoradas para o formato usado pelo Parlametria
#' @param proposicoes_apensadas_filepath Caminho para o CSV de proposições apensadas não monotiradas
#' @param proposicoes_filepath Caminho para o CSV de proposições
#' @param export_folder Folder para exportar CSV de proposições 
#' @return 
casa_proposicoes_apensadas_nao_monitoradas <- function(proposicoes_apensadas_filepath, proposicoes_filepath, export_folder){
  
  proposicoes <- read_csv(proposicoes_filepath, col_types = cols(id_ext = "c"))
  proposicoes_apensadas_nao_monitoradas <- read_csv(props_apensadas_filepath, col_types = cols(id_ext = "c"))
  
  proposicoes <- read_csv("proposicoes_camara_senado_desde_2011.csv")
  
  sigla_props_senado <- proposicoes %>%  
    filter(!is.na(id_senado)) %>% 
    select(id_senado, sigla_tipo_senado, numero_senado, ano_senado) %>% 
    mutate(sigla = paste0(sigla_tipo_senado, " ", as.numeric(numero_senado), "/", ano_senado)) %>% 
    mutate(id_ext = id_senado) %>% 
    mutate(casa = "senado") %>% 
    select(id_ext, casa, sigla)
  
  sigla_props_camara <- proposicoes %>%  
    filter(!is.na(id_camara)) %>% 
    select(id_camara, sigla_tipo_camara, numero_camara, ano_camara) %>% 
    mutate(sigla = paste0(sigla_tipo_camara, " ", as.numeric(numero_camara), "/", ano_camara))%>% 
    mutate(id_ext = id_camara) %>% 
    mutate(casa = "camara") %>% 
    select(id_ext, casa, sigla)
  
  #TODO:
  # join dos dataframes de siglas camara e senado
  props_join <- sigla_props_camara %>% 
    full_join(sigla_props_senado, by = c("id_ext", "casa")) %>% 
    mutate(sigla_camara = sigla.x) %>% 
    mutate(sigla_senado = sigla.y) %>% 
    select(id_ext, casa, sigla_camara, sigla_senado)
  
  proposicoes_apensadas <- proposicoes_apensadas_nao_monitoradas %>%
    mutate(id_ext = as.numeric(id_ext)) %>% 
    select(id_ext, casa, interesse)
  
  #TODO:
  # join com csv de proposicoes apensadas para retornar nome, casa e interesses por id
  
  props_finais <- props_join %>% 
    inner_join(proposicoes_apensadas, by = c("id_ext", "casa"))
 
}