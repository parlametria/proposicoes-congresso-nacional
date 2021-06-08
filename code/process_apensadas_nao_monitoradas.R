library(tidyverse)
source(here::here("code/fetcher_proposicao.R"))


#' @title Processa os ids para a lista de proposições principais não monitoradas
#' @description A partir do csv de proposições apensadas cuja proposição principal não é monitorada recupera os ids correspondentes
#' das proposições na Câmara e no Senado e salva em csv.
#' @param proposicoes_nao_monitoradas_filepath Caminho para o csv com as proposições principais não monitoradas
#' @param proposicoes_mapeadas_filepath Caminho para o csv com o histório de proposições mapeadas
#' @param export_folderpath Caminho para o diretório onde os dados de saída devem ser salvos
#' @param flag_export_interesses TRUE se para cada interesse um csv deve ser exportado. FALSE caso deva se salvar as proposições 
#' num mesmo arquivo.
process_proposicoes_nao_monitoradas <- function(proposicoes_nao_monitoradas_filepath,
                                                proposicoes_mapeadas_filepath = here::here("data/proposicoes_camara_senado_desde_2011.csv"),
                                                export_folderpath = here::here("data/"),
                                                flag_export_interesses = FALSE) {
  
  proposicoes_nao_monitoradas <- read_csv(proposicoes_nao_monitoradas_filepath, 
                                          col_types = cols(id_ext_prop_principal_raiz = "c")) %>% 
    select(id_ext = id_ext_prop_principal_raiz, casa, interesse) %>% 
    distinct(id_ext, casa, interesse)
  
  proposicoes_nao_monitoradas_complete <- process_ids_proposicoes(proposicoes_nao_monitoradas,
                                                                  proposicoes_mapeadas_filepath)
  
  write_csv(proposicoes_nao_monitoradas_complete, paste0(export_folderpath, "principais_nao_monitoradas.csv"))
  
  if (flag_export_interesses) {
    interesses <- proposicoes_nao_monitoradas_complete %>%
      filter(!is.na(interesse)) %>%
      distinct(interesse)
    
    pmap(list(interesses$interesse),
         ~ export_props_por_interesse(..1, proposicoes_nao_monitoradas_complete, export_folderpath))
  }
}

#' @title Gera CSVs de acordo com o interesse
#' @description Gera CSVs de proposições principais não monitoradas de acordo com o interesse passado pelo parâmetro
#' @param props_principais Dataframe com proposições principais para serem monitoradas
#' @param interesse_arg Nome do interesse
#' @param export_folderpath Folder para exportar CSV com as proposições do interesse
export_props_por_interesse <-
  function(interesse_arg,
           props_principais,
           export_folderpath) {
    
    props_filtradas <- props_principais %>%
      filter(interesse == interesse_arg) %>% 
      mutate(proposicao = if_else(is.na(sigla_camara), sigla_senado, sigla_camara)) %>% 
      select(proposicao, id_camara, id_senado)
    
    message(paste0("Salvando ", interesse_arg))
    write_csv(props_filtradas, paste0(export_folderpath, "props_principais_nao_monitoradas_", 
                                      interesse_arg, ".csv"))
  }
