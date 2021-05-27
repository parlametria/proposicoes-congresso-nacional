library(tidyverse)
devtools::install_github("analytics-ufcg/rcongresso")


#' @title Fetch sigla
#' @description Recebe um id e casa de proposição e retorna sua sigla,
#' caso a mesma tenha ocorrido a partir de 2019
#' @param id Id da proposição
#' @param casa Casa da proposição
#' @return Sigla da proposição
fetch_sigla <- function(id, casa) {
  library(rcongresso)
  prop_sigla <- NA_character_
  
  if (casa == "senado") {
    sigla <- fetch_proposicao(id, casa) %>%
      pull(descricao_identificacao_materia)
    
    if (ano_maior_que_2019(sigla)) {
      prop_sigla <- sigla
    } else {
      message('Proposição anterior ao ano 2019')
    }
    
  } 
  if (casa == "camara") {
    sigla <- fetch_proposicao(id, casa) %>% 
      mutate(sigla = paste0(siglaTipo, " ", as.numeric(numero), "/", ano)) %>% 
      pull(sigla)
    
    if (ano_maior_que_2019(sigla)) {
      prop_sigla <- sigla
    } else {
      message('Proposição anterior ao ano 2019')
    }

  }
  
  return(prop_sigla)
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
