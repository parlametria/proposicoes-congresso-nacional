library(tidyverse)
library(here)
source(here::here("code/process_apensadas_nao_monitoradas.R"))

.HELP <- "
Usage:
Rscript export_apensadas_nao_monitoradas.R -a <proposicoes_apensadas_filepath> -p <proposicoes_filepath>  -e <export_folder> -f <flag_separa_csv>
proposicoes_apensadas_filepath: Caminho para o csv com as proposições apensadas
proposicoes_filepath: Caminho para o csv com as proposições da Câmara e Senado
export_folder: Caminho para exportação dos csvs
flag_separa_csv: Flag indicando se os csvs devem ser separados por interesse
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly = TRUE)
  
  option_list = list(
    optparse::make_option(
      c("-a", "--apensadas"),
      type = "character",
      default = here::here("data/props_apensadas_nao_monitoradas.csv"),
      help = .HELP,
      metavar = "character"
    ),
    optparse::make_option(
      c("-p", "--proposicoes"),
      type = "character",
      default = here::here("data/proposicoes_camara_senado_desde_2011.csv"),
      help = .HELP,
      metavar = "character"
    ),
    optparse::make_option(
      c("-e", "--export"),
      type = "character",
      default = here::here("data/"),
      help = .HELP,
      metavar = "character"
    ),
    optparse::make_option(
      c("-f", "--flag"),
      type = "character",
      default = "1",
      help = .HELP,
      metavar = "character"
    )
  )
  
  
  opt_parser <- optparse::OptionParser(option_list = option_list)
  opt <- optparse::parse_args(opt_parser)
  return(opt)
  
}

## Process args
args <- get_args()
print(args)

proposicoes_apensadas <- args$apensadas
proposicoes <- args$proposicoes
saida <- args$export
flag <- args$flag

flag_filter <- if_else(flag == "1",
                       TRUE,
                       FALSE)
print("Processando e escrevendo csvs...")

process_proposicoes_nao_monitoradas(proposicoes_apensadas,
                                    proposicoes,
                                    saida,
                                    flag_filter)

print("Salvo")
