if (FALSE) {

  if (!require("usethis")) install.packages("usethis")
  if (!require("prefixer")) install.packages("prefixer")
  if (!require("rstudioapi")) install.packages("rstudioapi")
  if (!require("devtools")) install.packages("devtools")
  
  ############################################################
  #                                                          #
  #           IMPORTANTE: defina em `func` o nome            #
  #           da função (sem parênteses; sem .R).            #
  #                # A sequência de todo esse                #
  #          workflow será baseada no objeto `func`          #
  #                                                          #
  ############################################################
  func <- "bdc_flag_invalid_xy"
  func_file <- paste0("R/", func, ".R")
  
  # cria arquivo de teste para a função
  # ver: https://rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf
  usethis::use_test(func)
  
  # rode os testes
  devtools::test_file(paste0("tests/testthat/test-", func, ".R"))
  
  # abre o arquivo da função e o painel do prefixer
  # atenção: ignore dplyr::id e utils::data
  rstudioapi::navigateToFile(func_file); prefixer::prefixer()
  
  # carrega a função
  source(paste0("R/", func, ".R"))
  
  # gera o importFrom para ser colado na documentação da função, abaixos dos @params.
  # exemplo:
  #' @param worldmap_cntr_code 
  #' 
  #' @importFrom dplyr select bind_rows
  #' @importFrom sp SpatialPoints over
  prefixer::import_from(paste(func))
  
  # renova a documentação da função
  # ver: https://r-pkgs.org/man.html
  # ou: https://rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf
  devtools::document()

}
