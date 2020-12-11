
context("rem_taxo_unc")


n_confer<- c("Jorge Anibal cf", "joao cf.", "joao CF","joao CF.", "João Cf","João cF.", "cf joao")   

test_that("confer", {
  res<-bdc_rem_taxo_unc(n_confer)
  expect_equal(res, c("Jorge Anibal", "joao","joao","joao", "João", "João", "joao" ))
})

n_afinis<-c("Jorge Anibal aff", "joao aff.", "joao AFF", "João AFF.","João AfF.", "João aFF.", "João aFF.","aff João")              

test_that("afinis", {
  res<-bdc_rem_taxo_unc(n_afinis)
  expect_equal(res, c("Jorge Anibal", "joao","joao","João", "João", "João", "João","João" ))
})

n_complex<- c("Jorge Anibal complex", "joao COMPLEX", "João Complexo.", "João COMplex","complex João")

test_that("complex", {
  res<-bdc_rem_taxo_unc(n_complex)
  expect_equal(res, c("Jorge Anibal", "joao", "João .", "João","João"))
})

n_gen<- c("Jorge Anibal gen", "joao GEN", "João gen.", "João gen ","gen João"," João GEn")

test_that("gen", {
  res<-bdc_rem_taxo_unc(n_gen)
  expect_equal(res, c("Jorge Anibal", "joao", "João", "João","João","João"))
})

n_sp<-   c("Jorge Anibal sp", "joao sp.", "João ssp", "João ssp.","sp João", "joão SSP", "joão SP." )              

test_that("sp", {
  res<-bdc_rem_taxo_unc(n_sp)
  expect_equal(res, c( "Jorge Anibal", "joao", "João", "João", "João", "joão", "joão"))
})

n_incerta<-  c("Jorge Anibal inc", "joao inc.", "João inc ", "João INC","inc João", "joão iNc")

test_that("incerta", {
  res<-bdc_rem_taxo_unc(n_incerta)
  expect_equal(res, c("Jorge Anibal", "joao","João","João","João", "joão"))
})

n_inquirenda<- c("Jorge Anibal inq", "joao inq.", "João inq ", "João INQ","inq João", "joão iNq")

test_that("inquirenda", {
  res<-bdc_rem_taxo_unc(n_inquirenda)
  expect_equal(res, c("Jorge Anibal", "joao", "João", "João", "João","joão"))
})


n_indet<- c("Jorge Anibal ind", "joao ind.", "João ind ", "João IND","ind João", "joão iNd","Jorge Anibal indet", "joao indet.", "João indet ", "João INDET","indet João", "joão iNdeT")

test_that("indet", {
  res<-bdc_rem_taxo_unc(n_indet)
  expect_equal(res, c( "Jorge Anibal", "joao","João", "João", "João", "joão",        
                       "Jorge Anibal", "joao", "João", "João", "João","joão"))
})

n_nova<- c("Jorge Anibal nov", "joao nov.", "João nov ", "João NOV","nov João", "joão NOv")

test_that("nova", {
  res<-bdc_rem_taxo_unc(n_nova)
  expect_equal(res, c("Jorge Anibal", "joao","João", "João", "João","joão"))
})


n_prox<- c("Jorge Anibal prox", "joao prox.", "João prox ", "João PROX","prox João", "joão pRox","Jorge Anibal nr", "joao nr.", "João nr ", "João NR","nr João", "joão Nr")

test_that("prox", {
  res<-bdc_rem_taxo_unc(n_prox)
  expect_equal(res, c( "Jorge Anibal", "joao","João", "João","João","joão",        
                       "Jorge Anibal", "joao","João", "João", "João","joão"))
})

n_stet<-  c("Jorge Anibal stet", "joao stet.", "João stet ", "João STET","stet João", "joão SteT")

test_that("stet", {
  res<-bdc_rem_taxo_unc(n_stet)
  expect_equal(res, c("Jorge Anibal", "joao","João", "João", "João","joão"))
})

