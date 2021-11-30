context("suggest names")

sci_names <- c("Janusia occhioni", "Janusia", "Xilosma ciliatifolium",  "Crataeva benthamii", "Oxalis rhombeo ovata", "Cebus apella", "Puma concolar") 

test_that("gbif - bdc_suggest_names_taxadb suggest valid names", {
 
   res <- suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, max_distance = 0.75, provider = "gbif"))
  
   expect_equal(res,data.frame(original = sci_names, suggested = c("janusia occhionii", 
                                "janusia", 
                                "xylosma ciliatifolium", 
                                "crateva benthamii", 
                                "oxalis rhombeoovata", 
                                "cebus apella", 
                                "puma concolor"), 
                               distance = c(0.88, 0.86, 0.90, 0.89, 0.90, 0.92, 0.85)
                              )
  )
}
)

test_that("itis - bdc_suggest_names_taxadb suggest valid names", {
  
  res <- suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, max_distance = 0.75, provider = "itis", parallel = FALSE))
  
  expect_equal(res,data.frame(original = sci_names, suggested = c(NA, 
                                                                  "janusia", 
                                                                  NA, 
                                                                  NA, 
                                                                  NA, 
                                                                  "cebus apella", 
                                                                  "puma concolor"), 
                              distance = c(0.56, 0.86, 0.67, 0.61, 0.50, 0.92, 0.85)
  )
  )
}
)

test_that("ncbi - bdc_suggest_names_taxadb suggest valid names", {
  
  res <- suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, max_distance = 0.75, provider = "ncbi", parallel = FALSE))
  
  expect_equal(res,data.frame(original = sci_names, suggested = c(NA, 
                                                                  "janusia", 
                                                                  NA, 
                                                                  NA, 
                                                                  "oxalis rhombeo ovata", 
                                                                  "cebus apella", 
                                                                  "puma concolor"), 
                              distance = c(0.50, 0.86, 0.62, 0.67, 0.95, 0.92, 0.85)
  )
  )
}
)

test_that("col - bdc_suggest_names_taxadb suggest valid names", {
  
  res <- suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, max_distance = 0.75, provider = "col", parallel = FALSE))
  
  expect_equal(res,data.frame(original = sci_names, suggested = c("Janusia occhionii", 
                                                                  NA, 
                                                                  "Xylosma ciliatifolium", 
                                                                  NA, 
                                                                  "Oxalis rhombeoovata", 
                                                                  NA, 
                                                                  "Puma concolor"), 
                              distance = c(0.94, 0.43, 0.95, 0.68, 0.95, 0.67, 0.92)
  )
  )
}
)

test_that("ott - bdc_suggest_names_taxadb suggest valid names", {
  
  res <- suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, max_distance = 0.75, provider = "ott", parallel = FALSE))
  
  expect_equal(res,data.frame(original = sci_names, suggested = c("janusia occhionii", 
                                                                  "janusia", 
                                                                  "xylosma ciliatifolium", 
                                                                  "crateva benthamii", 
                                                                  "oxalis rhombeo ovata", 
                                                                  "cebus apella", 
                                                                  "puma concolor"), 
                              distance = c(0.88, 0.86, 0.90, 0.89, 0.95, 0.92, 0.85)
  )
  )
}
)


test_that("testing parallelization", {
  
  res <- suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, max_distance = 0.75, provider = "ott", parallel = TRUE))
  
  expect_equal(res,data.frame(original = sci_names, suggested = c("janusia occhionii", 
                                                                  "janusia", 
                                                                  "xylosma ciliatifolium", 
                                                                  "crateva benthamii", 
                                                                  "oxalis rhombeo ovata", 
                                                                  "cebus apella", 
                                                                  "puma concolor"), 
                              distance = c(0.88, 0.86, 0.90, 0.89, 0.95, 0.92, 0.85)
  )
  )
}
)

test_that("testing ranks", {
  
  res <- suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, rank_name = "Plantae", rank = "kingdom",
                                                   max_distance = 0.75, provider = "ott", parallel = FALSE))
  
  expect_equal(res,data.frame(original = sci_names, suggested = c("janusia occhionii", 
                                                                  "janusia", 
                                                                  "xylosma ciliatifolium", 
                                                                  "crateva benthamii", 
                                                                  "oxalis rhombeo ovata", 
                                                                  NA, 
                                                                  NA), 
                              distance = c(0.88, 0.86, 0.90, 0.89, 0.95, 0.58, 0.64)
  )
  )
}
)

test_that("testing null rank", {
  
  res <- testthat::capture_message(suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, rank_name = "Plantae", rank = NULL,
                                                   max_distance = 0.75, provider = "ott", parallel = FALSE)))
  
  expect_equal(res$message, "Please, provide both 'rank_name' and 'rank' arguments\n")
}
)

test_that("testing null rank_name", {
  
  res <- testthat::capture_message(suppressWarnings(bdc_suggest_names_taxadb(sci_name = sci_names, rank = "kingdom",
                                                   max_distance = 0.75, provider = "ott", parallel = FALSE)))
  
  expect_equal(res$message, "Please, provide both 'rank_name' and 'rank' arguments\n")
  
}
)