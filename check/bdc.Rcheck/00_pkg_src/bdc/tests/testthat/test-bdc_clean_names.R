## skip("dont run")
skip_on_cran()
skip_on_ci()
skip_if_not_installed("curl")

scientificName <- c(
  "Fridericia bahiensis (Schauer ex. DC.) L.G.Lohmann",
  "Peltophorum dubium (Spreng.) Taub. (Griseb.) Barneby",
  "Gymnanthes edwalliana (Pax & K.Hoffm.) Laurenio-Melo & M.F.Sales",
  "LEGUMINOSAE Senna aff. organensis (Glaz. ex Harms) H.S.Irwin & Barneby",
  "Aspidosperma australe Müll.Arg."
)


testthat::test_that("test with function example", {
  r <- bdc_clean_names(sci_names = scientificName, save_outputs = FALSE)
  expect_equal(class(bdc_clean_names(sci_names = scientificName, save_outputs = FALSE)), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(r), c("scientificName", ".uncer_terms", ".infraesp_names", "names_clean", "quality"))
})

testthat::test_that("test names with accent", {
  r <- bdc_clean_names(sci_names = scientificName, save_outputs = FALSE)
  expect_equal(nrow(r), length(scientificName))
})

sci_names <- "Lauraceae Ocotea odorifera"
names_raw <-
  sci_names %>%
  tibble::as_tibble() %>%
  dplyr::rename(scientificName = value)

# Only unique taxa names will be queried (empty or NA names are excluded)
names <-
  names_raw %>%
  dplyr::distinct(scientificName, .keep_all = T) %>% # unique names
  dplyr::select(scientificName) %>% # select this column
  dplyr::mutate_all(dplyr::na_if, "") %>% # change empty names to NA
  dplyr::filter(!is.na(scientificName))

testthat::test_that("remore suffix dae", {
  r <- bdc_rem_family_names(data = names, sci_names = "scientificName")

  expect_equal(r$clean_family_names, "Ocotea odorifera")
})

sci_names <- "Solanum lacerdae"
names_raw <-
  sci_names %>%
  tibble::as_tibble() %>%
  dplyr::rename(scientificName = value)

# Only unique taxa names will be queried (empty or NA names are excluded)
names <-
  names_raw %>%
  dplyr::distinct(scientificName, .keep_all = T) %>% # unique names
  dplyr::select(scientificName) %>% # select this column
  dplyr::mutate_all(dplyr::na_if, "") %>% # change empty names to NA
  dplyr::filter(!is.na(scientificName))

testthat::test_that("avoid remove valid suffix dae", {
  r <- bdc_rem_family_names(data = names, sci_names = "scientificName")

  expect_equal(r$clean_family_names, "Solanum lacerdae")
})


sci_names <- "Taccaceae Solanum lacerdae"
names_raw <-
  sci_names %>%
  tibble::as_tibble() %>%
  dplyr::rename(scientificName = value)

# Only unique taxa names will be queried (empty or NA names are excluded)
names <-
  names_raw %>%
  dplyr::distinct(scientificName, .keep_all = T) %>% # unique names
  dplyr::select(scientificName) %>% # select this column
  dplyr::mutate_all(dplyr::na_if, "") %>% # change empty names to NA
  dplyr::filter(!is.na(scientificName))

testthat::test_that("invalid suffix dae ", {
  r <- bdc_rem_family_names(data = names, sci_names = "scientificName")

  expect_equal(r$clean_family_names, "Taccaceae Solanum")
})

