# bdc_clean_names() needs the gnparser binary but no network.
skip_if_no_gnparser()

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


# Realistic messy names --------------------------------------------------------
#
# Each test runs bdc_clean_names() with save_outputs = TRUE in a throwaway
# project: the returned tibble carries `.uncer_terms`, `.infraesp_names` and
# `names_clean`, and the parsed-names check file adds the flags of the other
# steps (`.family_names`, `.other_issues`) and the detected terms.
#
# Names that clean to the same string make the join inside bdc_clean_names()
# many-to-many. dplyr warns about that only for calls it sees as direct, which
# happens under devtools::load_all() but not with the installed package, so
# that one warning class is muffled here. The join is harmless (the output
# keeps one row per input, in input order), which the helper asserts on every
# call.

clean_names_and_check <- function(x, .local_envir = parent.frame()) {
  root <- local_temp_project(.local_envir = .local_envir)
  out <- withCallingHandlers(
    suppressMessages(bdc_clean_names(x, save_outputs = TRUE)),
    dplyr_warning_join_relationship_many_to_many =
      \(w) invokeRestart("muffleWarning")
  )
  expect_identical(nrow(out), length(x), label = "output row count")
  expect_identical(out$scientificName, x, label = "output names (input order)")
  check <- readr::read_csv(
    file.path(root, "Output", "Check", "02_parsed_names.csv"),
    show_col_types = FALSE
  )
  list(out = out, check = check)
}

test_that("bdc_clean_names() removes plant and animal family prefixes", {
  x <- c(
    "LEGUMINOSAE Senna organensis",
    "Fabaceae Inga vera",
    "Felidae Panthera onca (Linnaeus, 1758)",
    "Accipitridae Accipiter striatus",
    "Solanum lacerdae"
  )
  res <- clean_names_and_check(x)

  expect_equal(res$out$scientificName, x)
  expect_equal(res$check$.family_names, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(
    res$check$clean_family_names,
    c(
      "Senna organensis", "Inga vera", "Panthera onca (Linnaeus, 1758)",
      "Accipiter striatus", "Solanum lacerdae"
    )
  )
  expect_equal(
    res$out$names_clean,
    c(
      "Senna organensis", "Inga vera", "Panthera onca", "Accipiter striatus",
      "Solanum lacerdae"
    )
  )
})

test_that("bdc_clean_names() flags and removes uncertainty terms", {
  x <- c(
    "Myrcia cf. splendens",
    "Myrcia aff. splendens",
    "Myrcia sp.",
    "Myrcia spp.",
    "Myrcia splendens ?",
    "Myrcia splendens"
  )
  res <- clean_names_and_check(x)

  expect_equal(res$out$scientificName, x)
  expect_equal(res$out$.uncer_terms, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(
    res$check$uncer_terms,
    c("confer", "affinis", "species", "species", "species incerta", NA)
  )
  expect_equal(
    res$out$names_clean,
    c(
      "Myrcia splendens", "Myrcia splendens", "Myrcia", "Myrcia",
      "Myrcia splendens", "Myrcia splendens"
    )
  )
  expect_true(all(res$out$.infraesp_names))
})

test_that("bdc_clean_names() flags and removes infraspecific ranks", {
  x <- c(
    "Inga vera var. affinis",
    "Inga vera subsp. affinis (DC.) T.D.Penn.",
    "Tabebuia aurea f. alba",
    "Inga vera"
  )
  res <- clean_names_and_check(x)

  expect_equal(res$out$scientificName, x)
  expect_equal(res$out$.infraesp_names, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(
    res$check$infraesp_term,
    c("varietas", "subspecies", "forma", NA)
  )
  expect_equal(
    res$out$names_clean,
    c("Inga vera affinis", "Inga vera affinis", "Tabebuia aurea alba", "Inga vera")
  )
  expect_true(all(res$out$.uncer_terms))
})

test_that("bdc_clean_names() drops authorship with parentheses and ex", {
  x <- c(
    "Fridericia bahiensis (Schauer ex DC.) L.G.Lohmann",
    "Peltophorum dubium (Spreng.) Taub.",
    "Solanum paniculatum L. ex Dunal",
    "Panthera onca (Linnaeus, 1758)"
  )
  res <- clean_names_and_check(x)

  expect_equal(res$out$scientificName, x)
  expect_equal(
    res$out$names_clean,
    c(
      "Fridericia bahiensis", "Peltophorum dubium", "Solanum paniculatum",
      "Panthera onca"
    )
  )
  expect_true(all(res$out$.uncer_terms))
  expect_true(all(res$out$.infraesp_names))
  expect_true(all(res$check$.family_names))
})

test_that("bdc_clean_names() drops a bare author name without initials", {
  skip("authorship is lowercased and kept as an epithet (#311)")
  x <- c("Bauhinia forficata Link", "Eugenia uniflora L.")
  res <- clean_names_and_check(x)

  expect_equal(res$out$names_clean, c("Bauhinia forficata", "Eugenia uniflora"))
  expect_equal(res$out$quality, c(1, 1))
})

test_that("bdc_clean_names() fixes all-caps names and accented characters", {
  x <- c(
    "MYRCIA SPLENDENS",
    "myrcia splendens",
    "Coffea arábica",
    "Mimosa caesalpiniæfolia",
    "Aspidosperma australe Müll.Arg."
  )
  res <- clean_names_and_check(x)

  expect_equal(res$out$scientificName, x)
  expect_equal(res$check$.other_issues[1:2], c(FALSE, FALSE))
  expect_equal(
    res$out$names_clean,
    c(
      "Myrcia splendens", "Myrcia splendens", "Coffea arabica",
      "Mimosa caesalpiniaefolia", "Aspidosperma australe"
    )
  )
  expect_equal(res$out$quality[1:4], c(1, 1, 1, 1))
})

test_that("bdc_clean_names() keeps one row per input when names clean alike", {
  x <- c(
    "Felidae Panthera onca", "Inga vera", "Panthera onca",
    "cf. Panthera onca", "Inga vera"
  )
  res <- clean_names_and_check(x)

  expect_equal(
    res$out$names_clean,
    c(
      "Panthera onca", "Inga vera", "Panthera onca", "Panthera onca",
      "Inga vera"
    )
  )
  expect_equal(res$out$.uncer_terms, c(TRUE, TRUE, TRUE, FALSE, TRUE))
})

testthat::test_that("bdc_clean_names() saves and reports 02_parsed_names.csv", {
  root <- local_temp_project(subdir = "reports")

  expect_message(
    out <- bdc_clean_names(sci_names = scientificName, save_outputs = TRUE),
    "02_parsed_names\\.csv"
  )

  expect_equal(
    list.files(file.path(root, "Output", "Check")),
    "02_parsed_names.csv"
  )
  expect_false(dir.exists(file.path(root, "reports", "Output")))

  check <- readr::read_csv(
    file.path(root, "Output", "Check", "02_parsed_names.csv"),
    show_col_types = FALSE
  )
  expect_true(all(names(out) %in% names(check)))
  expect_equal(check[names(out)], out, ignore_attr = TRUE)
})
