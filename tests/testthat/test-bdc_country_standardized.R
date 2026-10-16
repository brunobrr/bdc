# bdc_country_standardized() reads a bundled table of country names; it needs
# no network, only rnaturalearthdata to be installed.

standardize <- function(...) {
  suppressMessages(bdc_country_standardized(...))
}

test_that("full names in English are standardized with their ISO alpha-2 code", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(id = 1:3, country = c("Brazil", "Bolivia", "Argentina"))

  r <- standardize(data = x)

  expect_equal(r$country_suggested, c("Brazil", "Bolivia", "Argentina"))
  expect_equal(r$countryCode, c("BR", "BO", "AR"))
})

test_that("common and other-language names resolve to the standard name", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(
    country = c("United States", "Viet Nam", "Brasil", "Deutschland", "Estados Unidos")
  )

  r <- standardize(data = x)

  expect_equal(
    r$country_suggested,
    c("United States of America", "Vietnam", "Brazil", "Germany", "United States of America")
  )
  expect_equal(r$countryCode, c("US", "VN", "BR", "DE", "US"))
})

test_that("upper-case ISO alpha-2 codes are resolved", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(country = c("BR", "PE"))

  r <- standardize(data = x)

  expect_equal(r$country_suggested, c("Brazil", "Peru"))
  expect_equal(r$countryCode, c("BR", "PE"))
})

test_that("upper-case ISO alpha-3 codes are resolved", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(country = c("ARG", "PER", "USA"))

  r <- standardize(data = x)

  expect_equal(r$country_suggested, c("Argentina", "Peru", "United States of America"))
  expect_equal(r$countryCode, c("AR", "PE", "US"))
})

test_that("lower- and mixed-case ISO codes are resolved", {
  skip("lower/mixed-case ISO codes map to the wrong country (#313)")
  x <- data.frame(country = c("br", "Arg"))

  r <- standardize(data = x)

  expect_equal(r$country_suggested, c("Brazil", "Argentina"))
  expect_equal(r$countryCode, c("BR", "AR"))
})

test_that("country names in mixed case are standardized", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(country = c("bRaZiL", "BOLIVIA", "bolivia", "argentina"))

  r <- standardize(data = x)

  expect_equal(r$country_suggested, c("Brazil", "Bolivia", "Bolivia", "Argentina"))
  expect_equal(r$countryCode, c("BR", "BO", "BO", "AR"))
})

test_that("unknown names are returned unresolved", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(country = c("Brazil", "Atlantis", "xyzzy"))

  r <- standardize(data = x)

  expect_equal(r$country, c("Brazil", "Atlantis", "xyzzy"))
  expect_equal(r$country_suggested, c("Brazil", NA, NA))
  expect_equal(r$countryCode, c("BR", NA, NA))
})

test_that("rows, row order and user columns are kept, two columns appended", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(
    id = c(3L, 1L, 2L, 4L),
    country = c("Peru", "Brazil", "Atlantis", "Brazil"),
    note = c("c", "a", "b", "d")
  )

  r <- standardize(data = x)

  expect_equal(
    names(r),
    c("id", "country", "note", "country_suggested", "countryCode")
  )
  expect_equal(r[c("id", "country", "note")], x)
  expect_equal(r$country_suggested, c("Peru", "Brazil", NA, "Brazil"))
})

test_that("a custom country column is honoured and left unchanged", {
  skip_if_not_installed("rnaturalearthdata")
  x <- data.frame(id = 1:3, study.countries = c("Brasil", "PER", "xyzzy"))

  r <- standardize(data = x, country = "study.countries")

  expect_equal(
    names(r),
    c("id", "study.countries", "country_suggested", "countryCode")
  )
  expect_equal(r$study.countries, x$study.countries)
  expect_equal(r$country_suggested, c("Brazil", "Peru", NA))
  expect_equal(r$countryCode, c("BR", "PE", NA))
})

test_that("a missing country column raises an error naming the problem", {
  x <- data.frame(id = 1:2, pais = c("Brazil", "Peru"))

  expect_error(
    bdc_country_standardized(data = x),
    "column containing country names was not found"
  )
})
