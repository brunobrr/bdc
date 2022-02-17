df_notes <-
  data.frame(
    notes_a = c(
      "accepted",
      "accepted | wasMisspelled",
      "synonym",
      "multipleAccepted"
    ),
    notes_b = letters[1:4],
    notes_c = c(
      "wasMisspelled",
      "heterotypic synonym",
      "homotypic synonym",
      "notFound"
    ),
    notes_d = c("notFound", "accepted", "pro-parte synonym", "wasMisspelled")
  )

test_that("bdc_filter_out_names can filter accepted names", {
  res_acc <-
    bdc_filter_out_names(data = df_notes, col_name = "notes_a")

  expect_equal(
    res_acc$notes_a, c("accepted", "accepted | wasMisspelled")
  )
})

test_that("bdc_filter_out_names can filter multiple taxonomic status", {
  res_acc <-
    bdc_filter_out_names(
      data = df_notes,
      col_name = "notes_a",
      taxonomic_status = c("accepted", "synonym")
    )

  expect_equal(
    res_acc$notes_a, c("accepted", "accepted | wasMisspelled", "synonym")
  )
})

test_that("bdc_filter_out_names when opposite is TRUE", {
  res_acc <-
    bdc_filter_out_names(data = df_notes, col_name = "notes_a", opposite = T)

  expect_equal(
    res_acc$notes_a, c("synonym", "multipleAccepted")
  )
})

test_that("bdc_filter_out_names when opposite is TRUE and multiple taxonomic status provided", {
  res_acc <-
    bdc_filter_out_names(
      data = df_notes, col_name = "notes_a",
      taxonomic_status = c("accepted", "synonym"),
      opposite = T
    )

  expect_equal(
    res_acc$notes_a, c("multipleAccepted")
  )
})


test_that("bdc_filter_out_names misuse taxonomic status", {
  expect_error(bdc_filter_out_names(
    data = df_notes, col_name = "notes_a",
    taxonomic_status = c("acc", "ss"),
    opposite = T
  ))
})


test_that("bdc_filter_out_names collumn not contain valid taxonomic status", {
  expect_error(
    bdc_filter_out_names(
      data = df_notes, col_name = "notes_b",
      taxonomic_status = "accepted",
      opposite = F
    )
  )
})

test_that("bdc_filter_out_names collumn containing taxonomic notes not found misapplied", {
  expect_error(
    bdc_filter_out_names(
      data = df_notes, col_name = "scientific.name",
      taxonomic_status = "accepted",
      opposite = F
    )
  )
})

test_that("bdc_filter_out_names data is not a data.frame", {
  expect_error(
    bdc_filter_out_names(
      data = df_notes$notes_b, col_name = "notes_b",
      taxonomic_status = "accepted",
      opposite = F
    )
  )
})
