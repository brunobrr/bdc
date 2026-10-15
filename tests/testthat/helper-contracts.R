# Output contracts for exported flag functions ---------------------------------
#
# Every exported data-quality (flag) function promises the same things to the
# user. `expect_output_contracts()` checks one function against all of them:
#
#   1. rows:         output row count and row order equal the input;
#   2. columns:      input columns come back unchanged in name, order and
#                    values; only the documented column(s) are appended;
#   3. custom names: every column-name argument works with non-Darwin-Core
#                    names;
#   4. session:      `search()` and `sf::sf_use_s2()` are unchanged;
#   5. flags:        flag columns are logical, with TRUE meaning compliant;
#   6. edge cases:   a single row, `NA`, `""` and non-ASCII text in the tested
#                    columns don't crash it or misalign the output.
#
# Each function is described by a `contract_row()`; adding a function to the
# suite means adding one row to the table in `test-output_contracts.R`. Every
# contract is its own test, named after the function and the contract, so a
# failure says which function broke which contract.

contract_names <- c(
  rows = "contract 1 (rows): row count and order equal the input",
  columns = "contract 2 (columns): input columns kept, only flags appended",
  columns_non_syntactic =
    "contract 2 (columns): non-syntactic input column names kept",
  custom_names = "contract 3 (custom names): column-name arguments honoured",
  session = "contract 4 (session): search() and sf_use_s2() unchanged",
  flags = "contract 5 (flags): logical, TRUE = compliant",
  edge_cases = "contract 6 (edge cases): one row, NA, \"\" and non-ASCII"
)

#' Describe one exported flag function for the contract suite
#'
#' @param fun Name of the exported function.
#' @param cols Named character vector: column-name argument -> its default
#'   (Darwin Core) column name. Every entry is passed to `fun`.
#' @param build Function taking `cols` and returning a minimal valid
#'   data.frame whose columns are named after `cols`. Include compliant and
#'   non-compliant records, compliant first.
#' @param added Columns `fun` appends to the input, in order.
#' @param flags The subset of `added` that are logical flags.
#' @param expected Named list: appended (or modified) column -> its expected
#'   values for the `build()` input, aligned with the output rows.
#' @param keep Input rows expected in the output, for functions that return a
#'   subset of records by design. `NULL` means all rows.
#' @param modifies Column-name arguments whose column values `fun` may change
#'   by design (checked through `expected`, not against the input).
#' @param args Other arguments passed to `fun`.
#' @param requires Function called before each contract, to skip for a
#'   concrete reason (e.g. a missing Suggests package).
#' @param skip If not `NULL`, the reason (with issue number) the whole row is
#'   skipped.
#' @param gaps Named list: contract (a name of `contract_names`) -> reason,
#'   with issue number, to skip that one contract.
#' @noRd
contract_row <- function(fun, cols, build, added, flags = added,
                         expected = list(), keep = NULL,
                         modifies = character(), args = list(),
                         requires = \() invisible(), skip = NULL,
                         gaps = list()) {
  unknown <- setdiff(names(gaps), names(contract_names))
  if (length(unknown) > 0) stop("Unknown contract: ", unknown)
  list(
    fun = fun, cols = cols, build = build, added = added, flags = flags,
    expected = expected, keep = keep, modifies = modifies, args = args,
    requires = requires, skip = skip, gaps = gaps
  )
}

# Input: `build()` wrapped by user columns the function must carry through
# untouched, one before and one after (with non-ASCII values).
contract_input <- function(row, cols = row$cols) {
  core <- row$build(cols)
  n <- nrow(core)
  data <- data.frame(record_ref = seq_len(n), core, check.names = FALSE)
  data$collector_name <-
    rep_len(c("José Gonçalves", "Åsa Næss", "李四"), n)
  data
}

contract_call <- function(row, data, cols = row$cols) {
  fun <- get(row$fun, envir = asNamespace("bdc"))
  args <- c(list(data = data), as.list(cols), row$args)
  suppressMessages(suppressWarnings(do.call(fun, args)))
}

# Custom, non-Darwin-Core names for every column-name argument.
contract_custom_cols <- function(row) {
  stats::setNames(paste0("my_", names(row$cols)), names(row$cols))
}

# Contract 1. `keep`: input rows expected in the output; `NULL` means all;
# `"subset"` accepts any ordered subset (edge cases of subsetting functions).
expect_contract_rows <- function(out, data, keep = NULL, what = "output:") {
  ids <- out$record_ref
  if (identical(keep, "subset")) {
    expect_true(
      !is.null(ids) && all(ids %in% data$record_ref) &&
        !anyDuplicated(ids) && !is.unsorted(ids),
      label = paste(what, "rows are an ordered subset of the input")
    )
    return(invisible())
  }
  if (is.null(keep)) keep <- seq_len(nrow(data))
  expect_identical(nrow(out), length(keep), label = paste(what, "row count"))
  expect_identical(
    ids, data$record_ref[keep],
    label = paste(what, "rows (by record_ref)")
  )
}

# Contract 2. `exact = FALSE` lets the function append any subset of `added`
# (e.g. a column only added when some record is flagged).
expect_contract_columns <- function(out, data, added, modifies = character(),
                                    exact = TRUE, what = "output:") {
  out_names <- names(out)
  expect_identical(
    out_names[seq_along(data)], names(data),
    label = paste(what, "leading column names")
  )
  appended <- out_names[-seq_along(data)]
  if (exact) {
    expect_identical(appended, added, label = paste(what, "appended columns"))
  } else {
    expect_identical(
      appended, intersect(added, appended),
      label = paste(what, "appended columns")
    )
  }

  ids <- out$record_ref
  same <- intersect(setdiff(names(data), modifies), out_names)
  expect_equal(
    as.data.frame(out)[same],
    data[match(ids, data$record_ref), same, drop = FALSE],
    ignore_attr = TRUE,
    label = paste(what, "input column values")
  )
}

# Contract 5. With `expected`, the appended columns must equal it.
expect_contract_flags <- function(out, flags, expected = list(),
                                  what = "output:") {
  for (flag in flags) {
    expect_identical(
      typeof(out[[flag]]), "logical",
      label = paste(what, flag, "type")
    )
    expect_false(anyNA(out[[flag]]), label = paste(what, flag, "has NA"))
  }
  for (col in names(expected)) {
    expect_equal(
      out[[col]], expected[[col]],
      ignore_attr = TRUE,
      label = paste(what, col)
    )
  }
}

contract_checks <- list(
  rows = function(row, who) {
    data <- contract_input(row)
    expect_contract_rows(contract_call(row, data), data, row$keep, who)
  },
  columns = function(row, who) {
    data <- contract_input(row)
    out <- contract_call(row, data)
    expect_contract_columns(
      out, data, row$added, unname(row$cols[row$modifies]),
      what = who
    )
  },
  columns_non_syntactic = function(row, who) {
    data <- contract_input(row)
    names(data)[names(data) == "collector_name"] <- "collector name"
    out <- contract_call(row, data)
    expect_contract_columns(
      out, data, row$added, unname(row$cols[row$modifies]),
      what = who
    )
  },
  custom_names = function(row, who) {
    custom <- contract_custom_cols(row)
    data <- contract_input(row, custom)
    out <- contract_call(row, data, custom)
    expect_contract_columns(
      out, data, row$added, unname(custom[row$modifies]),
      what = who
    )

    # Same result as with the default names, column for column.
    reference <- contract_call(row, contract_input(row))
    renamed <- as.data.frame(out)
    hit <- match(custom, names(renamed))
    names(renamed)[hit[!is.na(hit)]] <- row$cols[!is.na(hit)]
    expect_equal(
      renamed, as.data.frame(reference),
      ignore_attr = TRUE,
      label = paste(who, "output renamed back to default names")
    )
  },
  session = function(row, who) {
    data <- contract_input(row)
    original <- suppressMessages(sf::sf_use_s2())
    withr::defer(suppressMessages(sf::sf_use_s2(original)))
    for (s2 in c(TRUE, FALSE)) {
      suppressMessages(sf::sf_use_s2(s2))
      before <- search()
      contract_call(row, data)
      expect_identical(
        search(), before,
        label = paste(who, "search() after the call")
      )
      expect_identical(
        suppressMessages(sf::sf_use_s2()), s2,
        label = paste0(who, " sf_use_s2() after the call (was ", s2, ")")
      )
    }
  },
  flags = function(row, who) {
    out <- contract_call(row, contract_input(row))
    expect_contract_flags(out, row$flags, row$expected, what = who)
  },
  edge_cases = function(row, who) {
    data <- contract_input(row)
    first <- if (is.null(row$keep)) 1 else row$keep[[1]]
    cases <- list("a single row" = data[first, , drop = FALSE])
    values <- list(
      "NA" = NA,
      '""' = "",
      "non-ASCII text" = "São José Ñandú Åçø"
    )
    for (arg in names(row$cols)) {
      col <- row$cols[[arg]]
      for (label in names(values)) {
        value <- values[[label]]
        case <- data
        if (!is.na(value)) case[[col]] <- as.character(case[[col]])
        case[[col]][[1]] <- value
        cases[[paste0(label, " in `", col, "`")]] <- case
      }
    }

    keep <- if (is.null(row$keep)) NULL else "subset"
    for (name in names(cases)) {
      case <- cases[[name]]
      what <- paste0(who, " [", name, "]")
      out <- tryCatch(contract_call(row, case), error = identity)
      if (inherits(out, "error")) {
        fail(paste0(what, " errored: ", conditionMessage(out)))
        next
      }
      expect_contract_rows(out, case, keep, what = what)
      expect_contract_columns(
        out, case, row$added, unname(row$cols[row$modifies]),
        exact = FALSE, what = what
      )
      expect_contract_flags(out, intersect(row$flags, names(out)), what = what)
    }
  }
)

#' Run one function through every output contract
#'
#' Registers one test per contract, named "<fun>() <contract>". A skipped row
#' registers a single skipped test that shows the reason.
#' @noRd
expect_output_contracts <- function(row) {
  if (!is.null(row$skip)) {
    test_that(paste0(row$fun, "() output contracts"), skip(row$skip))
    return(invisible())
  }
  for (contract in names(contract_names)) {
    who <- paste0(row$fun, "() ", contract_names[[contract]])
    test_that(who, {
      row$requires()
      gap <- row$gaps[[contract]]
      if (!is.null(gap)) skip(gap)
      contract_checks[[contract]](row, paste0(who, ":"))
    })
  }
  invisible()
}
