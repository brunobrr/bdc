#' Flag, identify and remove infraespecific categories from scientific names
#'
#' @param data 
#' @param sci_names 
#'
#' @return
#' @export
#'
#' @examples
bdc_rem_infaesp_names <- function(data, sci_names) {

  # Note that:
  # \\s = space
  # \\. = end point
  # | = or
  # $ = search at the end of a string

  ### Flag infraespecific terms (variety, subspecies, forma)

  sci_names <- data[[sci_names]] %>% stringr::str_squish()

  # subspecies
  subsp0 <- stringr::str_detect(
    sci_names,
    regex(
      "^(ssp\\.|ssp\\s|ssp$|subsp\\.|subsp\\s|subsp$)",
      ignore_case = TRUE
    )
  )

  subsp <- stringr::str_detect(
    sci_names,
    regex(
      "\\sssp\\.|\\sssp\\s|\\sssp$|\\ssubsp\\.|\\ssubsp\\s|\\ssubsp$",
      ignore_case = TRUE
    )
  )

  # forma
  forma <- stringr::str_detect(
    sci_names,
    regex("\\sf\\.|\\sf\\s|\\sfo\\.|\\fo\\.",
      ignore_case = TRUE
    )
  )

  # varietas (variety)
  var <- stringr::str_detect(
    sci_names,
    regex(
      "\\svar\\.|\\svar\\s|\\svar$|\\ssubvar\\.|\\ssubvar\\s|\\ssubvar$",
      ignore_case = TRUE
    )
  )

  terms <- c("subsp0", "subsp", "forma", "var")
  terms_names <- c("subspecies", "subspecies", "forma", "varietas")

  infraesp_term <- rep(NA, length(sci_names))
  .infraesp_names <- rep(FALSE, length(sci_names))

  for (i in 1:length(terms)) {
    t <- get(terms[i])
    posi <- which(t == TRUE)
    infraesp_term[posi] <- terms_names[i]
    .infraesp_names[posi] <- TRUE
  }

  .infraesp_names <- ifelse(.infraesp_names == TRUE, FALSE, TRUE)
  tab_res <- as.data.frame(cbind(infraesp_term, flag_infraesp))
  w <- which(is.na(sci_names))
  tab_res[w, "flag_infraesp"] <- NA

  colnames(tab_res) <- c(".infraesp_names", "infraesp_names")
  df <- dplyr::bind_cols(data, tab_res)


  ### Remove infraespecif names

  # subspecies
  spp_names_clean <- stringr::str_replace_all(
    sci_names,
    regex(
      "^(ssp\\.|ssp\\s|ssp$|subsp\\.|subsp\\s|subsp$)",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    regex(
      "\\sssp\\.|\\sssp\\s|\\sssp$|\\ssubsp\\.|\\ssubsp\\s|\\ssubsp$",
      ignore_case = TRUE
    ),
    replacement = " "
  )


  # forma
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    regex("\\sf\\.|\\sf\\s|\\f$|\\sfo\\.|\\fo\\s",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # varietas (variety)
  spp_names_clean <- stringr::str_replace_all(
    spp_names_clean,
    regex(
      "\\svar\\.|\\svar\\s|\\svar$|\\ssubvar\\.|\\ssubvar\\s|\\ssubvar$",
      ignore_case = TRUE
    ),
    replacement = " "
  )

  # Remove extra spaces
  clean_infaesp_names <-
    stringr::str_squish(spp_names_clean) %>%
    as_tibble() %>%
    dplyr::rename(clean_infaesp_names = value)

  # Add column to the database
  df <- dplyr::bind_cols(df, clean_infaesp_names)

  message(
    paste(
      "bdc_rem_infraesp_names:\nRemoved and flagged",
      sum(!.infraesp_names),
      "records.\nThree collumns were added to the database.\n"
    )
  )
  return(df)
}