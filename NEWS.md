# bdc 1.1.2

- Fix database version in `taxadb::td_create` inside the
  `bdc_query_names_taxadb`.
- Remove deprecated `dplyr::mutate_all` by `dplyr::across` in the
  `bdc_coordinates_empty`.
- Improve tests for `bdc_coordinates_empty`.
- Little refactor in `bdc_clean_names` fixing CRAN issues with
  encoding characters (hiding characters into a file and reading them
  when needed).

# bdc 1.1.1

- `bdc_country_from_coordinates` now filter out countries with no data (`NA`).
- Fix a minor issue of `bdc_clean_names` character encoding on debian-clang CRAN server.

# bdc 1.1.0

- Bugfix in `bdc_country_from_coordinates()` reported in #209.
- New argument added to function
  `bdc_coordinates_country_inconsistent`
- Improve tests
- Add messages when no data is available to create figures and report
- `bdc_coordinates_from_locality`: adding an error message when there
  is no column named locality
- `bdc_country_from_coordinates`: adding a message when there are no
  records missing country names. Avoiding returning a NULL object.
- `bdc_create_report`: adding coordinates transposed to the report;
  improve test and the message returned
- `bdc_clean_names`: adding code to remove strange characters (e.g.,
  accents) from names, and returning a message when these characters
  were not automatically removed.
- Remove the first column (id) of interactive tables in vignettes
- Add rlang (\>= 1.0.1) to suggestion in the DESCRIPTION file
- `bdc_coordinates_transposed`: avoid error when a column ".summary"
  is already present in the provided dataset
- `bdc_standardize_datasets`: convert all columns to characters before
  merging datasets. Added a message to indicate which column names of
  the dataset were not founded in the configuration table (metadata).

# bdc 1.0.0
