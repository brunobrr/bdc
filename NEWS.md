# bdc (development version)

- `bdc_create_figures()`, `bdc_create_report()`, `bdc_coordinates_transposed()`, `bdc_country_from_coordinates()`, `bdc_coordinates_country_inconsistent()` and other functions no longer attach packages to the search path when checking optional dependencies. `bdc_country_from_coordinates()` and `bdc_coordinates_country_inconsistent()` now restore the previous spherical geometry (`sf::sf_use_s2()`) setting on exit, even on error. `hexbin` is now listed in Suggests, and `bdc_create_figures()` no longer requires `rworldmap`. The `rnaturalearthdata` hint now suggests the CRAN install (#292).
- `bdc_query_names_taxadb()` now honours `parallel`: with the default `parallel = FALSE`, fuzzy matching no longer starts worker processes, and with `parallel = TRUE` the cluster is stopped even when matching fails. Supplying only one of `rank` or `rank_name` now stops immediately with an error naming both, instead of failing later with "object 'valid_duplicates' not found". Fuzzy matching no longer runs an unused query against the taxonomic database, and the help page shows its description again (#290).
- `bdc_standardize_datasets()` now returns only the datasets described in the `metadata` of that call. Previously, intermediate files were shared across calls in the same session, so a later call also returned records from earlier calls, and a dataset that failed standardization could be replaced by a stale copy from an earlier run; failing datasets are now excluded and named in a message. With `overwrite = FALSE` and an existing merged file, the CSV format now returns that table (previously `NULL`), read with all columns as character like the qs2 format. `save_database = TRUE` now saves the merged file whenever a new one is built, not only when `overwrite = TRUE`. Missing required metadata fields now raise an error naming each dataset and its missing fields, and the missing-column message no longer requires knitr (#291).
- `bdc_coordinates_country_inconsistent()`, `bdc_country_from_coordinates()` and `bdc_coordinates_transposed()` now honour custom column names. `bdc_coordinates_country_inconsistent()` no longer errors with custom `lat`/`lon`; `bdc_country_from_coordinates()` reads and fills the column named by `country` (previously it ignored it and reported "Nothing was done") and no longer adds `decimalLatitude`/`decimalLongitude` helper columns; `bdc_coordinates_transposed()` returns the caller's original names for id, scientific name, coordinates and country code instead of Darwin Core names. The `coordinates_transposed` column keeps its name and stays out of `.summary`: it is informational, because transposed records are corrected, not invalidated (#293).
- `bdc_query_names_taxadb()` now returns exactly one row per supplied name, in input order, with `original_search` identical to `sci_name`. Previously, names not in sentence case (e.g. "puma concolor", "PUMA CONCOLOR") produced extra, misaligned rows (4 names could return 7 rows), breaking workflows that bind the result to the occurrence table by position. The function now stops with an informative error instead of returning misaligned data (#289).
- `bdc_create_report()` and `bdc_create_figures()` now find the transposed-coordinates check file (`Output/Check/01_coordinates_transposed.csv`) at the project root, where `bdc_coordinates_transposed()` writes it. Previously, when run from a subfolder (e.g. a knitted Rmd), the report silently dropped the transposed-coordinates row and the map was skipped (#295).
- `bdc_create_figures()` and `bdc_create_report()` now error with a message listing the valid steps when `workflow_step` is missing or invalid. Previously, `bdc_create_figures()` failed with `argument is of length zero`, and `bdc_create_report()` accepted partial values such as `"pre"` and silently returned the input data unchanged (#295).
- `bdc_clean_names()` message and help page now name the file it actually writes, `Output/Check/02_parsed_names.csv` (#295).
- `bdc_year_outOfRange()` behaviour change: records with missing or empty dates are now compliant (`TRUE`) whether `year_threshold` is numeric or `NULL`; previously `year_threshold = NULL` flagged them as `FALSE`. Records from exactly the `year_threshold` year are now compliant (inclusive lower bound); previously they were flagged as too old. The help page now documents both rules (#294).
- `bdc_coordinates_country_inconsistent()` now resolves the `country_name` and the country column to a single canonical vocabulary before comparing names. Previously, records in the United States and Russia were wrongly flagged as inconsistent when the country was given by a common name ("United States", "Russia") or an ISO code ("US", "RU"), because the function compared Natural Earth's `name_long` (e.g. "Russian Federation", "United States of America") directly to `country_name`. Full names, common names, and ISO alpha-2/alpha-3 codes are now all accepted (#280).
- Fixed deprecation warnings in `bdc_standardize_country()` (tidyselect `.data[[var]]` -> `all_of()`) and `bdc_quickmap()` (ggplot2 `borders()` -> `annotation_borders()`) (#281).

## taxadb 0.3.0 compatibility (#278)

- `bdc_query_names_taxadb()` and `bdc_filter_name()` / `bdc_filter_id()` pin `db_version = "22.12"`, which remains available and byte-identical at the new data location (source.coop Parquet). No change needed for existing scripts.
- `fb` (FishBase) and `slb` (SeaLifeBase) are republished in taxadb 0.3.0 in version 2026 (CC-BY-NC), but remain blocked in `bdc_query_names_taxadb()` because bdc pins `db_version = "22.12"`, which does not include them. The error message now directs users to `taxadb::available_providers("2026")`.
- `iucn`, `tpl`, and `wd` remain blocked with an updated error message: iucn cannot be redistributed under Red List terms, TPL was retired upstream in 2013, and wd was never published. Use `rredlist` for Red List data.
- Removed the `## FIXME` note referencing ropensci/taxadb#88, which is closed.
- Removed dead `contentid` / `TAXADB_DRIVER` environment-variable code in `bdc_suggest_names_taxadb()`, superseded by the Parquet-on-source.coop reading layer.
- Bumped `taxadb` dependency from `>= 0.1.3` to `>= 0.3.0`; removed `contentid` from Suggests.

# bdc 1.1.6

- `reword-countries.txt` and `country_names.txt` were updated and fixed (by @sjevelazco; [274](https://github.com/brunobrr/bdc/pull/274)).
- Migrated from `{qs}` to `{qs2}` package. Updated `bdc_standardize_datasets()` to use `qs2::qs_save()` and `qs2::qs_read()` instead of `qs::qsave()` and `qs::qread()`. The format option has been changed from `"qs"` to `"qs2"` and file extensions from `.qs` to `.qs2`.

# bdc 1.1.5

- Move `{doParallel}` from Suggests to Imports (Thanks, @black-snow; PR #251).
- Minor adjustments in `bdc_correct_coordinates` for improved functionality.
- Debugged `bdc_quick_map`: updated dependency handling for the dplyr package.
- Enhanced `bdc_country_standardize` function: added an internal function to improve country name checking and standardization.
- Deprecated rgnarspser lifecycle. Added a tutorial on the bdc website (https://brunobrr.github.io/bdc/articles/help/installing_gnparser.html) with installation instructions for gnparser on macOS, Windows, and Linux.
- Updated `bdc_parse_names` documentation for clarity.
- Updated `bdc_create_figures`: removed outdated dependencies (sp, rgeos), debugged functionality, and added a new "hex" argument.
- Updated README file.


# bdc 1.1.4

- Adaptation of the databases to the latest version of `{taxadb}`
  (0.2.0), temporarily discontinuing the `tpl`, `fb` and `iucn`
  databases (see [here](https://github.com/ropensci/taxadb/commit/593c7856a603c802762829d60acb2a313ad7a6dd)).

- `bdc_query_names_taxadb()` now informs the database provider and its
  version and does not create Output directory when
  `export_accepted=FALSE`.

# bdc 1.1.3

- `bdc_standardize_datasets` now throws an error when dataset names defined in the metadata file are not unique.
- Fix minor bug in `bdc_coordinates_country_inconsistent()` (see: 5c4e0aa).
- `{countrycode}` and `{rangeBuilder}` dependencies were
  removed. Country names now are derived from [Stefan Gabos](https://github.com/stefangabos/world_countries/) repository
  following the [ISO 3166-1](https://www.iso.org/iso-3166-country-codes.html) standard.

- `{bdc}` wins the Ebbe Nielsen Challenge GBIF 2022 (see links below)!
  - https://www.gbif.org/news/6J94JrRZtDCPhUZMMiTALq/gridder-and-bdc-share-top-honors-in-2022-gbif-ebbe-nielsen-challenge
  - https://www.sibbr.gov.br/noticia/gridder-e-bdc-propostas-vencedoras-do-desafio-ebbe-nielsen-2022.html?lang=pt_BR
  - https://ecoevol.ufg.br/n/160758-ferramenta-biodiversity-data-cleaning-bdc-agraciada-com-o-primeiro-lugar-do-desafio-ebbe-nielsen-2022-do-gbif
  - http://www.ueg.br/noticia/59726_trabalhos_de_professores_da_ueg_sao_premiados_em_eventos
  - https://jhortal.com/geiziane-tessarolo-and-colleagues-win-gbifs-ebbe-nielsen-challenge/

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
