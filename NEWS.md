# bdc 1.1.3

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
