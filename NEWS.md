# bdc 1.1.0

-   bugfix in `bdc_country_from_coordinates()` reported in #209.
-   new argument added to function `bdc_coordinates_country_inconsistent`
-   Improve tests
-   Add messages when no data is available to create figures and report
-   `bdc_coordinates_from_locality`: adding an error message when there is no column named locality
-   `bdc_country_from_coordinates`: adding a message when there are no records missing country names. Avoiding returning a NULL object.
-   `bdc_create_report`: adding coordinates transposed to the report; improve test and the message returned
-   `bdc_clean_names`: adding code to remove strange characters (e.g., accents) from names, and returning a message when these characters were not automatically removed.
-   Remove the first column (id) of interactive tables in vignettes
-   Add rlang (\>= 1.0.1) to suggestion in the DESCRIPTION file
-   `bdc_coordinates_transposed`: avoid error when a column ".summary" is already present in the provided dataset
-   `bdc_clean_names`: change unicode escaping characters to lower case in #224
