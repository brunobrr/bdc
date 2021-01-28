# Input file:

Initial database information has to be provided in file __DatabaseInfo.csv__, localized on folder __Config/__.

Users must configure this file to indicate which columns in each original database correspond to each field (i.e. columns) of the __DatabaseInfo.csv__ file.
In doing so, names in the original database are translated to standard Darwin Core terms. This step is important for standardizing metadata names before databases
be merged in a standard database.

Below are listed the specifications of each field of the database, where:

__Field__: Name of the fields in _DatabaseInfo.csv_ to be filled in.

__Category__: Classification of each field in _DatabaseInfo.csv_. If _required_, the field is obligatory information required to run the workflow; if _recommended_, the field is not mandatory to be able to run the workflow but it contains important details on species records; if _additional_, the field contains information that can be provided by users.

__Description__: Description about the content of the specified field in the original database.

__Type__: Type of content data on the specified field in the original database.

__Example__: An example of a single content on the specified field in the original database.  


***

| Field | Category | Description of DarwinCore terms (if existing) | Type | Example |
|-|-|-|-|-|
| dataset_name | Required | A short name of the database | Character | specieslink |
| file_name_to_load | Required | The path containg the name of the input database to be loaded | Character | Input_files/specieslink.csv |
| scientific_name | Required | Name of the column in the original database presenting the scientific name of the taxon with or without authorship information, depending on the format of the source database | Character | Myrcia acuminata |
| decimal_latitude | Required | Name of the column in the original database presenting the geographic latitude in decimal degrees | Numeric | -6.370833 |
| decimal_longitude | Required | Name of the column in the original database presenting the geographic longitude in decimal degrees | Numeric | -3.25500 |
| occurrenceID | Recommended | Name of the column presenting the unique identifiers in the original databases. If absent, NA. | Numeric | 1087566037 |
| basis_of_record | Recommended | Name of the column in the original database presenting the specific nature of the data record | Character | PRESERVED_SPECIMEN |
| event_date | Recommended | The date-time during which an Event occurred. Information only on year of collection can be provided | Character | 1814-01-01 |
| taxon_rank | Recommended | Name of the column in the original database presenting the taxonomic rank of the most specific name in the scientificName | Character | SPECIES |
| family | Recommended | Name of the column in the original database presenting the full scientific name of the family in which the taxon is classified | Character | Lauraceae |
| country | Recommended | Name of the column in the original database presenting the name of the country (or country code) or major administrative unit in which the Location | Character | Brazil |
| state_province | Recommended | Name of the column in the original database presenting the name of the next smaller administrative region than country (state, province, canton,department, region, etc.) in which the Location occurs | Character | Goias |
| county | Recommended | Name of the column in the original database presenting the full, unabbreviated name of the next smaller administrative region than stateProvince (county, shire, department, etc.) in which the Location occurs | Character | Goiania |
| locality | Recommended | Name of the column in the original database presenting the specific description of the place where taxon was sampled | Character | Serra dos Pirineus |
| identified_by | Additional | Name of the column in the original database presenting the list (concatenated and separated) of names of people, groups, or organizations who assigned the taxon to the subject | Character | M. Sobral |
| coordinate_uncertainty_in_meters | Additional | Name of the column in the original database presenting the horizontal distance (in meters) from the given decimalLatitude and decimalLongitude describing the smallest circle containing the whole of the Location | Numeric | 10 |
| coordinate_precision | Additional | Name of the column in the original database presenting the decimal representation of the precision of the coordinates given in the decimalLatitude and decimalLongitude | Numeric | 0.0001 |
| recorded_by | Additional | Name of the column in the original database presenting the list (concatenated and separated) of names of people, groups, or organizations responsible for recording the original Occurrence | Character | Cervi, A.C |
|  |  |  |  |