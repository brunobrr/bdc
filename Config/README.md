# Input file:

Initial database information has to be provided in file __DatabaInfo.csv__, localized on folder __Config/__.

Users must configure this file to indicate which columns in each original database correspond to each field (i.e. columns) of the __DatabaseInfo.csv__ file.
In doing so, names in the original database are translated to standard Darwin Core terms. This step is important for standardizing metadata names before databases
be merged in a standard database.

Information stored in the __DatabaInfo.csv__ file are classified into three categories:

- _Required_: Obligatory information required to run the workflow, which includes the fields.
- _Recommended_: Information not required to run the workflow but contains important details on species records.  
- _Additional_: Additional information that can be provided by users. 

Below are listed each field of the database file along with its specifications:

| Field | Category | Description of DarwinCore terms (if existing) | Class | Example |
|-|-|-|-|-|
| dataset_name | Required | A short name of the database | Character | "specieslink" |
| file_name_to_load | Required | The path containg the name of the database to be loaded | Character | "Input_files/specieslink.csv" |
| scientific_name | Required | The scientific name of taxon with or without authorship information depending on the format of the source database | Character | "Myrcia acuminata" |
| decimal_latitude | Required | The geographic latitude in decimal degrees | Numeric | -6.370833 |
| decimal_longitude | Required | The geographic longitude in decimal degrees | Numeric | -3.25500 |
| occurenceID | Recommended | Unique identifiers as presented in the original databases | Numeric | 1087566037 |
| basis_of_record | Recommended | The specific nature of the data record | Character | "PRESERVED_SPECIMEN" |
| event_date | Recommended | The date-time during which an Event occurred. Information only on year of collection can be provided | Character | "1814-01-01" |
| taxon_rank | Recommended | The taxonomic rank of the most specific name in the scientificName | Character | "SPECIES" |
| family | Recommended | The full scientific name of the family in which the taxon is classified | Character | "Lauraceae" |
| country | Recommended | The name of the country (or country code) or major administrative unit in which the Location | Character | "Brazil" |
| state_province | Recommended | The name of the next smaller administrative region than country (state, province, canton,department, region, etc.) in which the Location occurs | Character | "Goias" |
| county | Recommended | The full, unabbreviated name of the next smaller administrative region than stateProvince (county, shire, department, etc.) in which the Location occurs | Character | "Goiania" |
| locality | Recommended | The specific description of the place where taxon was sampled | Character | "Serra dos Pirineus" |
| identified_by | Additional | A list (concatenated and separated) of names of people, groups, or organizations who assigned the Taxon to the subject | Character | "M. Sobral" |
| coordinate_uncertainty_in_meters | Additional | The horizontal distance (in meters) from the given decimalLatitude and decimalLongitude describing the smallest circle containing the whole of the Location | Numeric | 10 |
| coordinate_precision | Additional | A decimal representation of the precision of the coordinates given in the decimalLatitude and decimalLongitude | Numeric | 0.0001 |
| recorded_by | Additional | A list (concatenated and separated) of names of people, groups, or organizations responsible for recording the original Occurrence | Character | "Cervi, A.C" |
|  |  |  |  |