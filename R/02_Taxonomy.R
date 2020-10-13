# Install and load the following packages ---------------------------------

# Install package flora and vroom directly from github
install_github("gustavobio/flora")
devtools::install_github("r-lib/vroom")
devtools::install_github('bbc/bbplot')

# ipak function: install and load multiple R packages.
# Check to see if packages are installed.
# Install them if they are not, then load them into the R session.
# Forked from: https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}

ipak(c("tidyverse", "data.table", "skimr", "naniar", "devtools", "stringr", 
       "tm", "vroom", "furrr", "tidylog",  "colorspace", "taxize", "qdap", 
       "DT", "raster", "flora", "bbplot", "CoordinateCleaner",  "rCAT"))

# palette of colours
pal <- viridisLite::viridis(10)







# Load raw database -------------------------------------------------------
# 21732211 records | 24 original columns
data_raw <-
  vroom(
    "data/clean/01_merge_all_datasets/merge_all_datasets.csv",
    delim = ",",
    locale = locale(encoding = "UTF-8"))

# Select columns that will be used in the subsequent analysis. It is worth noting that it is posible retrieve results or combine columns by using the unique id "record_id"
data <- data_raw %>% dplyr::select(
  record_id,
  basisOfRecord,
  database_source,
  latitude,
  longitude,
  verbatim_scientific_binomial,
  verbatim_scientific_name_author,
  verified_species_binomial,
  year
)

# Total number of records
data %>% nrow # 21732211

# Number of records per database
records_database <- data %>% 
  group_by(database_source) %>% 
  count(sort = T) %>% 
  arrange(., database_source)
fwrite(records_database, "output/results/records_RAW_database.csv")

# database_source       n
# 1 ATLANTIC        99056
# 2 BIEN            3325918
# 3 DRYFLOR         184279
# 4 GBIF            2075019
# 5 ICMBIO          247393
# 6 IDIGBIO         5953985
# 7 NEOTROPTREE     698445
# 8 SIBBR           3073376
# 9 SPECIESLINK     5799278
# 10 ZWIENER        275462

# Figure: Total number of records per database
# my_pallete <- rev(colorspace::qualitative_hcl(10, "Harmonic"))

data %>% group_by(database_source) %>%
  count(sort = T) %>%
  ggplot(aes(
    x = reorder(database_source,n),
    y = n,
    fill = as.factor(database_source))) +
  theme_minimal() +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Database", y = "Number of records") +
  scale_fill_manual(values = pal)


# save the figure
ggsave("output/figures/f0_numberRecordsDatabase.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f0_numberRecordsDatabase.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)








# Merge species names in a single collumn ---------------------------------

# Note that species' names are present in the colluns "verified_species_binomial" and "verbatim_scientific_name_author", which difference is that the last cointain the names of the authors.

# The taxonomic names in BIEN is checked against authoritative sources by using TNRS. Verified names are shown in the column "verified_species_binomial" as well as in DRYFLOR. Verbatim (non-verified names) are in column "verbatim_scientific_binomial". 

# GBIF uses its backbone taxonomy for checking species names. Species verified names are shown in the column "verbatim_scientific_name_author". Verbatim (non-verified names) are in column "verbatim_scientific_binomial".

# Other databases (DRYFLOR, ICMBIO, IDIGBIO, SIBBR) also performed some kind of taxonomic checking. However, the information on which authoritative taxonomic sources is used to checking species names is not available.

# Species and author names
data %>% 
  drop_na_(., vars = c("verbatim_scientific_name_author")) %>% 
  group_by(database_source) %>% 
  summarise(n())
# database_source   `n()`
# 1 BIEN            3325908
# 2 DRYFLOR          184279
# 3 GBIF            2075019
# 4 ICMBIO           247393
# 5 IDIGBIO         5792246
# 6 SIBBR           3068587

# Only species names
data %>% 
  drop_na_(., vars = c("verbatim_scientific_binomial")) %>% 
  group_by(database_source) %>% 
  summarise(n())
# database_source   `n()`
# 1 ATLANTIC          99056
# 2 GBIF            2074518
# 3 ICMBIO           247393
# 4 NEOTROPTREE      698445
# 5 SPECIESLINK     5550717
# 6 ZWIENER          275462

# Verified species names 
data %>% 
  drop_na_(., vars = c("verified_species_binomial")) %>% 
  group_by(database_source) %>% 
  summarise(n())
# database_source   `n()`
# 1 BIEN            2778479
# 2 DRYFLOR          184279

# As BIEN and DRYFLOR are checked against authoritative sources we choose to use this information in subsequent analysis.

posi_b <- which(data$database_source == "BIEN")
data$verbatim_scientific_binomial[posi_b] <- data$verified_species_binomial[posi_b]

posi_d <- which(data$database_source == "DRYFLOR")
data$verbatim_scientific_name_author[posi_d] <- data$verified_species_binomial[posi_d]

# Note that DRYFLOR, IDIGBIO and SIBBR databases only containing information on species names along with author names (there are exception, some records do not contain authors names). On the other hand, ATLANTIC, BIEN, NEOTROPTREE, SPECIESLINK and ZWIENER databases only containg information on species names. ICMBIO and GBIF containg both kind of taxonomic information (with and without authors names).

# In the GBIF database, an incomplete species names (i.g, missing ephitet) is considered as NA in the column "verbatim_scientific_binomial" and the column "verbatim_scientic_name_author" contains a taxonomic but incomplete information (see below). We used the "verbatim_scientic_name_author" in the subsequent analysis because it contains names verified by GBIF blackbone taxonomy.

# In the ICMBIO, in turn, the column "verbatim_scientific_binomial" sometimes contains incomplete taxonomic information compared to the "verbatim_scientic_name_author" column. We used the "verbatim_scientic_name_author" in the subsequent analyses.

i <- data %>% filter(database_source == "ICMBIO")
i[1540:1550, 5:7]

# longitude verbatim_scientific_bi… verbatim_scientific_name_author 
# 1     -51.8 Sloanea                 Sloanea eichleri K.Schum.       
# 2     -57.6 Ebenaceae               EBENACEAE                       
# 3     -51.2 Sloanea                 Sloanea eichleri K.Schum.       
# 4     -42.5 Sloanea                 Sloanea garckeana K.Schum.      
# 5     -51.8 Sloanea                 Sloanea garckeana K.Schum.      
# 6     -69.4 Ebenaceae               EBENACEAE                       
# 7     -52.3 Sloanea                 Sloanea eichleri K.Schum.       
# 8     -51.9 Sloanea                 Sloanea garckeana K.Schum.      
# 9     -70   Sloanea                 Sloanea garckeana K.Schum.      
# 10     -41.9 Gaylussacia incana      Gaylussacia incana Cham.        
# 11     -41.3 Ericaceae               ERICACEAE Gaylussacia cf. harle…

rm(i, posi_b, posi_d)

# Databases containing species names along with author information
a <- data %>% select(verbatim_scientific_name_author,
                     database_source,
                     record_id) %>%
  rename(species_names = verbatim_scientific_name_author) %>%
  filter(database_source %in% c("DRYFLOR",
                                "ICMBIO",
                                "IDIGBIO",
                                "SIBBR",
                                "GBIF"))

# Create an unique id based on species names
a2 <- transform(a, idUni = match(species_names, unique(species_names)))

# unique names
a_unique <- distinct(a2, species_names, .keep_all = TRUE) # 497299








# Remove author names from species names ----------------------------------

# Remove author names (using the "remove.authors" fuction form the package flora). It is an important step for parsing names in the get.flora function because it increases the chance of finding a matching name

future::plan(multiprocess) # plan (parallel)
start_time <- Sys.time()
n <- a_unique$species_names

res <- furrr::future_map_chr(.x = n,
                             .f = remove.authors, # from flora package
                             .progress = TRUE)
end_time <- Sys.time()
end_time - start_time

a_unique$species_namesNOauthors <- res
a_unique <- a_unique %>% select(idUni, species_namesNOauthors)
a3 <- left_join(a2, a_unique, by = "idUni")

# Save the file
fwrite(a3 %>% select(species_namesNOauthors),
       file = 'output/results/vector_removeAuthorNames.csv')

a3[1:5,c(1,5)]
# species_names                                species_namesNOauthors
# 1 Piper aduncum L.                           Piper aduncum
# 2 Andira anthelmia (Vell.) Benth.            Andira anthelmia
# 3 Gustavia augusta L.                        Gustavia augusta
# 4 Himatanthus bracteatus (A.DC.) Woodson     Himatanthus bracteatus
# 5 Machaerium brasiliense Vogel               Machaerium brasiliense


# Substitute species names containing author names by names without authors names
a$species_names <- a3$species_namesNOauthors


# Databases containg species names without author information
b <- data %>% select(verbatim_scientific_binomial,
                     database_source,
                     record_id) %>%
  rename(species_names = verbatim_scientific_binomial) %>%
  filter(database_source %in% c("ATLANTIC",
                                "BIEN",
                                "NEOTROPTREE",
                                "SPECIESLINK",
                                "ZWIENER")) 

# No missing information? Yeah!
nrow(a) + nrow(b) == nrow(data)

# Create a dataframe with information on species names and database source which will be used for parsing species names
d <- bind_rows(a,b)
colnames(d)[1] <- "verbatim.scientific.name"
d <- d %>% dplyr::select(verbatim.scientific.name, record_id)

# Add the new species names column without author names
data <- left_join(d, data, by = "record_id")

# Remove unnecessary columns
data <- data %>% dplyr::select(-c(verbatim_scientific_binomial,
                                  verbatim_scientific_name_author))

# Save the table with standardized column of species names
fwrite(data,
       file = 'data/clean/02_preFilter/data.csv')

rm(a,a_unique, a2, a3, b, d, n, res, end_time, start_time)







# Flag records without geographic coordinates ---------------------------

# Read the file (21732211 records | 8 columns)
data_01 <- vroom('data/clean/02_preFilter/data.csv',
                 delim = ",",
                 locale = locale(encoding = "UTF-8")) 

# Pallete
pal <- viridisLite::viridis(10)

# Test the valid of the geographic coordinates using cc_val from the package CoordinateCleaner. Non-numeric, not available and non existent coordinates (e.g., lat >90, la <-90) are flagged and removed. 

valid_coords <- data_01 %>% cc_val(
  .,
  lon = "longitude",
  lat = "latitude",
  verbose = T,
  value = "flagged"
)

data_01$.coordNotNA <- valid_coords

# Number of records with invalid coordinates per database (n = 5473303)
data_01 %>% 
  dplyr::select(database_source, .coordNotNA) %>%
  filter(.coordNotNA == FALSE) %>% 
  group_by(database_source) %>% 
  summarise(total = n()) 
# database_source   total
# 1 ATLANTIC              2
# 2 BIEN             248594
# 3 ICMBIO              236
# 4 IDIGBIO         3636166
# 5 SIBBR           1576865
# 6 SPECIESLINK       11440


# Figure: Number of records missing geographic information per database
data_01 %>% select(database_source, .coordNotNA) %>%
  filter(.coordNotNA == FALSE) %>% 
  group_by(database_source) %>% 
  summarise(total = n()) %>% 
  rename(database_source = database_source, n = total) %>% 
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100) %>% 
  ggplot(aes(
    x = reorder(database_source, prop),
    y = prop,
    fill = as.factor(database_source))) +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Database",
       y = "Proportion of records") +
  scale_fill_manual(values = pal)


# save the figure
ggsave("output/figures/f01_RecordsWithoutCoordinates.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f01_RecordsWithoutCoordinates.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)


# Flagged and removed 5473303 with invalid latidue or longitude (25%)
data_01 %>% filter(.coordNotNA == FALSE) %>% summarise(n())

# Select only records with valid coordinates (16258908 records | 9)
data_01 <- data_01 %>% filter(.coordNotNA == TRUE) 







# Remove records outside Brazil -------------------------------------------

# Map of Brazil
m <- rworldmap::getMap() # rworldmap
brazil <- m[which(m$NAME == "Brazil"), ]
brazil@data <- brazil@data %>% dplyr::select(ADMIN)
brazil@data$fill <- 1

# Flag records outside Brazil (i.e., in other countries or in the sea)
OCC <- data_01 %>% dplyr::select(longitude, latitude)
selec_points <- raster::extract(brazil, OCC, buffer = 2000)
data_01$.inBrazil <- selec_points$fill

# 3973373 records outside Brazil flagged
sum(is.na(data_01$.inBrazil))

# Number of records outside Brazil per database
data_01 %>% 
  dplyr::select(database_source, .inBrazil) %>%
  filter(is.na(.inBrazil)) %>% 
  group_by(database_source) %>% 
  summarise(total = n()) 
# database_source     total
# 1 ATLANTIC           1415
# 2 BIEN              36894
# 3 DRYFLOR             643
# 4 GBIF              37133
# 5 ICMBIO             9995
# 6 IDIGBIO           75175
# 7 NEOTROPTREE       15736
# 8 SIBBR            408104
# 9 SPECIESLINK     3384161
# 10 ZWIENER           4117


# Figure: Points outside Brazil
outBrazil <- data_01 %>% filter(is.na(.inBrazil))
ggplot()+
  geom_polygon(
    data = m, 
    aes(x = long, y = lat, group = group),
    fill = "gray70"
    # colour = "black"
  ) +
  geom_hex(data = outBrazil, 
           aes(x = longitude, y = latitude),
           pch = 19,
           colour = "blue",
           size = 0.1,
           bins = 150) +
  geom_polygon(
    data = brazil, 
    aes(x = long, y = lat, group = group),
    fill = "gray60"
    # colour = "black"
  ) +
  coord_equal() +
  theme_void() +
  labs(fill = "# of Records") +
  scale_fill_viridis_c()

outBrazil
ggsave("output/figures/f02_pointsOutsideBrazil.tiff", units = "cm", width = 16, height = 10, dpi = 400)

ggsave("output/figures/f02_pointsOutsideBrazil.eps", units = "cm", width = 16, height = 10, dpi = 400)


# Figure: Number of records outside Brazil per database
outBrazil %>% dplyr::select(database_source, .inBrazil) %>%
  group_by(database_source) %>% 
  summarise(n = n()) %>% 
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100) %>% 
  ggplot(aes(
    x = reorder(database_source, prop),
    y = prop,
    fill = as.factor(database_source))) +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Database",
       y = "Proportion of records") + 
  scale_fill_manual(values = pal)

# save the figure
ggsave("output/figures/f02_RecordsOutsideBrazil.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f02_RecordsOutsideBrazil.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

# Select only records occurring in Brazil (12285535 | 10)
data_01 <- data_01 %>% filter(.inBrazil == 1) 







# Flag records without any taxonomic information --------------------------
data_01$.nameNotNA <-  is.na(data_01$verbatim.scientific.name)
data_01$.nameNotNA  <- ifelse(data_01$.nameNotNA == TRUE, FALSE, TRUE)


# Flagged 696796 records without taxonomic information
data_01 %>% filter(.nameNotNA == FALSE) %>% summarise(n())

# Number of records missing taxonomic information
data_01 %>% 
  select(database_source, .nameNotNA) %>%
  filter(.nameNotNA == FALSE) %>% 
  group_by(database_source) %>% 
  summarise(n()) 
# database_source  `n()`
# 1 BIEN            509214
# 2 IDIGBIO          69270
# 3 SIBBR              754
# 4 SPECIESLINK     117558 


# Figure: Number of records lacking taxonomic information per database
data_01 %>% dplyr::select(database_source, .nameNotNA) %>%
  filter(.nameNotNA == FALSE) %>%
  group_by(database_source) %>% 
  summarise(n = n()) %>% 
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100) %>% 
  ggplot(aes(
    x = reorder(database_source, prop),
    y = prop,
    fill = as.factor(database_source))) +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Database", y = "Proportion of records") + 
  scale_fill_manual(values = pal)


# save the figure
ggsave("output/figures/f03_RecordsWithoutNames.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f03_RecordsWithoutNames.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

# Select only record with species names (11588739 | 11)
data_01 <- data_01 %>% filter(.nameNotNA == TRUE)







# Remove fossil records and those with doubtful provenance ----------------

# Varying data sources
DT::datatable(as.data.frame(table(data_01$basisOfRecord)))

data_01 <- data_01 %>%
  mutate(.provenance = if_else(basisOfRecord %in% 
                                 c("Amost", "DrawingOrPhotograph" ,
                                   "Dupli", "EX" , "E", "Extra",
                                   "F", "FOSSIL_SPECIMEN",   
                                   "FossilSpecimen", "HS", "HUCP",
                                   "MACHINE_OBSERVATION",
                                   "MachineObservation", "MultimediaObject",
                                   "QQQQQ", "REPET", "RON", "V",  
                                   "X", "XS"), FALSE, TRUE)) 

# Number of records with doubtful provenance per database
data_01 %>% 
  filter(.provenance == FALSE) %>% 
  group_by(database_source) %>% 
  summarise(n()) 
# database_source `n()`
# <chr>           <int>
# 1 GBIF              578
# 2 IDIGBIO           53
# 3 SPECIESLINK       3413


# Figure: Number of records with doubtly provinance per database
data_01 %>% dplyr::select(database_source, .provenance) %>%
  filter(.provenance == FALSE) %>% 
  group_by(database_source) %>% 
  summarise(n = n()) %>% 
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100) %>% 
  ggplot(aes(
    x = reorder(database_source, prop),
    y = prop,
    fill = as.factor(database_source))) +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Database", 
       y = "Proportion of records") + 
  scale_fill_manual(values = pal)

# save the figure
ggsave("output/figures/f04_doubtProvinance.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f04_doubtProvinance.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

# Flaggegd 4004 records with doubtly provinance
data_01 <- data_01 %>% filter(.provenance == TRUE)

# Remove unnecessary columns
data_01 <- data_01 %>% select(-c(.coordNotNA, .inBrazil, .nameNotNA, .provenance, verified_species_binomial))

# Save the table of the pre-filter step (11588739 | 12)
fwrite(data_01,
       file = 'data/clean/02_preFilter/data_01.csv')






# Figure: Pre Filter (remove missing coordinates and names, records outside Brazil, and records with doubtful provenance)
df_issues <- read_csv2("output/figures/df_issues.csv")
df_issues <- df_issues[,1:3]
colnames(df_issues) <- c("Database", "Issue", "n")

records_database <- vroom("output/results/records_database.csv")
colnames(records_database) <- c("Database", "n")

pal_s <- viridisLite::viridis(30)
pal_sel <- c("#481B6DFF", "#46307EFF", "#404688FF", "#38598CFF")

df_issues %>%
  group_by(Database) %>%
  # count(sort = T) %>%
  left_join(., records_database, by = "Database") %>%
  mutate(., prop = (n.x / n.y) * 100) %>% 
  ggplot(aes(
    x =  reorder(Database, prop),
    y = prop,
    fill = as.factor(Issue))) +
  geom_col() +
  coord_flip() +
  bbc_style() + 
  theme(axis.title = element_text(size = 24),
        legend.position = "top",
        legend.text = element_text(size = 11)) +
  labs(x = "Database", y = "Proportion of records") +
  scale_fill_viridis_d(direction = -1)

# save the figure
ggsave("output/figures/f05_preFilter.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f05_preFilter.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)







# Taxonomic checking - parsing species names --------------------------------

# We used the flora package for checking species names. This packgaes compares species names against those present in the oficil list of the Brazilian Flora.

# Package "flora"

# Advantages: 
# Uptade regurlarly 
# Ease to use

# Disadvantage: 
# Not provided matching confidence index
# Links:
# https://cran.r-project.org/web/packages/flora/flora.pdf
# https://github.com/gustavobio/flora
# https://medium.com/@ghcarvalho/como-corrigir-nomes-e-buscar-informa%C3%A7%C3%B5es-taxon%C3%B4micas-de-plantas-no-r-cdd019d5de8f


# Read the file (11584695 | 7)
data_01 <- vroom('data/clean/02_preFilter/data_01.csv',
                 delim = ",",
                 locale = locale(encoding = "UTF-8"))


records_database <- data_01 %>%
  group_by(database_source) %>%
  count(sort = T) %>% 
  arrange(., database_source)
fwrite(records_database, "output/results/records_database.csv")
# 1 ATLANTIC          97639
# 2 BIEN            2531216
# 3 DRYFLOR          183636
# 4 GBIF            2037308
# 5 ICMBIO           237162
# 6 IDIGBIO         2173321
# 7 NEOTROPTREE      682709
# 8 SIBBR           1087653
# 9 SPECIESLINK     2282706
# 10 ZWIENER          271345

# Pallete
pal <- viridisLite::viridis(10)




# Quantify taxonomic uncertainty ------------------------------------------

# Useful for identifying and removing terms denoting taxonomic uncertainty (cf, sp, aff, and many more)

# Note that:
# \\s = space
# \\. = end point
# | = or
# $ = search at the end of a string

# Function for quantifying taxonomic uncertainty
taxo_unc <- function(spp_names)
{
  # confer
  cf0 <- str_detect(spp_names,
                    regex("^(cf\\.|cf\\s|cf$)", 
                          ignore_case = TRUE)) # at the beginning
  
  cf <- str_detect(spp_names,
                   regex("\\scf\\.|\\scf\\s|\\scf$", 
                         ignore_case = TRUE)) # anywhere
  
  # affinis
  aff0 <- str_detect(spp_names,
                     regex("^(aff\\.|aff\\s|aff$)", 
                           ignore_case = TRUE))
  
  aff <- str_detect(spp_names,
                    regex("\\saff\\.|\\saff\\s|\\saff$",
                          ignore_case = TRUE))
  
  # complex
  complex0 <- str_detect(spp_names,
                         regex("^(complex\\s|complexo|complex$)",
                               ignore_case = TRUE))
  
  complex <- str_detect(spp_names,
                        regex("\\scomplex\\s|\\scomplexo|\\scomplex$", 
                              ignore_case = TRUE))
  
  # genus novum | genus species
  gen0 <- str_detect(spp_names,
                     regex("^(gen\\.|gen\\s|gen$)", 
                           ignore_case = TRUE))
  
  gen <- str_detect(spp_names,
                    regex("\\sgen\\.|\\sgen\\s|\\sgen$", 
                          ignore_case = TRUE))
  

  # species | species (plural)
  sp0 <- str_detect(spp_names,
                    regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$)",
                          ignore_case = TRUE))
  
  sp <- str_detect(spp_names,
                   regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$",
                         ignore_case = TRUE))
  
  # species incerta
  sp_inc0 <- str_detect(spp_names,
                        regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)", 
                              ignore_case = TRUE))
  
  sp_inc <- str_detect(spp_names,
                       regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
                             ignore_case = TRUE))
  
  # species inquirenda
  sp_inq0 <- str_detect(spp_names,
                        regex("^(inq\\.|inq\\s|inq$)", 
                              ignore_case = TRUE))
  
  sp_inq <- str_detect(spp_names,
                       regex("\\sinq\\.|\\sinq\\s|\\sinq$",
                             ignore_case = TRUE))
  
  # species indeterminabilis
  sp_indet0 <- str_detect(
    spp_names,
    regex(
      "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
      ignore_case = TRUE))
  
  sp_indet <- str_detect(
    spp_names,
    regex(
      "\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
      ignore_case = TRUE))
  
  # species nova
  sp_nova0 <- str_detect(spp_names,
                         regex("^(nov\\.|nov\\s|nov$)",
                               ignore_case = TRUE))
  
  sp_nova <- str_detect(spp_names,
                        regex("\\snov\\.|\\snov\\s|\\snov$",
                              ignore_case = TRUE))
  
  # species proxima
  sp_proxima0 <- str_detect(spp_names,
                            regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
                                  ignore_case = TRUE))
  
  sp_proxima <- str_detect(spp_names,
                           regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
                                 ignore_case = TRUE))
  
  # stetit
  stet0 <- str_detect(spp_names,
                      regex("^(stet\\.|stet\\s|stet$)", 
                            ignore_case = TRUE))
  
  stet <- str_detect(spp_names,
                     regex("\\sstet\\.|\\sstet\\s|\\sstet$",
                           ignore_case = TRUE))
  
  terms <- 
    c('cf0', 'cf', 'aff0', 'aff', 'complex0', 'complex', 'gen0', 'gen',
      'sp0', 'sp', 'sp_inc0', 'sp_inc', 'sp_inq0',
      'sp_inq', 'sp_indet0', 'sp_indet', 'sp_nova0', 'sp_nova',
      'sp_proxima0', 'sp_proxima', 'stet0', 'stet')
  
  terms_names <- 
    c('confer', 'confer', 'affinis', 'affinis', 'complex',  'complex', 
      'genus novum or genus species', 'genus novum or genus species',
      'species', 'species', 'species incerta', 
      'species incerta', 'species inquirenda', 'species inquirenda',
      'species indeterminabilis', 'species indeterminabilis', 
      'species nova', 'species nova', 'species proxima', 'species proxima',
      'stetit', 'stetit')
  
  term_uncertainty <- rep(NA, length(spp_names))
  taxo_uncertainty <- rep(FALSE, length(spp_names))
  
  for (i in 1:length(terms)){
    t <- get(terms[i])
    posi <- which(t == TRUE)
    term_uncertainty[posi] <- terms_names[i]
    taxo_uncertainty[posi] <- TRUE
  } 
  
  taxo_uncertainty <- ifelse(taxo_uncertainty == TRUE, FALSE, TRUE)
  tab_res <- as.data.frame(cbind(term_uncertainty, taxo_uncertainty))
  return(tab_res)
}

taxonomic_uncertainty <- taxo_unc(spp_names = data_01$verbatim.scientific.name)
colnames(taxonomic_uncertainty) <- c(".term_uncertainty",
                                     ".taxonomic_uncertainty")

data_01 <- cbind(data_01, taxonomic_uncertainty)


# # Figure 06: Number of records with containing terms denoting taxonomic uncertainty

# Number of records per database
records_database <- vroom("output/results/records_database.csv")

# Figure: Proportion of records per database containing terms denoting taxonomic uncertainty
data_01 %>%
  filter(.taxonomic_uncertainty == FALSE) %>%
  group_by(database_source) %>%
  count(sort = T) %>%
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100) %>%
  ggplot(aes(
    x = reorder(database_source, prop),
    y = prop,
    fill = as.factor(prop)
  )) +
  theme_minimal() +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Database", y = "% of records") +
  scale_fill_manual(values = pal)

# save the figure
ggsave("output/figures/f06_taxonomicUncertainty.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f06_taxonomicUncertainty.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

# Number of taxonomic uncertainty terms per database
data_01 %>% 
  filter(.taxonomic_uncertainty == FALSE) %>%
  group_by(database_source) %>%
  count(sort = T) %>%
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100)
# 1 SPECIESLINK     46761 2282706 2.05   
# 2 IDIGBIO         40995 2173321 1.89   
# 3 SIBBR            9173 1087653 0.843  
# 4 ICMBIO           3075  237162 1.30   
# 5 DRYFLOR            52  183636 0.0283 
# 6 NEOTROPTREE        16  682709 0.00234


# Figure: Most frequenty qualifiers (terms) of taxonomic uncertainty
data_01  %>%
  filter(!is.na(.term_uncertainty)) %>% 
  group_by(.term_uncertainty) %>% 
  count(sort = T) %>%
  ggplot(aes(x = reorder(.term_uncertainty, n), y = n, fill = as.factor(n))) +
  theme_minimal() +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Terms", y = "Number of records") +
  scale_fill_manual(values = pal)

# save the figure
ggsave("output/figures/f07_terms_uncertainty.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f07_terms_uncertainty.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

data_01  %>%
  filter(!is.na(.term_uncertainty)) %>% 
  group_by(.term_uncertainty) %>% 
  count(sort = T)
# .term_uncertainty            n
# <fct>                    <int>
# 1 species                  71151
# 2 confer                   21261
# 3 affinis                   4716
# 4 species indeterminabilis  2632
# 5 species incerta            109
# 6 complex                    108
# 7 species nova                95







# Select unique species' names --------------------------------------------

# Add a id column indicating duplicated species names  In other words, duplicated names will receive a same id. This column it is important for saving time when quering species names against authoritative taxonomic sources (taxonomic checking bellow).

data_01 <- transform(data_01, 
                     .idUnique = match(verbatim.scientific.name, 
                                       unique(verbatim.scientific.name)))
data_01[80:90, c(1,10)]
# verbatim.scientific.name        .idUnique
# 80 Handroanthus impetiginosus        80
# 81           Couepia impressa        81
# 82     Cupania impressinervia        82
# 83        Diplotropis incexis        83
# 84       Diospyros inconstans        84
# 85          Samanea inopinata        85
# 86      Acrocomia intumescens        86
# 87             Myrcia isaiana        87
# 88          Licania kunthiana        88
# 89        Pradosia lactescens        89
# 90       Posoqueria latifolia        90

# a vector with unique species names
spp_names <- unique(data_01$verbatim.scientific.name) # 179106


# Below you find three functions usefull for cleaning taxonomic names. Specifically, these functions are usufell for:
# 1) remove taxonomic uncertainty terms (e.g., cf, sp, affins), 
# 2) remove family names present along with species names, 
# 3) remove other issues such as alphanumeric characters, numbers, names wit more than 3 words containing the same generic and specific name, extra spaces, and procedures for standardizing species names (i.e Capitalize only the generic name of each string)


# Remove taxonomic uncertainty terms --------------------------------------
# This step increase the odds of the function get.taxa and tnrs match taxonomicnames

# Not identifical to the function "taxo_unc" because some terms (var, subspecies, etc) are also removed.
remove_taxoUnc <- function(spp_names)
{
  # confer
  spp_names_clean <- str_replace_all(
    spp_names,
    regex("^(cf\\.|cf\\s|cf$)",
          ignore_case = TRUE),
    replacement = " ") # at the beginning
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\scf\\.|\\scf\\s|\\scf$",
          ignore_case = TRUE),
    replacement = " ") # anywhere
  
  # affinis
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(aff\\.|aff\\s|aff$)",
          ignore_case = TRUE),
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\saff\\.|\\saff\\s|\\saff$",
          ignore_case = TRUE),
    replacement = " ")
  
  # complex
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(complex\\s|complexo|complex$)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\scomplex\\s|\\scomplexo|\\scomplex$",
          ignore_case = TRUE),
    replacement = " ")
  
  # genus novum | genus species
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(gen\\.|gen\\s|gen$)",
          ignore_case = TRUE),
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sgen\\.|\\sgen\\s|\\sgen$",
          ignore_case = TRUE), 
    replacement = " ")
  
  # species | species (plural)
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(sp\\.|sp\\s|\\ssp$|spp\\.|spp\\s|\\sspp$|ssp\\.|ssp\\s|ssp$)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\ssp\\.|\\ssp\\s|\\ssp$|\\sspp\\.|\\sspp\\s|\\sspp$|\\sssp\\.|\\sssp\\s|\\sssp$",
          ignore_case = TRUE),
    replacement = " ")
  
  # species incerta
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(inc\\.|inc\\s|inc$|\\?\\s|\\?)",
          ignore_case = TRUE),
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sinc\\.|\\sinc\\s|\\sinc$|\\s\\?\\s|\\?",
          ignore_case = TRUE), 
    replacement = " ")
  
  # species inquirenda
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(inq\\.|inq\\s|inq$)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sinq\\.|\\sinq\\s|\\sinq$",
          ignore_case = TRUE),
    replacement = " ")
  
  # species indeterminabilis
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex(
      "^(indet\\.|indet\\s|indet$|ind\\.|ind\\s|ind$|indt\\.|indt\\s|indt$)",
      ignore_case = TRUE),
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sindet\\.|\\sindet\\s|\\sindet$|\\sind\\.|\\sind\\s|\\sind$|\\sindt\\.|\\sindt\\s|\\sindt$",
          ignore_case = TRUE), 
    replacement = " ")
  
  # species nova
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(nov\\.|nov\\s|nov$)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\snov\\.|\\snov\\s|\\snov$",
          ignore_case = TRUE),
    replacement = " ")
  
  # species proxima
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(prox\\.|prox\\s|prox$|nr\\.|nr\\s|nr$)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sprox\\.|\\sprox\\s|\\sprox$|\\snr\\.|\\snr\\s|\\snr$",
          ignore_case = TRUE), 
    replacement = " ")
  
  # stetit
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(stet\\.|stet\\s|stet$)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sstet\\.|\\sstet\\s|\\sstet$",
          ignore_case = TRUE), 
    replacement = " ")
  
  # hybrids 
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(x\\s)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sx\\s|\\sx$",
          ignore_case = TRUE), 
    replacement = " ")
  
  # forma
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sf\\.|\\sf\\s|\\f$|\\sfo\\.|\\fo\\s",
          ignore_case = TRUE), 
    replacement = " ")
  
  # Non-available (NA)
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\sna\\.|\\sna\\s|\\sna$",
          ignore_case = TRUE), 
    replacement = " ")
  
  # sem
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("^(sem\\s|sem\\.)",
          ignore_case = TRUE), 
    replacement = " ")
  
  spp_names_clean <- str_replace_all(
    spp_names_clean,
    regex("\\ssem\\.|\\ssem\\s|\\ssem$",
          ignore_case = TRUE), 
    replacement = " ")
  
  
  # Remove extra spaces
  spp_names_clean <- str_squish(spp_names_clean)
  return(spp_names_clean)
}

spp_names2 <- remove_taxoUnc(spp_names)
spp_names[65227:65233]
# [1] "RUBIACEAE Psychotria cf. carthagenensis"
# [2] "Zornia"                                 
# [3] "Abrus"                                  
# [4] "Limnosipanea"                           
# [5] "Deguelia"                               
# [6] "LEGUMINOSAE Andira cf. ormosioides"     
# [7] "LEGUMINOSAE Senna cf. silvestris"  

# And after use the function "remove_taxoUnc"
spp_names2[65227:65233]
# 1] "RUBIACEAE Psychotria carthagenensis"
# [2] "Zornia"                             
# [3] "Abrus"                              
# [4] "Limnosipanea"                       
# [5] "Deguelia"                           
# [6] "LEGUMINOSAE Andira ormosioides"     
# [7] "LEGUMINOSAE Senna silvestris"    


# Remove family names -----------------------------------------------------
remove_familyNames <- function(sp_names) {
  
    df <- data.frame(str_count(sp_names, "\\S+"), sp_names)
    n_string <- ifelse(df[, 1] < 2, FALSE, TRUE)
    n_string <- ifelse(is.na(n_string), FALSE, n_string)
    
    w_rem <- which(n_string == TRUE)
    
    rem_fam <- str_replace_all(
    sp_names[w_rem],
    regex("^[a-zA-Z]+aceae|Leguminosae|Compositae",
          ignore_case = TRUE),
    replacement = " "
  )
  
  rem_fam <- str_squish(rem_fam)
  sp_names[w_rem] <- rem_fam
  return(sp_names)
}

spp_names3 <- remove_familyNames(spp_names2)
spp_names3[65227:65233]
# [1] "Psychotria carthagenensis"
# [2] "Zornia"                   
# [3] "Abrus"                    
# [4] "Limnosipanea"             
# [5] "Deguelia"                 
# [6] "Andira ormosioides"       
# [7] "Senna silvestris"   

# Quantify the number of records containing family names along species names per database
i <- as.integer(1:length(spp_names)) # UdUnique
t <- data.frame(i, spp_names3)
colnames(t)[1] <- ".idUnique"
family_name <- spp_names3 == spp_names2 

# records containing family names (== TRUE)
family_name <- ifelse(family_name == FALSE, FALSE, TRUE)
t <- data.frame(t, family_name)
t2 <- t %>% dplyr::select(.idUnique, family_name)
colnames(t2)[2] <- ".family_name"

table(t2$.family_name)
# FALSE   TRUE 
# 54606 124500 

family_name <- data_01 %>% 
  dplyr::select(database_source, .idUnique, verbatim.scientific.name)
family_name <- left_join(family_name, t2, by = ".idUnique")

# 677224 records containg family names along with species names
table(family_name$.family_name) 
# FALSE     TRUE 
# 617964 10966731 

family_name %>%
  filter(.family_name == FALSE) %>%
  group_by(database_source) %>%
  count(sort = T) 
# database_source      n
# 1 SIBBR           613246
# 2 ICMBIO            3029
# 3 IDIGBIO           1499
# 4 SPECIESLINK        190
rm(i,t,t2)

# Remove other issues -----------------------------------------------------
remove_otherIssues <- function(spp_names)
{
  rem_dup_names <- function(x) {
    res <- x
    word_count <- wc(x)
    w3 <- which(word_count >= 3)
    
    for (i in w3)
    {
      sp <- x[i]
      t0 <- tolower(sp)
      t <- trimws(t0)
      u <- unlist(
        strsplit(t, split = " ", fixed = F, perl = T))
      dup <- paste(unique(c(u[1], u[2])), sep = " ", collapse = " ")
      remain <- paste(u[3:length(u)], sep = " ", collapse = " ")
      p <- paste(dup, remain)
      res[i] <- p
    }
    return(res)
  }
  remDup <- rem_dup_names(spp_names)
  v <- str_replace_all(remDup, "[^[:alnum:]]", " ")
  v1 <- str_replace_all(v, "[0-9]+", " ")
  v2 <- str_squish(v1)
  v3 <- gsub("^$", NA, v2) # substitue empty records by NA
  v4 <- Hmisc::capitalize(v3)
  return(v4)
}

spp_names4 <- remove_otherIssues(spp_names3)

summary(is.na(spp_names4)) # 52 records NA (out 179106)
sum((is.na(spp_names4)))/length(spp_names4) * 100 #0.02%

# Quantify the number of records containg "other issues"
i <- as.integer(1:length(spp_names)) # UdUnique
t <- data.frame(i, spp_names4)
colnames(t) <- c(".idUnique", "verbatim.scientific.name")
other_issues <- spp_names4 == spp_names3 

# records containing "other issues" (== TRUE)
other_issues <- ifelse(other_issues == FALSE, FALSE, TRUE)
t <- data.frame(t, other_issues)
t2 <- t %>% dplyr::select(.idUnique, other_issues)
colnames(t2)[2] <- ".other_issues"

table(t2$.other_issues)
# FALSE   TRUE 
# 29954 149100

other_issues <- data_01 %>% 
  dplyr::select(database_source, .idUnique, verbatim.scientific.name)
other_issues <- left_join(other_issues, t2, by = ".idUnique")

table(other_issues$.other_issues) 
# FALSE     TRUE 
# 327861 11250424 

other_issues %>%
  filter(.other_issues == FALSE) %>%
  group_by(database_source) %>%
  count(sort = T) 
# database_source      n
# 1 GBIF            113006
# 2 SIBBR            81259
# 3 IDIGBIO          62027
# 4 SPECIESLINK      56219
# 5 BIEN              6971
# 6 ICMBIO            4005
# 7 NEOTROPTREE       2309
# 8 DRYFLOR           1042
# 9 ATLANTIC           616
# 10 ZWIENER           407

# Most records flagged as "other issues" contains punctuations signs (".", "-")
# and do not represent "errors". Instead, removing these issues increase the odds of find a matching name.

spp <- t %>% dplyr::select(-other_issues)
colnames(spp)[2] <- "scientific.name" 
rm(i,t, t2)







# To run the taxonomic checking in parallel we create a function based on the get.taxa function of the flora package 

try_flora <- function(list_sp, replace.synonyms, suggest.names,
                      suggestion.distance, establishment, states)
{
  res <- get.taxa(
    list_sp,
    replace.synonyms = replace.synonyms,
    suggest.names = suggest.names,
    suggestion.distance = suggestion.distance,
    establishment = establishment,
    states = states
  )
  return(res)
}


# In "get.taxa" when parse=T, the function returns an error  when some names with specific structures are used. Examples of such errors include names with special characters ("GÃ Âªnero novo"), names starting with family names with some special character ("Leg.minosaceae"), and names with only a letter ("C" after removing family names "Leguminosae-c").

# Run try_flora in parallel
future::plan(multiprocess)
start_time <- Sys.time()

parse_sppNames <-
  furrr::future_map_dfr(
    .x = spp$scientific.name,
    .f = try_flora,
    replace.synonyms = TRUE,
    suggest.names = TRUE,
    suggestion.distance = 0.9,
    establishment = TRUE,
    states = TRUE,
    .progress = TRUE
  )
end_time <- Sys.time()
end_time - start_time

# Replace synonyms by accepted names (179106 | 12)
dim(parse_sppNames)
parse_sppNames %>% 
  filter(!is.na(scientific.name)) %>%
  distinct(scientific.name) %>% nrow() # 43609 unique species names

w3 <- which(!is.na(parse_sppNames$accepted.name)) # 556 names
parse_sppNames$scientific.name[w3] <- parse_sppNames$accepted.name[w3]
parse_sppNames %>% 
  filter(!is.na(scientific.name)) %>% 
  distinct(scientific.name) %>% nrow() # 43346

# Around 26.08% of the unique names were not found by get.taxa function (46718 out 179098)
sum((is.na(parse_sppNames$scientific.name))) / nrow(parse_sppNames) * 100 

# merge the tables
parse_sppNames <- bind_cols(spp %>% 
                              dplyr::select(.idUnique), parse_sppNames)

# Around 3.86% of records had names not found by get.taxa function (448283)
temp <- left_join(data_01, parse_sppNames, by=".idUnique") 
sum((is.na(temp$scientific.name))) / nrow(temp) * 100 

# Note that ".scientic_name" in the file "spp" is identical to the column "original.search" in the file "parse_sppNames". 

# # Save the table (11584695 | 22)
fwrite(parse_sppNames, "output/results/parse_sppNames.csv")
fwrite(temp, "data/clean/03_dataset_taxonomic_cleaning/data_01_temp.csv")







# Manual name checking ----------------------------------------------------

# We manually parse names seeking for removing author nomes, uncertainty terms (in Portuguese, e.g "indeterminado", sp, af) do not removed by our clean functions. We also corrected names with special characters whenever possible. Misspelled family names (not ending with aceae), taxonomic levels ("Plantae", "Bryophyta", "Alga", "Gymnosperma", "Genre"), indermined terms ("Cruz", "Jardim", "Ponta", "UnKnown", "NÃ o Identificada", Ïndeterminada") were also removed. 

# Read the files (179106 | 13)
parse_sppNames <- vroom::vroom(
  "output/results/parse_sppNames.csv", delim = ",",
  locale = locale(encoding = "UTF-8"))

manual_checking <- parse_sppNames %>%
  dplyr::select(.idUnique,
                scientific.name,
                original.search)

#132386 records removed out 179106  unique names
manual_checking <- manual_checking %>% 
  filter(is.na(scientific.name)) %>% 
  dplyr::select(-scientific.name)

# Save the table
fwrite(manual_checking, "output/tnrs/00_checking_names.csv")







# Check species names against other authoritative taxonomic sources -------

# We selected unmatched names and performed a query in the Taxonomic Names Resolution Service website (TNRS v4.0; http://tnrs.iplantcollaborative.org/index.html). Then, we used the name retrievied by TNRS website to check again species names against Brazilian flora using the flora package. For some reason, TNRS does not allow users download files containg more than ~20.000 names. Thus, we recomend split a large database in smaller ones before performing the query in the TNRS website.

# TNRS retrieves a collum name "Taxnomic status" that contains the taxonomic Status refers of the matched name. As recommendated by Boyled et al. 2013, we only used names with the status "accepted", "synonym" or "no opinion". Note when a no suitable matches are found, a empty field is retrieved. TNRS retrieves two columns of species names. By comparing them, we found that the collum "Name_matched" retrieved more matched (when using get.taxa function) names compared when using the collum scientific.name

# Also, we only selected records with matching score  equal or higher than 0.9. Since TNRS retrieves query names in four varyhing taxonomic sources, more than one matched names with score equal or higher than 0.9 can be retrieved. Overall, the same name is retrieved by the four alternative sources. But when it is not the case, we selected only the name with the highest lexical score (best spelling match).

# Load the table containing results of the manual checking process and the names retrieved by TNRS
updated_names <- vroom::vroom(
  "output/tnrs/update_names_5.csv", delim = ";",
  locale = locale(encoding = "UTF-8"))


# ".idUnique" = unique id that can be matched with the column ".idUnique" of the "data_01" and "spp"databases.
# "original.search": verbatim names presented in the databases
# "names_manually_corrected": similar to "original.search", but with some ortographical corrections.


# Compare the amount of changes made by the manual name checking procedure

# Amount the records that we manually removed. These records include non binomial names or names with special characters not ease to understand.
records_removed <- updated_names %>% 
  filter(is.na(names_manually_corrected))
head(records_removed) # 168 unique records NA

# Total number of records removed (252 records manually removed)
number_RR <- right_join(data_01, records_removed, by=".idUnique")

# Total number o records removed per database
number_RR  %>%
  group_by(database_source) %>%
  count(sort = T) 
# 1 SPECIESLINK      6693
# 2 IDIGBIO          5658
# 3 SIBBR            2048
# 4 ICMBIO           1249
# 5 DRYFLOR            13
# 6 GBIF                1

# Amount of records modified (e.g., orthograph modifications, remove author names or part of authors names not remove by our function, etc)
records_modified <- updated_names %>% 
  filter(!is.na(names_manually_corrected))

p <- which(records_modified$original.search...2 != 
             records_modified$names_manually_corrected)

records_modified <- records_modified [p, ] # 328 records unique records
records_modified[c(1,50,100), c(2, 5)]
# 1 Paralychnophora harleyi h rob Paralychnophora harleyi 
# 2 Hydrolithon samoÃ Â nse       Hydrolithon samoense    
# 3 Lapidia apicifolia s c        Lapidia apicifolia 


# Total number of records modified ( records manually modified)
number_RM <- right_join(data_01, records_modified, by=".idUnique")

number_RM  %>% # 11485 records
  group_by(database_source) %>%
  count(sort = T) 
# 1 GBIF             1324
# 2 SPECIESLINK       736
# 3 IDIGBIO           648
# 4 SIBBR             439
# 5 NEOTROPTREE       108
# 6 ICMBIO             81
# 7 BIEN               17
# 8 DRYFLOR             9

# Run try_flora using species names retrieved by tnrs
future::plan(multiprocess)
start_time <- Sys.time()

parse_sppNames2 <-
  furrr::future_map_dfr(
    .x = updated_names$Name_matched, # 39725
    .f = try_flora,
    replace.synonyms = TRUE,
    suggest.names = TRUE,
    suggestion.distance = 0.9,
    establishment =TRUE,
    states = TRUE,
    .progress = TRUE
  )
end_time <- Sys.time()
end_time - start_time


# Replace synonyms by accepted names
dim(parse_sppNames2) # 44628 | 12
parse_sppNames2 %>% 
  filter(!is.na(scientific.name)) %>%
  distinct(scientific.name) %>% nrow() # 3567 unique species

w4 <- which(!is.na(parse_sppNames2$accepted.name)) 
parse_sppNames2$scientific.name[w4] <- parse_sppNames2$accepted.name[w4]
parse_sppNames2 %>% 
  filter(!is.na(scientific.name)) %>% 
  distinct(scientific.name) %>% nrow() # 3566 unique species


parse_sppNames2 <- cbind(updated_names %>%
                         dplyr::select(.idUnique),
                         parse_sppNames2) # 44628 records

# Around 2.38% of unique names were matched with Brazilian List after manually correcting species names and by using species names retrieved by TNRS
sum((!is.na(parse_sppNames2$scientific.name))) / nrow(parse_sppNames) * 100 

# 4263 unique names found by TNRS
parse_sppNames2 <- parse_sppNames2 %>% filter(!is.na(scientific.name))


# Merge the databases -----------------------------------------------------
# Database with unique scientific names
parse_sppNames[parse_sppNames2$.idUnique,] <- parse_sppNames2

w4 <- which(!is.na(parse_sppNames$accepted.name)) 
parse_sppNames$scientific.name[w4] <- parse_sppNames$accepted.name[w4]

parse_sppNames %>% 
  filter(!is.na(scientific.name)) %>% 
  distinct(scientific.name) %>% nrow() # 43394 unique species


# 76.3% of unique names were matched with names of the Brazilian List
sum((!is.na(parse_sppNames$scientific.name))) / nrow(parse_sppNames) * 100 

# Around 3.30% of records had names not found by get.taxa function
temp <- left_join(data_01, parse_sppNames, by=".idUnique") 
sum(!is.na(temp$scientific.name)) / nrow(temp) * 100 


# Merging with the whole database (11584695 | 22)
data_02 <- left_join(data_01, parse_sppNames, by= ".idUnique")
colnames(data_02) <-
  c(
    "verbatim.scientific.name",
    "record_id",
    "basisOfRecord",
    "database_source",
    "latitude",
    "longitude",
    "year",
    ".term_uncertainty",
    ".taxonomic_uncertainty",
    ".idUnique",
    ".id",
    ".scientific.name",
    ".accepted.name",
    ".family",
    ".taxon.rank" ,
    ".taxon.status",
    ".search.str",
    ".threat.status",
    ".notes",
    ".original.search",
    ".occurrence",
    ".establishment")

# Note that all columns incorportated during the taxonomic checking process start with "."

# Number of records per database
records_database <- vroom("output/results/records_database.csv")

# Figure: Proportion of records with valid species names (contrasted against to the Brazilian list)
f <- data_02  %>%
  filter(!is.na(.scientific.name)) %>%
  group_by(database_source) %>%
  count(sort = T) %>%
  # Total number of records per database
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100) %>%
  ggplot(aes(
    x = reorder(database_source, prop),
    y = prop,
    fill = as.factor(prop))) +
  theme_minimal() +
  geom_col() +
  coord_flip() +
  bbc_style() +
  theme(axis.title = element_text(size = 24),
        legend.position = "none") +
  labs(x = "Database", y = "% of records") +
  scale_fill_manual(values = pal)

f
ggsave("output/figures/f08_proportionValidNames.tiff", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f08_proportionValidNames.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

labelled.bars <- f +
  geom_label(aes(x = reorder(database_source, prop), y = prop, label = round(prop, 0)),
             hjust = 1, 
             vjust = 0.5, 
             colour = "white", 
             fill = NA, 
             label.size = NA, 
             family="Helvetica", 
             size = 6)

labelled.bars
ggsave("output/figures/f08_proportionValidNames2.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)

ggsave("output/figures/f08_proportionValidNames2.eps", dpi=300,
       width = 6, height = 3, units = "cm", scale = 4)


data_02  %>%
  filter(!is.na(.scientific.name)) %>%
  group_by(database_source) %>%
  count(sort = T) %>%
  # Total number of records per database
  left_join(., records_database, by = "database_source") %>%
  mutate(., prop = (n.x / n.y) * 100) 
# database_source     n.x     n.y  prop
# 1 BIEN            2490348 2531216  98.4
# 2 SPECIESLINK     2166201 2282706  94.9
# 3 IDIGBIO         2080500 2173321  95.7
# 4 GBIF            1971971 2037308  96.8
# 5 SIBBR           1034186 1087653  95.1
# 6 NEOTROPTREE      676962  682709  99.2
# 7 ZWIENER          270756  271345  99.8
# 8 ICMBIO           230413  237162  97.2
# 9 DRYFLOR          183042  183636  99.7
# 10 ATLANTIC          97031   97639  99.4

# Save the table with valid names
fwrite(data_02, "data/clean/03_dataset_taxonomic_cleaning/data_02_TAXONOMIC_CHECKING.csv")

# Names not found (383285 | 22)
data02_namesNotFound <- data_02 %>% filter(is.na(.scientific.name))
fwrite(data02_namesNotFound, "data/clean/03_dataset_taxonomic_cleaning/data_02_namesNotFound.csv")

# Unique names not found (42457 names out of 179106)
names_not_found <- parse_sppNames %>% filter(is.na(scientific.name))
names_not_found <- names_not_found %>% select(.idUnique, original.search)
write_csv2(names_not_found, "output/results/names_not_found.csv")

data02_namesNotFound %>% group_by(database_source) %>% count(sort = T)
# database_source      n
# 1 SPECIESLINK     116505
# 2 IDIGBIO          92821
# 3 GBIF             65337
# 4 SIBBR            53467
# 5 BIEN             40868
# 6 ICMBIO            6749
# 7 NEOTROPTREE       5747
# 8 ATLANTIC           608
# 9 DRYFLOR            594
# 10 ZWIENER           589
