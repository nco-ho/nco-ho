## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inlezen CBS Arbeidsmarkt.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: In dit script worden de Arbeidsmarkt variabelen van CBS ingelezen
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfCBS_NCO_HO_naming <- read_documentation("Documentatie_CBS_Arbeidsmarkt.csv")

files <- list.files(paste0(Sys.getenv("NETWORK_DIR"), "Datasets/CBS NCO HO/"), pattern = "export_230222_1_", full.names = TRUE)

dfCBS_NCO_HO <- purrr::map_dfr(files, read_delim, locale = locale(decimal_mark = ","))
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Up to date check
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## TODO: Voer up-to-date uit
up_to_date(
  bestandspad = files[1],
  frequentie = 365,
  correctie_tijdstip = "2023-02-22",
  contact = "Tom Stolp",
  inleesscript = "Inlezen CBS NCO HO.R"
)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
assert_naming(dfCBS_NCO_HO, dfCBS_NCO_HO_naming, "CBS_NCO_HO")
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pas kolomnamen aan met behulp van de documentatie
dfCBS_NCO_HO <- dfCBS_NCO_HO %>%
  wrapper_translate_colnames_documentation(dfCBS_NCO_HO_naming) %>%
  rowwise() %>%
  ## Get rid of all the -999 values
  mutate(across(where(is.numeric), ~ ifelse(. < 0, NA, .)))

## Splits df op in landelijk en regionaal (Amsterdam&Utrecht)
dfCBS_NCO_HO_landelijk <- dfCBS_NCO_HO %>%
  filter(ARB_Regio == "alle")

dfCBS_NCO_HO_regionaal <- dfCBS_NCO_HO %>%
  filter(ARB_Regio != "alle")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
vvmover::write_file_proj(dfCBS_NCO_HO_landelijk, "CBS_NCO_HO_landelijk")
vvmover::write_file_proj(dfCBS_NCO_HO_regionaal, "CBS_NCO_HO_regionaal")

clear_script_objects()
