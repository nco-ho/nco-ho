## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 01. Inlezen CBS CROHO.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: In dit script wordt het bestand CBS Croho ingelezen
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bestandspad <- paste0(
  Sys.getenv(
    "NETWORK_DIR"
  ),
  "Datasets/CBS/CROHO/export_221003_classificatie_crohocode.xlsx"
)
dfCBS <- readxl::read_xlsx(bestandspad)
dfCBS_naming <- read_documentation("Documentatie_CBS_CROHO.csv")

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Up to date check
## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
up_to_date(
  bestandspad = bestandspad,
  frequentie = 365,
  contact = "Tom",
  inleesscript = "Inlezen CBS CROHO.R"
)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. ASSERTIONS ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

vvauditor::assert_naming(dfCBS, dfCBS_naming, "CBS")

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Pas kolomnamen aan met behulp van de documentatie

dfCBS <- dfCBS %>%
  wrapper_translate_colnames_documentation(
    dfCBS_naming
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vvmover::write_file_proj(
  dfCBS,
  "CBS_CROHO"
)

clear_script_objects()
