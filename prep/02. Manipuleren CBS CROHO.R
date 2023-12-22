## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Manipuleren CBS CROHO.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: In dit script worden bewerkingen op de CBS bestanden gedaan
## uitgevoerd
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees het ingelezen bestand CBSCROHO in

CBS_CROHO <- vvmover::read_file_proj("CBS_CROHO")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CBS_CROHO <- CBS_CROHO %>%
  mutate(OPL_CBS_Crohocode = as.double(OPL_CBS_Crohocode)) %>%
  ## Encode naar UTF-8
  mutate(OPL_CBS_Label_richting = iconv(OPL_CBS_Label_richting,
    from = "UTF-8",
    to = "ASCII//TRANSLIT"
  ))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vvmover::write_file_proj(CBS_CROHO, "CBS_CROHO")
clear_script_objects()
