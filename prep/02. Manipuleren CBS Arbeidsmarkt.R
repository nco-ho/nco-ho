## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Manipuleren CBS Arbeidsmarkt.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: In dit script worden bewerkingen op de CBS bestanden gedaan
## uitgevoerd
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##' *INFO* Deze bestanden zijn al geaggregeerd (per rij is het aantal groter dan één)
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees de ingelezen bestanden in:
## TODO Let op, twee bestanden worden ingelezen en daarop worden nu dezelfde bewerkingen gedaan
## In de toekomst kan dit veranderen, daarom zijn ze voor nu gescheiden gehouden.
CBS_Landelijk_aggr <-
  vvmover::read_file_proj("CBS_NCO_HO_landelijk")
CBS_Regionaal_aggr <-
  vvmover::read_file_proj("CBS_NCO_HO_regionaal")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Pas mapping toe
CBS_Landelijk_aggr <- CBS_Landelijk_aggr %>%
  mutate(
    ## TODO Let op! Op de inhoud van deze variabelen moet later worden gejoined, gebruik dus namen die
    ## ook in andere bestanden voorkomen
    CBS_Splits_waarde = mapping_fix(CBS_Splits_waarde, mapping_table_name = "Mapping_CBS_Splits_waarde"),
    CBS_Splits_variabele = mapping_fix(CBS_Splits_variabele, mapping_table_name = "Mapping_CBS_Splits_variabele")
  ) %>%
  ## Encode naar UTF-8
  mutate(INS_Opleiding_cluster = iconv(INS_Opleiding_cluster,
    from = "latin1", to =
      "ASCII//TRANSLIT"
  ))

CBS_Regionaal_aggr <- CBS_Regionaal_aggr %>%
  mutate(
    ## TODO Let op! Op de inhoud van deze variabelen moet later worden gejoined, gebruik dus namen die
    ## ook in andere bestanden voorkomen
    CBS_Splits_waarde = mapping_fix(CBS_Splits_waarde, mapping_table_name = "Mapping_CBS_Splits_waarde"),
    CBS_Splits_variabele = mapping_fix(CBS_Splits_variabele, mapping_table_name = "Mapping_CBS_Splits_variabele")
  ) %>%
  ## Encode naar UTF-8
  mutate(INS_Opleiding_cluster = iconv(INS_Opleiding_cluster,
    from = "latin1", to =
      "ASCII//TRANSLIT"
  ))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


vvmover::write_file_proj(CBS_Landelijk_aggr, "CBS_Landelijk_aggr")
vvmover::write_file_proj(CBS_Regionaal_aggr, "CBS_Regionaal_aggr")
clear_script_objects()
