## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Aggregeren Inschrijvingen instelling.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Doel
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Haal Inschrijvingen van instelling bestand op
dfAS <- get_analysisset()

## Haal de functies op
source("supporting_functions.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfAS_M_EOI <- dfAS %>%
  filter(
    INS_Opleidingsfase_BPM == "M",
    INS_Inschrijvingsjaar == INS_Inschrijvingsjaar_EOI,
    INS_Indicatie_actief_op_peildatum_status == "actief",
    INS_Inschrijvingsjaar < max(INS_Inschrijvingsjaar)
    ) %>%
  mutate(
    DEM_Nationaliteit_niet_NL = if_else(DEM_Nationaliteit_EER_naam == "NL", "Nederland", "Buitenland"),
    INS_Inschrijvingsvorm = if_else(INS_Opleidingsvorm_naam == "voltijd", "Voltijd", "Deeltijd-Duaal")
  )

dfAS_M_EOI_alle_aggr <- dfAS_M_EOI %>%
  group_as_cohort(pivot = TRUE)

dfAS_M_EOI_geslacht_aggr <- dfAS_M_EOI %>%
  group_as_cohort("DEM_Geslacht", pivot = TRUE)

dfAS_M_EOI_vorm_aggr <- dfAS_M_EOI %>%
  group_as_cohort("INS_Inschrijvingsvorm", pivot = TRUE)

dfAS_M_EOI_nationaliteit_aggr <- dfAS_M_EOI %>%
  group_as_cohort("DEM_Nationaliteit_niet_NL", pivot = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dfAS_M_EOI_aggr <- bind_rows(
  dfAS_M_EOI_alle_aggr,
  dfAS_M_EOI_geslacht_aggr,
  dfAS_M_EOI_vorm_aggr,
  dfAS_M_EOI_nationaliteit_aggr
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saverds_csv(dfAS_M_EOI_aggr, "EOI_VU_intern_aggr", output = "3. Analyseset/40. CBS Aggregaties/transposed/")

clear_script_objects()
