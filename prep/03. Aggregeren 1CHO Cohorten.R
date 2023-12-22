## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Aggregeren 1CHO Cohorten.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: In dit script worden bewerkingen op het Cohorten bestand uitgevoerd. De oorspronkelijke
## bron van het hier ingeladen bestand zijn de landelijke instroom- en examen cohorten bestanden
## van de VSNU.
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##' *INFO* Hier vinden complexe transformaties plaats, de resulterende data "_aggr" heeft meerderere
## observaties per rij. Dit is ongebruikelijk, maar noodzakelijk voor koppeling met CBS export
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO Dit is nu een export uit een rapport script. Eigenlijk moeten bewerkingen in manipuleren
## gebeuren
## TODO Tijdelijk uitgecomment omdat het warning gaf, het staat hier voor demonstratie redenen
#EOI_Landelijk <- readRDS("G:/DSZ/SA2016/Datasets/Output/main/6. Extracten WP/NCO_HO/Cohorten_VSNU.rds")
## TODO Deze export doen we nu nog niets mee
#Diploma_landelijk <- readRDS("G:/DSZ/SA2016/Datasets/Output/main/6. Extracten WP/NCO_HO/Geslaagden_VSNU.rds")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO De functie zit nog in test en er is wat herhaling in de code.

## Haal functie op
source("supporting_functions.R")

## TODO Filter op dit moment op masters om te koppelen aan CBS

## Doe laataste preparaties om later te kunnen koppelen aan CBS
EOI_Landelijk <- EOI_Landelijk %>%
  filter(INS_Opleidingsfase_actueel == "M") %>%
  rename(INS_Opleiding_CROHO_onderdeel = INS_Opleidingscode_CROHO) %>%
  mutate(DEM_Nationaliteit_niet_NL = if_else(DEM_Nationaliteit_EER_naam == "NL", "Nederland", "Buitenland"),
         INS_Inschrijvingsvorm = if_else(INS_Opleidingsvorm_naam == "Voltijd", "Voltijd", "Deeltijd-Duaal")
         )

## Aggregeer per instelling, opleiding en jaar (en eventueel aanvullende variabele)
EOI_Instellingen_alle_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(pivot = TRUE)

EOI_Instellingen_geslacht_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(sAdditional_grouping_variable = "DEM_Geslacht",
                            pivot = TRUE)

EOI_Instellingen_nationaliteit_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(sAdditional_grouping_variable = "DEM_Nationaliteit_niet_NL",
                            pivot = TRUE)

EOI_Instellingen_vorm_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(sAdditional_grouping_variable = "INS_Inschrijvingsvorm",
                            pivot = TRUE)


## Aggregeer voor heel NL en per opleidingsrichting en jaar (en eventueel aanvullende variabele)
EOI_Landelijk_alle_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(sGrouping_variables = "EOI_national",
                            pivot = TRUE)

EOI_Landelijk_geslacht_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(sGrouping_variables = "EOI_national",
                            sAdditional_grouping_variable = "DEM_Geslacht",
                            pivot = TRUE)

EOI_Landelijk_nationaliteit_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(sGrouping_variables = "EOI_national",
                            sAdditional_grouping_variable = "DEM_Nationaliteit_niet_NL",
                            pivot = TRUE)

EOI_Landelijk_vorm_aggr <- EOI_Landelijk %>%
  group_EOI_study_as_cohort(sGrouping_variables = "EOI_national",
                            sAdditional_grouping_variable = "INS_Inschrijvingsvorm",
                            pivot = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. SAMENVOEGEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


EOI_Instellingen_aggr <- bind_rows(
  EOI_Instellingen_alle_aggr,
  EOI_Instellingen_geslacht_aggr,
  EOI_Instellingen_nationaliteit_aggr,
  EOI_Instellingen_vorm_aggr
)

EOI_Landelijk_aggr <- bind_rows(
  EOI_Landelijk_alle_aggr,
  EOI_Landelijk_geslacht_aggr,
  EOI_Landelijk_nationaliteit_aggr,
  EOI_Landelijk_vorm_aggr
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


saverds_csv(EOI_Landelijk_aggr, "EOI_Landelijk_aggr", output = "3. Analyseset/40. CBS Aggregaties/transposed/")
saverds_csv(EOI_Instellingen_aggr, "EOI_Instellingen_aggr", output = "3. Analyseset/40. CBS Aggregaties/transposed/")

clear_script_objects()
