## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Aggregeren CBS Arbeidsmarkt.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: In dit script worden 2 basis data assets vanuit het CBS gedefinieerd. Eigenlijk is deze
## al geaggregeerd en wordt hier opgesplitst in een regionale versie en een landelijke versie.
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lees het ingelezen bestand CBS in en CROHO

CBS_Regionaal_aggr <- readrds_csv(output = "2. Geprepareerde data/CBS_Regionaal_aggr.rds")
CBS_Landelijk_aggr <- readrds_csv(output = "2. Geprepareerde data/CBS_landelijk_aggr.rds")

## TODO Dit zijn nog oude CROHO-codes, zonder mapping van oud naar nieuw
##      Misschien ook wel handig, omdat we dit voor andere instellingen ook niet hebben
##      Voorbeeld: M Physics (60202) is nu M Science, Business & Innovation (69320)
## TODO Er zit geen jaar waarde in
CBS_CROHO <- readrds_csv(output = "2. Geprepareerde data/CBS_CROHO.rds")
OPL_CROHO <- readrds_csv(output = "2. Geprepareerde data/CROHO.rds")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Regionaal (opleiding) ####
OPL_CROHO_regio_M <- OPL_CROHO %>%
  ## TODO Voor nu selecteer alleen master opleidingen
  filter(startsWith(OPL_Opleidingsnaam_CROHO, "M ")) %>%
  ## TODO Voor nu selecteer alleen opleidingen van UvA, UU en VU
  filter(OPL_Instellingscode %in% c("21PD", "21PK", "21PL")) %>%
  select(OPL_Instellingscode, INS_Opleidingscode_actueel, OPL_Opleidingsnaam_CROHO) %>%
  rename(INS_Opleidingsnaam_CROHO = OPL_Opleidingsnaam_CROHO) %>%
  distinct()

## Pas inner join toe, hiermee worden slechts enkele kleine joint degrees niet gevonden
CBS_CROHO_regio_M <- CBS_CROHO %>%
  inner_join(OPL_CROHO_regio_M, by = c("OPL_CBS_Crohocode" = "INS_Opleidingscode_actueel"))

## Dit verwijdert een lijst aan clusters die niet voorkomen
CBS_Regionaal_aggr <- CBS_Regionaal_aggr %>%
  inner_join(CBS_CROHO_regio_M, by = c("INS_Opleiding_cluster" = "OPL_CBS_Label_richting"))

## Landelijk ####
CBS_richting <- CBS_CROHO %>%
  select(OPL_CBS_Richting_code,
         OPL_CBS_Label_richting) %>%
  distinct()

## TODO Inner join gaat nog niet goed vanwege trema's bij cluster
CBS_Landelijk_aggr <- CBS_Landelijk_aggr %>%
  inner_join(CBS_richting, by = c("INS_Opleiding_cluster" = "OPL_CBS_Label_richting"))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


saverds_csv(CBS_Regionaal_aggr, "CBS_Regionaal_aggr", output = "3. Analyseset/40. CBS Aggregaties/transposed/")
saverds_csv(CBS_Landelijk_aggr, "CBS_Landelijk_aggr", output = "3. Analyseset/40. CBS Aggregaties/transposed/")

Clear_script_objects()
