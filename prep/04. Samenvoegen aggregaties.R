## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Samenvoegen Aggregaties.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: De al eerder geaggregeerde bestanden worden hier aan elkar gekoppeld, zodat er adhv één
## dataset gekoppeld kan worden. Op VU niveau wordt regionale arbeidsmarkt data uit het CBS
## gekoppeld. Op landelijk niveau wordt landelijke arbeidsmarkt data uit CBS gekoppeld aan studiedata
## uit het landelijke bestand van 1CHO gekoppeld. Voor alles geldt dat het al eerder is bewerkt en
## geaggregeerd.
##
## Opmerkingen:
## 1) Dit script is ter indicatie. De brondata is hier niet beschikbaar gesteld.
## 2) Ook wordt er gebruikt gemaakt van packages van het vusaverse die
## afhankelijk zijn van environment variables.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INLEZEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## TODO CBS Regionaal heeft teveel rijen
CBS_Regionaal_aggr <- readrds_csv(output = "3. Analyseset/40. CBS Aggregaties/transposed/CBS_Regionaal_aggr.rds")
CBS_Landelijk_aggr <- readrds_csv(output = "3. Analyseset/40. CBS Aggregaties/transposed/CBS_Landelijk_aggr.rds")

EOI_Instellingen_aggr <- readrds_csv(output = "3. Analyseset/40. CBS Aggregaties/transposed/EOI_Instellingen_aggr.rds")
EOI_Landelijk_aggr <- readrds_csv(output = "3. Analyseset/40. CBS Aggregaties/transposed/EOI_Landelijk_aggr.rds")

EOI_VU_intern_aggr <- readrds_csv(output = "3. Analyseset/40. CBS Aggregaties/transposed/EOI_VU_intern_aggr.rds")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BEWERKEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO: Bewerkingen op het databestand
## TODO: Voer hier de bewerkingen uit op het databestand

CBS_Regionaal_aggr <- CBS_Regionaal_aggr %>%
  filter(INS_Inschrijvingsjaar_EOI >= 2012)

EOI_VU_CHO_aggr <- EOI_Instellingen_aggr %>%
  filter(INS_Instellingsafkorting == "VU",
         INS_Eerste_jaar_opleiding_en_instelling <= 2019) %>%
  mutate(OPL_Instellingscode = "21PL")

CBS_Landelijk_aggr <- CBS_Landelijk_aggr %>%
  filter(INS_Inschrijvingsjaar_EOI >= 2012)

EOI_Landelijk_aggr <- EOI_Landelijk_aggr %>%
  filter(INS_Eerste_jaar_opleiding_en_instelling <= 2019)

EOI_VU_intern_aggr <- EOI_VU_intern_aggr %>%
  filter(INS_Inschrijvingsjaar_EOI >= 2012,
         INS_Inschrijvingsjaar_EOI <= 2019)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Testen ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO Volgende code hoort hier eigenlijk niet, maar staat er zolang de exports nog kunnen veranderen
## Dit levert bijna alleen maar lege rijen op (NA, dus aantal minder dan 10)
## 1 uitzondering: # openbare reiniging, waterbeheer en -distributie 2012, aantal 18
alleen_CBS_landelijk <- anti_join(CBS_Landelijk_aggr,
                  EOI_Landelijk_aggr,
                  by = c("INS_Inschrijvingsjaar_EOI" = "INS_Eerste_jaar_opleiding_en_instelling",
                         "OPL_CBS_Richting_code",
                         "CBS_Splits_variabele" = "INS_Splits_variabele",
                         "CBS_Splits_waarde" = "INS_Splits_variabele_waarde"))

alleen_EOI_landelijk <- anti_join(EOI_Landelijk_aggr,
                  CBS_Landelijk_aggr,
                  by = c("INS_Eerste_jaar_opleiding_en_instelling" = "INS_Inschrijvingsjaar_EOI",
                         "OPL_CBS_Richting_code",
                         "INS_Splits_variabele" = "CBS_Splits_variabele",
                         "INS_Splits_variabele_waarde" = "CBS_Splits_waarde"))

alleen_CBS_regionaal_niet_CHO_VU <- anti_join(CBS_Regionaal_aggr,
                                  EOI_VU_CHO_aggr,
                                  by = c("INS_Inschrijvingsjaar_EOI" = "INS_Eerste_jaar_opleiding_en_instelling",
                                         "OPL_Instellingscode",
                                         "OPL_CBS_Crohocode" = "INS_Opleidingscode_actueel",
                                         "CBS_Splits_variabele" = "INS_Splits_variabele",
                                         "CBS_Splits_waarde" = "INS_Splits_variabele_waarde"))

alleen_CBS_regionaal_niet_CHO_VU2 <- anti_join(CBS_Regionaal_aggr,
                                              EOI_VU_CHO_aggr,
                                              by = c("INS_Inschrijvingsjaar_EOI" = "INS_Eerste_jaar_opleiding_en_instelling",
                                                     "OPL_Instellingscode",
                                                     "OPL_CBS_Richting_code",
                                                     "INS_Opleiding_cluster" = "OPL_CBS_Label_richting",
                                                     "OPL_CBS_Crohocode" = "INS_Opleidingscode_actueel",
                                                     "CBS_Splits_variabele" = "INS_Splits_variabele",
                                                     "CBS_Splits_waarde" = "INS_Splits_variabele_waarde"))



## TODO Veel historische opleidingen
alleen_CBS_VU_niet_CHO_VU <- alleen_CBS_regionaal_niet_CHO_VU %>%
  filter(OPL_Instellingscode == "21PL") %>%
  count(INS_Opleidingsnaam_CROHO, OPL_CBS_Crohocode)


alleen_CHO_VU_niet_CBS_regionaal <- anti_join(
  EOI_VU_CHO_aggr,
  CBS_Regionaal_aggr,
  by = c(
    "INS_Eerste_jaar_opleiding_en_instelling" = "INS_Inschrijvingsjaar_EOI",
    "OPL_Instellingscode",
    "INS_Opleidingscode_actueel" = "OPL_CBS_Crohocode",
    "INS_Splits_variabele" = "CBS_Splits_variabele",
    "INS_Splits_variabele_waarde" = "CBS_Splits_waarde"
  )
)

## Enkele buitenland studenten
alleen_VU_intern_niet_CHO <-
  anti_join(
    EOI_VU_intern_aggr,
    EOI_VU_CHO_aggr,
    by = c(
      "INS_Inschrijvingsjaar_EOI" = "INS_Eerste_jaar_opleiding_en_instelling",
      "INS_Opleidingscode_actueel",
      "INS_Splits_variabele",
      "INS_Splits_variabele_waarde"
    )
  )

# M Transport and Supply Chain Management + joint degrees zijn vaak niet aanwezig bij VU intern
alleen_CHO_niet_VU_intern <- anti_join(
  EOI_VU_CHO_aggr,
  EOI_VU_intern_aggr,
  by = c(
    "INS_Eerste_jaar_opleiding_en_instelling" = "INS_Inschrijvingsjaar_EOI",
    "INS_Opleidingscode_actueel",
    "INS_Splits_variabele",
    "INS_Splits_variabele_waarde"
  )
)

## Enkele buitenland studenten
alleen_VU_intern_niet_CBS <-
  anti_join(
    CBS_VU_aggr,
    EOI_VU_intern_aggr,
    by = c(
      "INS_Inschrijvingsjaar_EOI",
      "INS_Opleidingscode_actueel",
      "INS_Splits_variabele",
      "INS_Splits_variabele_waarde"
    )
  )

alleen_VU_intern_niet_CBS2 <-
  anti_join(
    CBS_VU_aggr,
    EOI_VU_intern_aggr,
    by = c(
      "INS_Inschrijvingsjaar_EOI",
      "INS_Opleidingscode_actueel",
      "INS_Splits_variabele",
      "INS_Splits_variabele_waarde",
      "OPL_CBS_Richting_code",
      "INS_Opleiding_cluster" = "OPL_CBS_Label_richting"
    )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. SAMENVOEGEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


CBS_VU_aggr <- CBS_Regionaal_aggr %>%
  inner_join(
    EOI_VU_CHO_aggr,
    by = c(
      "INS_Inschrijvingsjaar_EOI" = "INS_Eerste_jaar_opleiding_en_instelling",
      "OPL_Instellingscode",
      "OPL_CBS_Richting_code",
      "INS_Opleiding_cluster" = "OPL_CBS_Label_richting",
      "OPL_CBS_Crohocode" = "INS_Opleidingscode_actueel",
      "CBS_Splits_variabele" = "INS_Splits_variabele",
      "CBS_Splits_waarde" = "INS_Splits_variabele_waarde"
    )
  ) %>%
  rename(
    INS_Opleidingscode_actueel = OPL_CBS_Crohocode,
    INS_Splits_variabele = CBS_Splits_variabele,
    INS_Splits_variabele_waarde = CBS_Splits_waarde
  ) %>%
  inner_join(
    EOI_VU_intern_aggr,
    by = c(
      "INS_Inschrijvingsjaar_EOI",
      "INS_Opleidingscode_actueel",
      "INS_Splits_variabele",
      "INS_Splits_variabele_waarde",
      "OPL_CBS_Richting_code"
    )
  )

CBS_Landelijk_aggr <- CBS_Landelijk_aggr %>%
  inner_join(
    EOI_Landelijk_aggr,
    by = c(
      "INS_Inschrijvingsjaar_EOI" = "INS_Eerste_jaar_opleiding_en_instelling",
      "OPL_CBS_Richting_code",
      "CBS_Splits_variabele" = "INS_Splits_variabele",
      "CBS_Splits_waarde" = "INS_Splits_variabele_waarde"
    )
  ) %>%
  rename(
    INS_Splits_variabele = CBS_Splits_variabele,
    INS_Splits_variabele_waarde = CBS_Splits_waarde
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BEWAAR & RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


saverds_csv(CBS_VU_aggr,
            "AS_CBS_VU_aggr",
            output = "3. Analyseset/40. CBS Aggregaties/")

saverds_csv(CBS_Landelijk_aggr,
            "AS_CBS_NL_aggr",
            output = "3. Analyseset/40. CBS Aggregaties/")

clear_script_objects()
