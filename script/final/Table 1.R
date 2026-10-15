##################################################
##
##
##################################################

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(cowplot)
library(gtsummary)
library(glue)
library(forcats)

data_clean3 <- readRDS("dataset/db01.rds")

data_clean3 <- data_clean3 %>%
  filter(idade >= 60)

data_clean3$alfabetizado <- as.factor(data_clean3$alfabetizado)


# Table

tabla2 <- data_clean3 %>%
  dplyr::select(
    idade, sexo_f, estado_civil_rec2,
    fuma_rec, alcool_rec,
    alfabetizado,
    renda_rec,
    filhos_rec,
    moradores_rec,
    any_comorbidity,
    starts_with("comorbidades___")
  ) %>%
  tbl_summary(
    statistic = list(
      idade ~ "{median} ({p25}–{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      idade ~ "Age, median (IQR)",
      sexo_f ~ "Female sex",
      estado_civil_rec2 ~ "Marital status",
      fuma_rec ~ "Smoking",
      alcool_rec ~ "Alcohol consumption",
      alfabetizado ~ "Education level",
      renda_rec ~ "Income (minimum wage)",
      filhos_rec ~ "Has children",
      moradores_rec ~ "Number of people in household",
      any_comorbidity ~ "Any comorbidity",
      comorbidades___1 ~ "Hypertension",
      comorbidades___2 ~ "Diabetes",
      comorbidades___3 ~ "Cardiovascular disease",
      comorbidades___4 ~ "Cancer",
      comorbidades___5 ~ "Stroke (CVA)",
      comorbidades___6 ~ "Dyslipidemia",
      comorbidades___7 ~ "Osteoporosis",
      comorbidades___8 ~ "Arthritis or osteoarthritis",
      comorbidades___9 ~ "Depression",
      comorbidades___10 ~ "Other comorbidities"
    )
  ) %>%
  modify_table_body(
    ~ .x %>%
      filter(!(variable == "alfabetizado" & label != "Not literate"))
  )

tabla2


#--------------------------------------------------
# PCS (Physical Component Summary)
#--------------------------------------------------
tabla_reg_pcs <- data_clean3 %>%
  select(
    PhysicalHealth1_v1,
    idade,
    sexo_f,
    estado_civil_rec2,
    fuma_rec,
    alcool_rec,
    alfabetizado,
    renda_rec,
    filhos_rec,
    moradores_rec,
    any_comorbidity,
    starts_with("comorbidades___")
  ) %>%
  tbl_uvregression(
    method = lm,
    y = PhysicalHealth1_v1,
    include = -PhysicalHealth1_v1,
    exponentiate = FALSE,
    show_single_row = c(
      fuma_rec,
      alcool_rec,
      #alfabetizado,
      filhos_rec,
      any_comorbidity,
      starts_with("comorbidades___")
    ),
    label = list(
      idade ~ "Age in years",
      sexo_f ~ "Female sex",
      estado_civil_rec2 ~ "Marital status",
      fuma_rec ~ "Smoking",
      alcool_rec ~ "Alcohol consumption",
      alfabetizado ~ "Education",
      renda_rec ~ "Income",
      filhos_rec ~ "Has children",
      moradores_rec ~ "Number of people in household",
      any_comorbidity ~ "Any comorbidity",
      comorbidades___1 ~ "Hypertension",
      comorbidades___2 ~ "Diabetes",
      comorbidades___3 ~ "Cardiovascular disease",
      comorbidades___4 ~ "Cancer",
      comorbidades___5 ~ "Stroke (CVA)",
      comorbidades___6 ~ "Dyslipidemia",
      comorbidades___7 ~ "Osteoporosis",
      comorbidades___8 ~ "Arthritis or osteoarthritis",
      comorbidades___9 ~ "Depression",
      comorbidades___10 ~ "Other comorbidities"
    )
  ) %>%
  modify_table_body(
    ~ .x %>%
      #filter(!(variable == "alfabetizado" & label != "Literate")) %>%
      mutate(
        `β (95% CI)` = if_else(
          !is.na(estimate),
          glue("{style_number(estimate, digits = 2)} ({ci})"),
          NA_character_
        )
      ) %>%
      select(-estimate, -ci)
  ) %>%
  modify_header(
    label ~ "**Predictors**",
    `β (95% CI)` ~ "**PCS β (95% CI)**",
    p.value ~ "**P-value**"
  )

tabla_reg_pcs

#--------------------------------------------------
# MCS (Mental Component Summary)
#--------------------------------------------------
tabla_reg_mcs <- data_clean3 %>%
  select(
    MentalHealth1_v1,
    idade,
    sexo_f,
    estado_civil_rec2,
    fuma_rec,
    alcool_rec,
    alfabetizado,
    renda_rec,
    filhos_rec,
    moradores_rec,
    any_comorbidity,
    starts_with("comorbidades___")
  ) %>%
  tbl_uvregression(
    method = lm,
    y = MentalHealth1_v1,
    include = -MentalHealth1_v1,
    exponentiate = FALSE,
    show_single_row = c(
      fuma_rec,
      alcool_rec,
      #alfabetizado,
      filhos_rec,
      any_comorbidity,
      starts_with("comorbidades___")
    ),
    label = list(
      idade ~ "Age in years",
      sexo_f ~ "Female sex",
      estado_civil_rec2 ~ "Marital status",
      fuma_rec ~ "Smoking",
      alcool_rec ~ "Alcohol consumption",
      alfabetizado ~ "Education",
      renda_rec ~ "Income",
      filhos_rec ~ "Has children",
      moradores_rec ~ "Number of people in household",
      any_comorbidity ~ "Any comorbidity",
      comorbidades___1 ~ "Hypertension",
      comorbidades___2 ~ "Diabetes",
      comorbidades___3 ~ "Cardiovascular disease",
      comorbidades___4 ~ "Cancer",
      comorbidades___5 ~ "Stroke (CVA)",
      comorbidades___6 ~ "Dyslipidemia",
      comorbidades___7 ~ "Osteoporosis",
      comorbidades___8 ~ "Arthritis or osteoarthritis",
      comorbidades___9 ~ "Depression",
      comorbidades___10 ~ "Other comorbidities"
    )
  ) %>%
  modify_table_body(
    ~ .x %>%
      #filter(!(variable == "alfabetizado" & label != "Literate")) %>%
      mutate(
        `β (95% CI)` = if_else(
          !is.na(estimate),
          glue("{style_number(estimate, digits = 2)} ({ci})"),
          NA_character_
        )
      ) %>%
      select(-estimate, -ci)
  ) %>%
  modify_header(
    label ~ "**Predictors**",
    `β (95% CI)` ~ "**MCS β (95% CI)**",
    p.value ~ "**P-value**"
  )


tabla_reg_mcs



##############################
## miss folow-up
##############################

tabla2 <- data_clean3 %>%
  dplyr::select(
    idade, sexo_f, estado_civil_rec2,
    fuma_rec, alcool_rec,
    alfabetizado,
    renda_rec,
    filhos_rec,
    moradores_rec,
    any_comorbidity,
    starts_with("comorbidades___")
  ) %>%
  tbl_summary(
    statistic = list(
      idade ~ "{median} ({p25}–{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      idade ~ "Age, median (IQR)",
      sexo_f ~ "Female sex",
      estado_civil_rec2 ~ "Marital status",
      fuma_rec ~ "Smoking",
      alcool_rec ~ "Alcohol consumption",
      alfabetizado ~ "Education level",
      renda_rec ~ "Income (minimum wage)",
      filhos_rec ~ "Has children",
      moradores_rec ~ "Number of people in household",
      any_comorbidity ~ "Any comorbidity",
      comorbidades___1 ~ "Hypertension",
      comorbidades___2 ~ "Diabetes",
      comorbidades___3 ~ "Cardiovascular disease",
      comorbidades___4 ~ "Cancer",
      comorbidades___5 ~ "Stroke (CVA)",
      comorbidades___6 ~ "Dyslipidemia",
      comorbidades___7 ~ "Osteoporosis",
      comorbidades___8 ~ "Arthritis or osteoarthritis",
      comorbidades___9 ~ "Depression",
      comorbidades___10 ~ "Other comorbidities"
    )
  ) %>%
  modify_table_body(
    ~ .x %>%
      filter(!(variable == "alfabetizado" & label != "Not literate"))
  )

tabla2

