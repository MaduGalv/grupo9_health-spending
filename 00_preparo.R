# =====================================================================
# 00_preparo.R
# Prepara os dados do trabalho (Gasto Global em Saúde, TidyTuesday 2026,
# semana 16) e salva o painel em app/dados/painel.rds.
#
# O relatório e o app Shiny LEEM esse mesmo arquivo. Assim, qualquer
# decisão de limpeza fica em um lugar só.
#
# Como rodar: abra o projeto do RStudio na pasta do trabalho e rode o
# arquivo inteiro (Ctrl+Shift+Enter, ou "Source").
# =====================================================================

library(tidyverse)
library(tidytuesdayR)
library(countrycode)   # só traduz os nomes dos países

# ---- 1. Baixar os dados ---------------------------------------------
tuesdata <- tt_load(2026, week = 16)

health_spending   <- tuesdata$health_spending    # fonte do gasto
financing_schemes <- tuesdata$financing_schemes  # esquema de financiamento
spending_purpose  <- tuesdata$spending_purpose   # finalidade do gasto

# ---- 2. Nomes de países em português --------------------------------
traduzir_pais <- function(nome, iso3) {
  pt <- suppressWarnings(
    tryCatch(
      countrycode(iso3, origin = "iso3c", destination = "cldr.name.pt"),
      error = function(e) rep(NA_character_, length(iso3))
    )
  )
  pt <- coalesce(pt, nome)
  case_when(
    iso3 == "GBR" ~ "Reino Unido",
    iso3 == "NLD" ~ "Países Baixos",
    iso3 == "USA" ~ "Estados Unidos",
    TRUE ~ pt
  )
}

# ---- 3. Formato largo (uma linha = país + ano) ----------------------
para_largo <- function(df) {
  df |>
    select(country_name, iso3_code, year, indicator_code, value) |>
    mutate(country_name = traduzir_pais(country_name, iso3_code)) |>
    pivot_wider(names_from = indicator_code, values_from = value) |>
    # Percentual ausente = 0% (as partes somam ~100%)
    mutate(across(ends_with("_che"), \(x) replace_na(x, 0)))
}

gasto      <- para_largo(health_spending)
esquemas   <- para_largo(financing_schemes)
finalidade <- para_largo(spending_purpose)

# ---- 4. Painel único ------------------------------------------------
chaves <- c("country_name", "iso3_code", "year")
painel <- gasto |>
  left_join(esquemas,   by = chaves) |>
  left_join(finalidade, by = chaves)

# ---- 5. Salvar ------------------------------------------------------
dir.create("app/dados", recursive = TRUE, showWarnings = FALSE)
saveRDS(painel, "app/dados/painel.rds")

# Conferência rápida
dim(painel)                      # esperado: 4588 linhas
n_distinct(painel$country_name)  # esperado: 195
