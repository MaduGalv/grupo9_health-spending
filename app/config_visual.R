# config_visual.R
# =====================================================================

# Cores principais
azul     <- "#1F4E79"   # governo / setor público
laranja  <- "#E08E45"   # setor privado / bolso das famílias
verde    <- "#2E8B57"   # Brasil (sempre destacado)
cinza    <- "#9AA5B1"   # outros / não especificado
vermelho <- "#B5483A"

# Cores por categoria
cores_fonte <- c(Governo = azul, Privado = laranja, Externo = cinza)

cores_esquema <- c(
  "Governo"            = azul,
  "Seguro voluntário"  = "#F0C27B",
  "Bolso das famílias" = laranja,
  "Exterior"           = cinza,
  "Não especificado"   = "#D5DAE0"
)

cores_fin <- c(
  "Curativo"            = azul,
  "Preventivo"          = verde,
  "Reabilitação"        = "#6C9BC9",
  "Longa duração"       = "#A9C8E4",
  "Bens médicos"        = "#E0A458",
  "Serviços auxiliares" = cinza,
  "Governança"          = "#6B7785",
  "Outros"              = "#D5DAE0"
)

# Cores para séries com vários países
paleta_series <- c(azul, laranja, vermelho, cinza, "#7B6FA6", "#6C9BC9",
                   "#E0A458", "#6B7785")

# Tema único dos gráficos
theme_set(
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
)

# Números no padrão brasileiro (1.000,5)
fmt_num <- scales::label_number(big.mark = ".", decimal.mark = ",")
fmt_pct <- scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ",")
