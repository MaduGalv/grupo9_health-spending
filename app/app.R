# app.R: App Shiny "Gasto Global em Saúde"
# =====================================================================

library(shiny)
library(tidyverse)
library(scales)
library(DT)
library(bslib)

source("config_visual.R")   

# ---- Dados ----------------------------------------------------------
painel <- readRDS("dados/painel.rds")

anos <- sort(unique(painel$year))

nomes_paises <- painel |>
  distinct(country_name) |>
  arrange(country_name) |>
  pull(country_name)

pais_br <- painel |> filter(iso3_code == "BRA") |>
  pull(country_name) |> unique() |> first()

# Países que começam selecionados (achados pelo código ISO)
padrao_aba3 <- painel |>
  filter(iso3_code %in% c("BRA", "USA", "DEU", "CHN")) |>
  distinct(country_name) |> pull(country_name)

padrao_fin <- painel |>
  filter(iso3_code %in% c("BRA", "USA", "DEU", "CHL")) |>
  distinct(country_name) |> pull(country_name)

# Anos em que existe a tabela de finalidade do gasto (2016 em diante)
anos_fin <- sort(unique(painel$year[!is.na(painel$hc1_che)]))

# Rótulos das finalidades e dos esquemas
rotulos_fin <- c(
  hc1_che = "Curativo",       hc2_che = "Reabilitação",
  hc3_che = "Longa duração",  hc4_che = "Serviços auxiliares",
  hc5_che = "Bens médicos",   hc6_che = "Preventivo",
  hc7_che = "Governança",     hc9_che = "Outros"
)
niveis_esq <- c("Governo", "Seguro voluntário", "Bolso das famílias",
                "Exterior", "Não especificado")

# Variáveis disponíveis na aba de correlações
vars_cor <- c(
  "Gasto total (bilhões de US$)" = "che_bi",
  "Governo (% do gasto)"         = "gghed_che",
  "Privado (% do gasto)"         = "pvtd_che",
  "Externo (% do gasto)"         = "ext_che",
  "Bolso das famílias (%)"       = "hf3_che",
  "Seguro voluntário (%)"        = "hf2_che",
  "Preventivo (% do gasto)"      = "hc6_che",
  "Curativo (% do gasto)"        = "hc1_che"
)

# Posição do Brasil no ranking mundial, ano a ano (aba 10)
posicao_brasil <- painel |>
  filter(!is.na(che_usd2023)) |>
  group_by(year) |>
  mutate(posicao = min_rank(desc(che_usd2023))) |>
  ungroup() |>
  filter(iso3_code == "BRA") |>
  select(year, posicao, che_usd2023)

# Opções padrão das tabelas
opcoes_tabela <- list(pageLength = 8, dom = "tp", scrollX = TRUE)


# ---- Interface (UI) -------------------------------------------------
ui <- navbarPage(
  title = "Gasto Global em Saúde",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = azul),

  # ---------- ABA 1: PANORAMA ----------
  tabPanel(
    "1. Panorama",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Quanto o mundo gasta em saúde?"),
        sliderInput("anos1", "Período:",
                    min = min(anos), max = max(anos),
                    value = c(min(anos), max(anos)), step = 1, sep = "")
      ),
      mainPanel(
        plotOutput("plot1", height = "380px"), br(), DTOutput("tab1")
      )
    )
  ),

  # ---------- ABA 2: RANKING ----------
  tabPanel(
    "2. Ranking de países",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Quem mais gasta em saúde?"),
        selectInput("ano2", "Ano:", choices = rev(anos), selected = max(anos)),
        sliderInput("n2", "Quantos países mostrar:",
                    min = 5, max = 30, value = 15, step = 1)
      ),
      mainPanel(
        plotOutput("plot2", height = "480px"), br(), DTOutput("tab2")
      )
    )
  ),

  # ---------- ABA 3: TENDÊNCIA ----------
  tabPanel(
    "3. Tendência por país",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Como o gasto mudou ao longo do tempo?"),
        selectizeInput("paises3", "Países:", choices = nomes_paises,
                       selected = padrao_aba3, multiple = TRUE),
        radioButtons("escala3", "Escala do eixo vertical:",
                     choices = c("Logarítmica" = "log", "Linear" = "lin"),
                     selected = "log", inline = TRUE)
      ),
      mainPanel(
        plotOutput("plot3", height = "400px"), br(), DTOutput("tab3")
      )
    )
  ),

  # ---------- ABA 4: ORIGEM DO GASTO ----------
  tabPanel(
    "4. Origem do gasto",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("De onde vem o dinheiro da saúde?"),
        selectInput("pais4", "País:", choices = nomes_paises, selected = pais_br),
        sliderInput("anos4", "Período:",
                    min = min(anos), max = max(anos),
                    value = c(min(anos), max(anos)), step = 1, sep = "")
      ),
      mainPanel(
        plotOutput("plot4", height = "400px"), br(), DTOutput("tab4")
      )
    )
  ),

  # ---------- ABA 5: GASTO DO BOLSO ----------
  tabPanel(
    "5. Gasto do bolso",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Onde as famílias pagam mais direto?"),
        selectInput("ano5", "Ano:", choices = rev(anos), selected = max(anos)),
        radioButtons("ordem5", "Mostrar os países com:",
                     choices = c("Maior participação" = "maiores",
                                 "Menor participação" = "menores")),
        sliderInput("n5", "Quantos países:", min = 5, max = 20, value = 10, step = 1)
      ),
      mainPanel(
        fluidRow(
          column(6, plotOutput("plot5a", height = "380px")),
          column(6, plotOutput("plot5b", height = "380px"))
        ),
        br(),
        DTOutput("tab5")
      )
    )
  ),

  # ---------- ABA 6: ESQUEMAS DE FINANCIAMENTO ----------
  tabPanel(
    "6. Esquemas",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Como cada país paga a saúde?"),
        selectInput("ano6", "Ano:", choices = rev(anos), selected = max(anos)),
        selectizeInput("paises6", "Países:", choices = nomes_paises,
                       selected = padrao_aba3, multiple = TRUE)
      ),
      mainPanel(
        plotOutput("plot6", height = "420px"), br(), DTOutput("tab6")
      )
    )
  ),

  # ---------- ABA 7: FINALIDADE ----------
  tabPanel(
    "7. Finalidade",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Em que o dinheiro é gasto?"),
        selectInput("ano7", "Ano (a partir de 2016):", choices = rev(anos_fin),
                    selected = 2019),
        selectizeInput("paises7", "Países:", choices = nomes_paises,
                       selected = padrao_fin, multiple = TRUE),
        helpText("Atenção: o número de países com dados de finalidade cai",
                 "bastante depois de 2019.")
      ),
      mainPanel(
        plotOutput("plot7", height = "420px"),
        textOutput("aviso7"),
        br(),
        DTOutput("tab7")
      )
    )
  ),

  # ---------- ABA 8: CORRELAÇÕES ----------
  tabPanel(
    "8. Correlações",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Quais variáveis andam juntas?"),
        selectInput("ano8", "Ano:", choices = rev(anos), selected = max(anos)),
        selectInput("x8", "Variável do eixo X:", choices = vars_cor,
                    selected = "che_bi"),
        selectInput("y8", "Variável do eixo Y:", choices = vars_cor,
                    selected = "hf3_che"),
        checkboxInput("log8", "Escala logarítmica no gasto total", value = TRUE)
      ),
      mainPanel(
        plotOutput("plot8", height = "400px"),
        verbatimTextOutput("texto8"),
        h5("Correlações de Spearman entre as variáveis principais"),
        DTOutput("tab8")
      )
    )
  ),

  # ---------- ABA 9: COVID-19 ----------
  tabPanel(
    "9. COVID-19",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("O que aconteceu com o gasto na pandemia?"),
        radioButtons("ano9", "Comparar 2019 com:",
                     choices = c("2020", "2021"), selected = "2020", inline = TRUE),
        radioButtons("ordem9", "A tabela mostra:",
                     choices = c("Maiores quedas" = "queda",
                                 "Maiores altas" = "alta")),
        sliderInput("n9", "Quantos países:", min = 5, max = 20, value = 10, step = 1)
      ),
      mainPanel(
        plotOutput("plot9", height = "400px"),
        verbatimTextOutput("texto9"),
        DTOutput("tab9")
      )
    )
  ),

  # ---------- ABA 10: BRASIL NO MUNDO ----------
  tabPanel(
    "10. Brasil no mundo",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Onde o Brasil está?"),
        selectInput("ano10", "Ano:", choices = rev(anos), selected = max(anos)),
        selectInput("pais10", "Comparar o Brasil com:",
                    choices = c("Mediana mundial", setdiff(nomes_paises, pais_br)),
                    selected = "Mediana mundial")
      ),
      mainPanel(
        fluidRow(
          column(6, plotOutput("plot10a", height = "380px")),
          column(6, plotOutput("plot10b", height = "380px"))
        ),
        br(),
        fluidRow(
          column(6, DTOutput("tab10a")),
          column(6, DTOutput("tab10b"))
        )
      )
    )
  )
)


# ---- Servidor (cálculos e gráficos) ---------------------------------
server <- function(input, output, session) {

  # ===== ABA 1 =====
  dados1 <- reactive({
    painel |>
      filter(year >= input$anos1[1], year <= input$anos1[2]) |>
      group_by(year) |>
      summarise(paises = n_distinct(country_name),
                gasto_bi = sum(che_usd2023, na.rm = TRUE) / 1e9,
                .groups = "drop")
  })

  output$plot1 <- renderPlot({
    ggplot(dados1(), aes(year, gasto_bi)) +
      geom_line(linewidth = 1.1, colour = azul) +
      geom_point(colour = azul, size = 2) +
      scale_y_continuous(labels = fmt_num) +
      labs(title = "Gasto mundial em saúde",
           x = "Ano", y = "Bilhões de US$ (valores de 2023)")
  })

  output$tab1 <- renderDT({
    dados1() |>
      transmute(Ano = year,
                `Países com dados` = paises,
                `Gasto mundial (bilhões de US$)` = round(gasto_bi, 1)) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 2 =====
  dados2 <- reactive({
    painel |>
      filter(year == as.integer(input$ano2), !is.na(che_usd2023)) |>
      slice_max(che_usd2023, n = input$n2) |>
      mutate(gasto_bi = che_usd2023 / 1e9,
             destaque = if_else(iso3_code == "BRA", "Brasil", "Outros"))
  })

  output$plot2 <- renderPlot({
    ggplot(dados2(), aes(x = reorder(country_name, gasto_bi),
                         y = gasto_bi, fill = destaque)) +
      geom_col(width = 0.75) +
      coord_flip() +
      scale_fill_manual(values = c(Brasil = verde, Outros = azul), guide = "none") +
      scale_y_continuous(labels = fmt_num, expand = expansion(mult = c(0, 0.05))) +
      labs(title = paste("Países que mais gastam em saúde,", input$ano2),
           x = NULL, y = "Bilhões de US$ (valores de 2023)")
  })

  output$tab2 <- renderDT({
    dados2() |>
      transmute(`Posição` = row_number(),
                `País` = country_name,
                `Gasto (bilhões de US$)` = round(gasto_bi, 1),
                `Governo (% do gasto)` = round(gghed_che, 1),
                `Bolso das famílias (%)` = round(hf3_che, 1)) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 3 =====
  dados3 <- reactive({
    req(length(input$paises3) > 0)
    painel |> filter(country_name %in% input$paises3, !is.na(che_usd2023))
  })

  output$plot3 <- renderPlot({
    d <- dados3()
    sel <- input$paises3
    cores <- setNames(rep_len(paleta_series, length(sel)), sel)
    cores[names(cores) == pais_br] <- verde

    p <- ggplot(d, aes(year, che_usd2023 / 1e9, colour = country_name)) +
      geom_line(linewidth = 1.1) +
      scale_colour_manual(values = cores) +
      scale_x_continuous(breaks = seq(2000, 2020, 5)) +
      labs(title = "Gasto total em saúde ao longo do tempo",
           x = "Ano", y = "Bilhões de US$ (valores de 2023)", colour = NULL)

    if (input$escala3 == "log") p + scale_y_log10(labels = fmt_num)
    else p + scale_y_continuous(labels = fmt_num)
  })

  output$tab3 <- renderDT({
    dados3() |>
      group_by(country_name) |>
      summarise(a2000 = first(che_usd2023[year == 2000]) / 1e9,
                a2023 = first(che_usd2023[year == 2023]) / 1e9,
                .groups = "drop") |>
      mutate(cresc = (a2023 / a2000 - 1) * 100) |>
      transmute(`País` = country_name,
                `2000 (bi US$)` = round(a2000, 1),
                `2023 (bi US$)` = round(a2023, 1),
                `Crescimento (%)` = round(cresc, 1)) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 4 =====
  dados4 <- reactive({
    painel |>
      filter(country_name == input$pais4,
             year >= input$anos4[1], year <= input$anos4[2]) |>
      select(year, Governo = gghed_che, Privado = pvtd_che, Externo = ext_che)
  })

  output$plot4 <- renderPlot({
    d <- dados4() |>
      pivot_longer(-year, names_to = "fonte", values_to = "percentual") |>
      mutate(fonte = factor(fonte, levels = c("Externo", "Governo", "Privado")))

    ggplot(d, aes(year, percentual, fill = fonte)) +
      geom_area(alpha = 0.95) +
      scale_fill_manual(values = cores_fonte,
                        breaks = c("Governo", "Privado", "Externo")) +
      scale_y_continuous(labels = fmt_pct, expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      labs(title = paste("Origem do gasto em saúde:", input$pais4),
           x = "Ano", y = "% do gasto total", fill = NULL)
  })

  output$tab4 <- renderDT({
    dados4() |>
      transmute(Ano = year,
                `Governo (%)` = round(Governo, 1),
                `Privado (%)` = round(Privado, 1),
                `Externo (%)` = round(Externo, 1)) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 5 =====
  dados5 <- reactive({
    painel |>
      filter(year == as.integer(input$ano5), !is.na(hf3_che)) |>
      select(country_name, iso3_code, oop_pct = hf3_che)
  })

  ranking5 <- reactive({
    d <- dados5()
    if (input$ordem5 == "maiores") {
      slice_max(d, oop_pct, n = input$n5, with_ties = FALSE)
    } else {
      slice_min(d, oop_pct, n = input$n5, with_ties = FALSE)
    }
  })

  output$plot5a <- renderPlot({
    titulo <- if (input$ordem5 == "maiores") {
      "Onde as famílias mais pagam do próprio bolso"
    } else {
      "Onde as famílias menos pagam do próprio bolso"
    }
    ggplot(ranking5(), aes(x = reorder(country_name, oop_pct), y = oop_pct)) +
      geom_col(fill = laranja, width = 0.75) +
      coord_flip() +
      scale_y_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.05))) +
      labs(title = titulo, x = NULL, y = "% do gasto total em saúde")
  })

  output$plot5b <- renderPlot({
    d <- dados5()
    mediana <- median(d$oop_pct, na.rm = TRUE)
    brasil  <- d$oop_pct[d$iso3_code == "BRA"]

    ggplot(d, aes(oop_pct)) +
      geom_histogram(bins = 20, fill = laranja, colour = "white") +
      geom_vline(xintercept = mediana, linetype = "dashed", colour = "#6B7785") +
      geom_vline(xintercept = brasil, colour = verde, linewidth = 1) +
      annotate("text", x = brasil, y = Inf, label = " Brasil",
               colour = verde, hjust = 0, vjust = 1.5, fontface = "bold") +
      annotate("text", x = mediana, y = Inf, label = "Mediana ",
               colour = "#6B7785", hjust = 1, vjust = 3) +
      scale_x_continuous(labels = fmt_pct) +
      labs(title = paste("Todos os países,", input$ano5),
           x = "% do gasto total em saúde", y = "Número de países")
  })

  output$tab5 <- renderDT({
    ranking5() |>
      transmute(`Posição` = row_number(),
                `País` = country_name,
                `Gasto do próprio bolso (%)` = round(oop_pct, 1)) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 6 =====
  dados6 <- reactive({
    req(length(input$paises6) > 0)
    painel |>
      filter(year == as.integer(input$ano6), country_name %in% input$paises6) |>
      select(country_name,
             "Governo"            = hf1_che,
             "Seguro voluntário"  = hf2_che,
             "Bolso das famílias" = hf3_che,
             "Exterior"           = hf4_che,
             "Não especificado"   = hfnec_che) |>
      pivot_longer(-country_name, names_to = "esquema", values_to = "percentual") |>
      mutate(esquema = factor(esquema, levels = rev(niveis_esq)))
  })

  output$plot6 <- renderPlot({
    ggplot(dados6(), aes(country_name, percentual, fill = esquema)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = cores_esquema, breaks = niveis_esq) +
      scale_y_continuous(labels = fmt_pct, expand = c(0, 0)) +
      guides(fill = guide_legend(nrow = 2)) +
      labs(title = paste("Como a saúde é financiada,", input$ano6),
           x = NULL, y = "% do gasto total", fill = NULL)
  })

  output$tab6 <- renderDT({
    dados6() |>
      mutate(percentual = round(percentual, 1)) |>
      pivot_wider(names_from = esquema, values_from = percentual) |>
      rename(`País` = country_name) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 7 =====
  dados7 <- reactive({
    req(length(input$paises7) > 0)
    painel |>
      filter(year == as.integer(input$ano7),
             country_name %in% input$paises7,
             !is.na(hc1_che)) |>
      select(country_name, all_of(names(rotulos_fin))) |>
      pivot_longer(-country_name, names_to = "codigo", values_to = "percentual") |>
      mutate(finalidade = factor(unname(rotulos_fin[codigo]),
                                 levels = rev(names(cores_fin))))
  })

  output$plot7 <- renderPlot({
    d <- dados7()
    validate(need(nrow(d) > 0,
                  "Nenhum dos países selecionados tem dados neste ano."))
    ggplot(d, aes(country_name, percentual, fill = finalidade)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = cores_fin, breaks = names(cores_fin)) +
      scale_y_continuous(labels = fmt_pct, expand = c(0, 0)) +
      guides(fill = guide_legend(nrow = 2)) +
      labs(title = paste("Finalidade do gasto em saúde,", input$ano7),
           x = NULL, y = "% do gasto total", fill = NULL)
  })

  output$aviso7 <- renderText({
    com_dados <- painel |>
      filter(year == as.integer(input$ano7),
             country_name %in% input$paises7, !is.na(hc1_che)) |>
      pull(country_name)
    sem <- setdiff(input$paises7, com_dados)
    if (length(sem) > 0) {
      paste("Sem dados de finalidade neste ano para:", paste(sem, collapse = ", "))
    } else ""
  })

  output$tab7 <- renderDT({
    d <- dados7()
    validate(need(nrow(d) > 0, ""))
    d |>
      mutate(percentual = round(percentual, 1)) |>
      select(country_name, finalidade, percentual) |>
      pivot_wider(names_from = finalidade, values_from = percentual) |>
      rename(`País` = country_name) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 8 =====
  dados_cor <- reactive({
    painel |>
      filter(year == as.integer(input$ano8)) |>
      mutate(che_bi = che_usd2023 / 1e9)
  })

  dados8 <- reactive({
    validate(need(input$x8 != input$y8, "Escolha variáveis diferentes para X e Y."))
    d <- dados_cor() |>
      select(country_name, iso3_code, x = all_of(input$x8), y = all_of(input$y8)) |>
      drop_na(x, y)
    validate(need(nrow(d) >= 10,
                  "Poucos países com dados dessas variáveis neste ano."))
    d
  })

  output$plot8 <- renderPlot({
    d <- dados8()
    br <- d |> filter(iso3_code == "BRA")
    rot_x <- names(vars_cor)[match(input$x8, vars_cor)]
    rot_y <- names(vars_cor)[match(input$y8, vars_cor)]

    p <- ggplot(d, aes(x, y)) +
      geom_point(colour = azul, alpha = 0.5, size = 2) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                  colour = laranja, linewidth = 1) +
      geom_point(data = br, colour = verde, size = 3.5) +
      geom_text(data = br, label = "Brasil", colour = verde,
                vjust = -1, fontface = "bold") +
      labs(title = paste(rot_y, "x", rot_x, paste0("(", input$ano8, ")")),
           x = rot_x, y = rot_y)

    if (input$x8 == "che_bi" && input$log8) p <- p + scale_x_log10(labels = fmt_num)
    if (input$y8 == "che_bi" && input$log8) p <- p + scale_y_log10(labels = fmt_num)
    p
  })

  output$texto8 <- renderText({
    d <- dados8()
    ct <- suppressWarnings(cor.test(d$x, d$y, method = "spearman", exact = FALSE))
    paste0("Correlação de Spearman (rho) = ", round(unname(ct$estimate), 2),
           "   |   p-valor = ", format.pval(ct$p.value, digits = 2, eps = 1e-4),
           "   |   n = ", nrow(d), " países")
  })

  output$tab8 <- renderDT({
    dados_cor() |>
      select("Gasto total" = che_bi, "% governo" = gghed_che,
             "% privado" = pvtd_che, "% externo" = ext_che,
             "% bolso" = hf3_che) |>
      cor(use = "pairwise.complete.obs", method = "spearman") |>
      round(2) |>
      as.data.frame() |>
      rownames_to_column("Variável") |>
      datatable(rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  # ===== ABA 9 =====
  dados9 <- reactive({
    a <- input$ano9
    painel |>
      filter(year %in% c(2019, as.integer(a)), !is.na(che_usd2023)) |>
      select(country_name, iso3_code, year, che_usd2023) |>
      pivot_wider(names_from = year, values_from = che_usd2023) |>
      mutate(antes = .data[["2019"]] / 1e9,
             depois = .data[[a]] / 1e9,
             variacao = (depois / antes - 1) * 100) |>
      filter(!is.na(variacao))
  })

  output$plot9 <- renderPlot({
    d <- dados9()
    brasil <- d$variacao[d$iso3_code == "BRA"]

    ggplot(d, aes(variacao)) +
      geom_histogram(bins = 30, fill = azul, colour = "white") +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#6B7785") +
      geom_vline(xintercept = brasil, colour = verde, linewidth = 1) +
      annotate("text", x = brasil, y = Inf, label = " Brasil",
               colour = verde, hjust = 0, vjust = 1.5, fontface = "bold") +
      scale_x_continuous(labels = fmt_pct) +
      labs(title = paste0("Gasto em saúde na pandemia de COVID-19: variação de 2019 para ",
                          input$ano9),
           subtitle = "Linha tracejada = sem variação",
           x = "Variação do gasto total (%)", y = "Número de países")
  })

  output$texto9 <- renderText({
    d <- dados9()
    wt <- suppressWarnings(wilcox.test(d$depois, d$antes, paired = TRUE, exact = FALSE))
    paste0("Mediana da variação: ", round(median(d$variacao), 1), "%",
           "   |   Wilcoxon pareado: p-valor = ",
           format.pval(wt$p.value, digits = 2, eps = 1e-4),
           "   |   n = ", nrow(d), " países")
  })

  output$tab9 <- renderDT({
    d <- dados9()
    d <- if (input$ordem9 == "queda") arrange(d, variacao) else arrange(d, desc(variacao))
    d |>
      slice_head(n = input$n9) |>
      transmute(`País` = country_name,
                `Gasto em 2019 (bi US$)` = round(antes, 2),
                !!paste0("Gasto em ", input$ano9, " (bi US$)") := round(depois, 2),
                `Variação (%)` = round(variacao, 1)) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })

  # ===== ABA 10 =====
  comp10 <- reactive({
    metricas <- c("Governo (% do gasto)"   = "gghed_che",
                  "Privado (% do gasto)"   = "pvtd_che",
                  "Bolso das famílias (%)" = "hf3_che")
    base <- painel |> filter(year == as.integer(input$ano10))

    vals_br <- as.numeric(base[base$iso3_code == "BRA", unname(metricas)])
    if (input$pais10 == "Mediana mundial") {
      nome_cmp <- "Mediana mundial"
      vals_cmp <- sapply(unname(metricas), \(v) median(base[[v]], na.rm = TRUE))
    } else {
      nome_cmp <- input$pais10
      vals_cmp <- as.numeric(base[base$country_name == nome_cmp, unname(metricas)])
    }

    tibble(indicador = rep(names(metricas), 2),
           grupo = rep(c("Brasil", nome_cmp), each = length(metricas)),
           valor = c(vals_br, vals_cmp))
  })

  output$plot10a <- renderPlot({
    d <- comp10()
    nome_cmp <- setdiff(unique(d$grupo), "Brasil")
    cores <- setNames(c(verde, cinza), c("Brasil", nome_cmp))
    d <- d |> mutate(grupo = factor(grupo, levels = c("Brasil", nome_cmp)))

    ggplot(d, aes(indicador, valor, fill = grupo)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = round(valor, 1)),
                position = position_dodge(width = 0.8),
                vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = cores) +
      scale_x_discrete(labels = label_wrap(14)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste("Brasil x", nome_cmp, paste0("(", input$ano10, ")")),
           x = NULL, y = "%", fill = NULL)
  })

  output$plot10b <- renderPlot({
    ggplot(posicao_brasil, aes(year, posicao)) +
      geom_line(colour = verde, linewidth = 1.1) +
      geom_point(colour = verde, size = 2.5) +
      geom_text(aes(label = posicao), vjust = -1, size = 3.5) +
      scale_y_reverse(breaks = scales::breaks_width(1)) +
      labs(title = "Posição do Brasil no gasto mundial em saúde",
           x = "Ano", y = "Posição no ranking")
  })

  output$tab10a <- renderDT({
    comp10() |>
      mutate(valor = round(valor, 1)) |>
      pivot_wider(names_from = grupo, values_from = valor) |>
      rename(Indicador = indicador) |>
      datatable(rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$tab10b <- renderDT({
    posicao_brasil |>
      transmute(Ano = year,
                `Posição` = posicao,
                `Gasto (bilhões de US$)` = round(che_usd2023 / 1e9, 1)) |>
      datatable(rownames = FALSE, options = opcoes_tabela)
  })
}

shinyApp(ui, server)
