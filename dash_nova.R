library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(readxl)
library(dplyr)
library(ggthemes)
library(ggformula)
library(shinyjs)

# ---- Carregar dados ----
cripto <- read_excel("Leviata_2026.xlsx")

dados <- cripto[1:6,1:7] %>%
  select(Moeda, Inv, Conv, Liq, Perc, "Mês", Ano) %>%
  rename(moeda = Moeda, investido = Inv, liquidez = "Liq",
         mes = "Mês", porcentagem = "Perc", conversao = Conv)

# ---- Gráficos básicos ----
regressao <- ggplot(dados, aes(x = investido, y = liquidez)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, size = 2,
              color = "grey50", fill = "lightblue") +
  labs(x = "Investido", y = "Liquidez", title = "Regressão Linear Simples com IC") +
  theme_classic()

radar <- ggplot(dados) +
  geom_bar(aes(x = moeda), fill = "darkorchid", alpha = 0.8, col="blue") +
  coord_polar() +
  labs(title = "Criptoativos - Radar")

colunas <- ggplot(dados) +
  geom_bar(aes(x = moeda), fill = "red", alpha = 0.9, col="blue") +
  labs(title = "Criptoativos - Barras")

liquidez_plot <- ggplot(dados) +
  geom_point(aes(x = moeda, y = liquidez), shape = 16, col = "red", size = 4, alpha = 0.9) +
  labs(title = "Liquidez por Moeda")

densidade_plot <- gf_dens(liquidez ~ moeda, data = dados, color = ~ moeda) +
  theme_classic() + coord_flip() +
  labs(title = "Distribuição de Ganho de Capital")

melhor_moeda <- ggplot(dados, aes(x = reorder(moeda, -liquidez), y = liquidez)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Avaliação das Moedas para Investimento") +
  theme_economist() + coord_flip()

# ---- Detecção de Anomalias ----
Q1 <- quantile(dados$liquidez, 0.25, na.rm = TRUE)
Q3 <- quantile(dados$liquidez, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR_val
limite_superior <- Q3 + 1.5 * IQR_val

dados$anomalia <- ifelse(dados$liquidez < limite_inferior | dados$liquidez > limite_superior, "Anômalo", "Normal")

anomalia_plot <- ggplot(dados, aes(x = moeda, y = liquidez, color = anomalia)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Normal" = "blue", "Anômalo" = "red")) +
  labs(title = "Detecção de Anomalias na Liquidez") +
  theme_minimal()

# ---- Regressão Múltipla ----
modelo_multiplo <- lm(liquidez ~ investido + porcentagem + conversao, data = dados)
dados$previsto_multi <- predict(modelo_multiplo, newdata = dados)

regressao_multipla_plot <- ggplot(dados, aes(x = liquidez, y = previsto_multi, color = moeda)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Regressão Múltipla: Real vs Previsto",
       x = "Liquidez Real", y = "Liquidez Prevista") +
  theme_light()

# Tabela de coeficientes
summary_model <- summary(modelo_multiplo)
conf_int <- confint(modelo_multiplo)

coef_table <- data.frame(
  Variável   = rownames(summary_model$coefficients),
  Estimativa = round(summary_model$coefficients[, "Estimate"], 3),
  ErroPadrao = round(summary_model$coefficients[, "Std. Error"], 3),
  tValor     = round(summary_model$coefficients[, "t value"], 3),
  pValor     = round(summary_model$coefficients[, "Pr(>|t|)"], 4),
  IC_Lower   = round(conf_int[, 1], 3),
  IC_Upper   = round(conf_int[, 2], 3)
)

# Adicionar R²
r2_val <- round(summary_model$r.squared, 3)
coef_table <- rbind(coef_table,
                    data.frame(Variável="R² do Modelo",
                               Estimativa=r2_val,
                               ErroPadrao=NA, tValor=NA, pValor=NA,
                               IC_Lower=NA, IC_Upper=NA))
# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Leviatã - Dark Mode"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa", tabName = "mapa", icon = icon("globe")),
      menuItem("Regressão", tabName = "regressao", icon = icon("chart-line")),
      menuItem("Regressão Múltipla", tabName = "multipla", icon = icon("project-diagram")),
      menuItem("Gráficos Básicos", tabName = "basicos", icon = icon("chart-bar")),
      menuItem("Melhor Moeda", tabName = "melhor", icon = icon("star")),
      menuItem("Anomalias", tabName = "anomalia", icon = icon("exclamation-triangle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css"),  # <-- CSS externo
  
    # Tela de abertura com GIF cobrindo toda a tela
    div(id = "splash",
        style = "
            position: fixed; top:0; left:0; width:100%; height:100%;
            background-color:#1e1e2f; color:white; z-index:9999;
            display:flex; align-items:center; justify-content:center;
            flex-direction:column;
            opacity:1; transition: opacity 2s ease;
          ",
        img(src = "2.gif", 
            style="width:100%; height:100%; object-fit:cover; position:absolute; top:0; left:0;"),
        
        # Texto e botão sobrepostos ao GIF
        div(style="position:relative; z-index:1000; text-align:center;",
            h1("Bem-vindo ao Projeto Leviatã", style="color:#ff7e5f;"),
            p("Clique no botão abaixo para iniciar", style="color:#cccccc; font-size:20px;"),
            actionButton("startBtn", "Entrar no Dashboard", 
                         style="background-color:#ff7e5f; color:white; font-size:18px; padding:10px 20px; border:none; border-radius:5px; animation: glow 1.5s infinite;")
        ),
        
        # CSS para efeito de brilho no botão
        tags$style("
            @keyframes glow {
              0% { box-shadow: 0 0 5px #ff7e5f; }
              50% { box-shadow: 0 0 20px #ff7e5f; }
              100% { box-shadow: 0 0 5px #ff7e5f; }
            }
          ")
    ),
    
    
      
    tabItems(
      tabItem(tabName = "mapa",
              fluidRow(box(width = 12, leafletOutput("mapPlot", height = 500)))),
      tabItem(tabName = "regressao",
              fluidRow(box(width = 12, plotlyOutput("regressaoPlot", height = 500)))),
      tabItem(tabName = "multipla",
              fluidRow(
                box(width = 12, plotOutput("regressaoMultiplaPlot", height = 500)),
                box(width = 12, tableOutput("coefTable"))
              )),
      tabItem(tabName = "basicos",
              fluidRow(
                box(width = 6, plotOutput("radarPlot", height = 300)),
                box(width = 6, plotOutput("colunasPlot", height = 300))
              ),
              fluidRow(
                box(width = 6, plotOutput("liquidezPlot", height = 300)),
                box(width = 6, plotOutput("densidadePlot", height = 300))
              )),
      tabItem(tabName = "melhor",
              fluidRow(box(width = 12, plotlyOutput("melhorMoedaPlot", height = 400)))),
      tabItem(tabName = "anomalia",
              fluidRow(box(width = 12, plotOutput("anomaliaPlot", height = 400))))
    )
  )
)
# ---- Server ----
server <- function(input, output, session) {
  # Mapa
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -46.9345, lat = -23.5480, zoom = 11) %>%
      addMarkers(lng = -46.9345, lat = -23.5480, popup = "Itapevi")
  })
  
  # Regressão simples
  output$regressaoPlot <- renderPlotly({ ggplotly(regressao) })
  
  # Regressão múltipla
  output$regressaoMultiplaPlot <- renderPlot({ regressao_multipla_plot })
  output$coefTable <- renderTable({ coef_table }, rownames = FALSE)
  
  # Gráficos básicos
  output$radarPlot <- renderPlot({ radar })
  output$colunasPlot <- renderPlot({ colunas })
  output$liquidezPlot <- renderPlot({ liquidez_plot })
  output$densidadePlot <- renderPlot({ densidade_plot })
  
  # Melhor Moeda
  output$melhorMoedaPlot <- renderPlotly({ ggplotly(melhor_moeda) })
  
  # Anomalias
  output$anomaliaPlot <- renderPlot({ anomalia_plot })
}

shinyApp(ui, server)
