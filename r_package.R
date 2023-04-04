library(bslib)
library(shiny)
library(crosstalk)
library(plotly)

# For creating the "filter" between the controls and plots
dat <- SharedData$new(dplyr::sample_n(diamonds, 1000))

# Sidebar elements (e.g., filter controls)
cut <- filter_select("cut", "Cut", dat, ~cut)
clarity <- filter_select("clarity", "Clarity", dat, ~clarity)
color <- filter_select("color", "Color", dat, ~color)

# Main elements (e.g., plots)
plot_price <- plot_ly(dat, x = ~price)
plot_carat <- plot_ly(dat, x = ~carat)
plot_depth <- plot_ly(dat, x = ~depth)

filters_acc <- accordion(
  open = TRUE,
  accordion_panel(
    "Cut", icon = bsicons::bs_icon("scissors"),
    filter_select("cut", NULL, dat, ~cut),
  ),
  accordion_panel(
    "Clarity", icon = bsicons::bs_icon("search"),
    filter_select("clarity", NULL, dat, ~clarity)
  )
)



ui <- navbarPage(
  theme = bs_theme(),
  fillable = T,
  title = "Signal Profiler",
  navs_tab_card(
    
    sidebar = sidebar(cut, clarity,conditionalPanel(
      "input.nav === 'Transformations'", 
      color
    )),
    height = 450,
    full_screen = FALSE,
    wrapper = card_body_fill,
    id = "nav",
    nav("Pre-processing", plot_price, plot_carat),
    nav("Transformations", plot_depth),
    nav("Descriptive analysis", plot_depth),
    nav("Stationary analysis", plot_depth),
    nav("Decomposition", plot_depth),
  )
  
  )

  
  

shinyApp(ui, function(...) {})
