library(shiny)
library(shinydashboard)
library(quantmod)
library(ggplot2)
library(dplyr)
library(scales)
library(sparkline)
library(shinycssloaders)
library(htmlwidgets)

source("collect_data.R")

body <- dashboardBody(
  
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #f4b943;
                            }
                            
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color: #f4b943;
                            }
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #f4b943;
                            }
                            .box-header h3 {
                            font-weight: bold;
                            }
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #f4b943;
                            }
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #ff0000;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #00ff00;
                            color: #000000;
                            }
                            
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #ff69b4;
                            }
                            /* toggle button when hovered  */
                            .info-box {
                            display: block;
                            min-height: 90px;
                            background: #eee;
                            width: 100%;
                            box-shadow: 0 0px 0px rgba(0, 0, 0, 0.1);
                            border-radius: 2px;
                            margin-bottom: 15px;
                            }
                            
                            
                            /* body */
                            .content-wrapper, .right-side {
                            background-color: #ffffff;
                            }
                            
                            ')))
  ,
  
  fluidPage(
    
    titlePanel("Scotland's Economy: key indicators"),
    mainPanel(
      box(DT::dataTableOutput("covid_data"),
          width = 12),
      box(title = "World Covid-19 cases", 
          plotOutput("world_plot", height = 200)%>% withSpinner(), width = 6),
      box(title = "UK Covid-19 cases",
          plotOutput("covid_plot", height = 200)%>% withSpinner(), width = 6),
      box(title = "FTSE 100",
          plotOutput("ftse100plot",
                     height = 200)%>% withSpinner(),
          tags$head(tags$style(HTML('.box{-webkit-box-shadow: none;
                                        -moz-box-shadow: none;
                                        box-shadow: none;}')))
          # ,
          #   infoBox(title = "FTSE 100 latest",
          #           #subtitle = "subtitle test",
          #           value = paste0(round(ftse_quote$`% Change`, 2),  "%"),
          #           color = if(ftse_quote$`% Change`> 0) {"green"
          #             } else {
          #               "red"
          #               },
          #           icon = if(ftse_quote$`% Change` > 0) {
          #             icon("chevron-up", lib="glyphicon")
          #             } else {
          #               icon("chevron-down", lib="glyphicon")
          #               },
          #           fill = TRUE,
          #           width = 12
          #           )
      )
      ,
      box(
        title = "FTSE 250",
        plotOutput("ftse250plot",
                   height = 200)%>% withSpinner()#,
        
        # infoBox(title = "FTSE 250 latest",
        #         #subtitle = "subtitle test",
        #         value = paste0(round(ftmc_quote$`% Change`, 2),  "%"),
        #         color = if(ftmc_quote$`% Change`> 0) {"green"
        #         } else {
        #           "red"
        #         },
        #         icon = if(ftmc_quote$`% Change` > 0) {
        #           icon("chevron-up", lib="glyphicon")
        #         } else {
        #           icon("chevron-down", lib="glyphicon")
        #         },
        #         fill = TRUE,
        #         width = 12
        # )
      ),
      box(
        title = "Pound to Dollar",
        plotOutput("gbpusd",
                   height = 200)%>% withSpinner()
        # ,
        # infoBox(title = "Pound to Euro",
        #         #subtitle = "subtitle test",
        #         value = paste0(brent_raw$Change[1],  "%"),
        #         color = if(brent_raw$Change[1]> 0) {"green"
        #         } else {
        #           "red"
        #         },
        #         icon = if(brent_raw$Change[1] > 0) {
        #           icon("chevron-up", lib="glyphicon")
        #         } else {
        #           icon("chevron-down", lib="glyphicon")
        #         },
        #         fill = TRUE,
        #         width = 12
        # )
      ),
      box(
        title = "Pound to Euro",
        plotOutput("gbpeur",
                   height = 200)%>% withSpinner()
        # ,
        # infoBox(title = "Pound to Euro",
        #         #subtitle = "subtitle test",
        #         value = paste0(brent_raw$Change[1],  "%"),
        #         color = if(brent_raw$Change[1]> 0) {"green"
        #         } else {
        #           "red"
        #         },
        #         icon = if(brent_raw$Change[1] > 0) {
        #           icon("chevron-up", lib="glyphicon")
        #         } else {
        #           icon("chevron-down", lib="glyphicon")
        #         },
        #         fill = TRUE,
        #         width = 12
        # )
      ),
      box(getDependency('sparkline'),
          DT::dataTableOutput("quant_data"),
          width = 12)
      #box(title = "easyjet", plotOutput("easyjetplot")),
      #box(title = "british airways", plotOutput("baplot")),
      #box(title = "gbpusd", plotOutput("gbpusd"))
    )
    
  )
  
)

ui <- dashboardPage(
  
  dashboardHeader(disable= TRUE),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu()),
  body
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ftse100plot <- renderPlot({
    FTSE %>%
      process_quant_data() %>%
      standard_graph()
  })
  
  output$brent_crude_plot <- renderPlot({
    brent_raw %>%
      select(Date, Settle) %>%
      ggplot(aes(x=Date, y=Settle, group = 1)) +
      geom_line() +
      theme_classic()
  })
  
  output$ftse250plot <- renderPlot({
    FTMC %>%
      process_quant_data() %>%
      standard_graph()
  })
  
  output$gbpusd <- renderPlot({
    
    (`GBPUSD=X`) %>%
      process_quant_data() %>%
      standard_graph()
    
  })
  
  output$gbpeur <- renderPlot({
    
    (`GBPEUR=X`) %>%
      process_quant_data() %>%
      standard_graph()
    
  })
  
  output$ftse100latestdata <- DT::renderDataTable({
    ftse_quote 
  })
  output$ftse250latestdata <- renderTable({
    ftmc_quote 
  })
  
  output$ftse_latest <- renderInfoBox({
    
    infoBox(
      "ftse_latest", 
      value = 100
      #(ftmc_quote$`% Change`)
      #, icon = icon("list"),
      #color = "purple"
    )
    
  })
  staticRender_cb <- JS('function(){debugger;HTMLWidgets.staticRender();}')
  
  output$quant_data <- DT::renderDataTable(
    #covid_quant
    df$data,
    #my,
    server = FALSE,
    escape = FALSE,
    selection = 'none',
    options = list(ordering = F,
                   drawCallback = staticRender_cb,
                   dom  = 't',
                   pageLength = 99
    )
  )
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  df <- reactiveValues(data = covid_quant 
                       #%>% mutate(Actions = shinyInput(actionButton, 17, 'button_', label = "Get Latest", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ))
  )
  
  
  output$covid_data <- DT::renderDataTable(
    
    covid_cases2 %>%
      filter(GeoId == "UK") %>%
      head(1),
    #my,
    server = FALSE,
    escape = FALSE,
    selection = 'none',
    options = list(ordering = F,
                   drawCallback = staticRender_cb,
                   dom  = 't',
                   pageLength = 99
    )
  )
  output$covid_plot <- renderPlot(
    
    covid_cases2 %>%
      filter(GeoId == "UK") %>%
      select(DateRep, Cases, Deaths) %>%
      tidyr::pivot_longer(c(Cases, Deaths), names_to = "status", values_to = "count") %>%
      ggplot(aes(x = DateRep,y=count, group = status, colour =status)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(legend.position=c(.1,.75),
            panel.grid.major.y = element_line(colour = "gray"))
    
  )
  
  output$world_plot <- renderPlot(
    
    covid_cases2 %>%
      group_by(DateRep) %>%
      summarise(world_cases = sum(Cases),
                world_deaths = sum(Deaths)) %>%
      tidyr::pivot_longer(c(world_cases, world_deaths), names_to = "status", values_to = "count") %>%
      ggplot(aes(x = DateRep,y=count, group = status, colour =status)) +
      geom_line(size = 1) +
      theme_classic() +
      theme(legend.position=c(.1,.75),
            panel.grid.major.y = element_line(colour = "gray"))
    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
