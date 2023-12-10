# header
source("header.R")

# UI
ui <- fluidPage(
  title = "Bundestagswahlergebnisse TH",
  # App Titel
  fluidRow(
    column(
      12, 
      tags$p("Bundestagswahlergebnisse Thüringen", fa("envelopes-bulk", fill = "#0089c1"),  style="font-size: 50px;")
    ) 
  ),
  
  br(),
  
  # App Layout
  sidebarLayout(
    
    sidebarPanel(
      # source
      tags$p(HTML("Quelle: Thüringer Landesamt für Statistik"), style = "text-align: center; font-size: 12px;"),
      ## Eingaben
      fluidRow(
        column(
          12,
          # Input: Wahljahr auswählen
          tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
          sliderInput(
            inputId = "jahr", label = "Welches Wahljahr soll betrachtet werden?",
            min = 2005, max = 2021, value = 2021, step = 4,
            ticks = TRUE, sep = ""
            ),
      
      tags$hr(),
      
      radioButtons(
        inputId = "radio", label = "Für welche Ebene sollen die Ergebnisse dargestellt werden?",
        choices = list(
          "Land" = 1, 
          "Wahlkreise" = 2,
          "Landkreise" = 5,
          "Gemeinden" = 3, 
          "Stadt-/Ortsteile" = 4
          ),
        selected = 1, inline = F
        ),

      # Input: Wahlkreis
      uiOutput("wkSelection"),
      
      # Input: Landkreis
      uiOutput("lkSelection"),
      
      # Input: Gemeinde
      uiOutput("gSelection"),
      
      # Input: Gemeinde
      uiOutput("otSelection"),
      
      tags$hr()
      ),
      tags$style(type = "text/css", ".center {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 30%;
      }"),
      img(src='logo.jpg', class = "center")
      )
    ),
    
    mainPanel(
      ## Ausgaben
      fluidRow(
        column(
          12,

      tabsetPanel(
       
        # Output: Erststimmen Barplot
        tabPanel(
          "Erststimmen",  
          br(),
          plotOutput(outputId = "barplot_erststimmen")
          ), 
        tabPanel(
          "Zweitstimmen",  
          br(),
          plotOutput(outputId = "barplot_zweitstimmen")
          )
        ),
      )
    )
  )
  )
)

# Server
server <- function(input, output, session) {
  
  # Wahlkreiseingabe beobachten
  wk_select <- reactiveVal(NULL)

  observeEvent(input$wahlkreis, {
    wk_select(input$wahlkreis)
  })
  
  # Landkreiseingabe beobachten
  lk_select <- reactiveVal(NULL)
  
  observeEvent(input$landkreis, {
    lk_select(input$landkreis)
  })
  
  # Gemeindeeingabe beobachten
  g_select <- reactiveVal(NULL)
  
  observeEvent(input$gemeinde, {
    g_select(input$gemeinde)
  })
  
  # Stadt-/Ortsteilebene beobachten
  ot_select <- reactiveVal(NULL)
  
  observeEvent(input$ot, {
    ot_select(input$ot)
  })
  
  ### UI Wahlkreis
  output$wkSelection <- renderUI({
    
    
    req(input$jahr)
    
    if (input$radio == 2){
      
      # Updater
      updateSelectInput(session, inputId = "wahlkreis", 
                        choices = c(
                          levels(
                            factor(
                              pull(datenbank[datenbank$Jahr == input$jahr &
                                               datenbank$dataset == "Wahlkreisergebnis",],
                                   Name))
                          )
                        ),
                        selected = wk_select())
      
      selectInput(inputId = "wahlkreis",
                  label = "Bitte einen Wahlkreis auswählen:",
                  choices = c(
                    levels(
                      factor(
                        pull(datenbank[datenbank$Jahr == input$jahr &
                             datenbank$dataset == "Wahlkreisergebnis",],
                             Name))
                      )
                    )
                  )
    }
  })
  
  # UI Landkreis
  output$lkSelection <- renderUI({
    
    req(input$jahr)
    
    if (input$radio == 5){
      
      # Updater
      updateSelectInput(session, inputId = "landkreis", 
                        choices = c(levels(factor(
                          pull(datenbank[datenbank$Jahr == input$jahr &
                                           datenbank$dataset == "Landkreisergebnis", ], Landkreis)
                        )))
                        ,
                        selected = lk_select())
      
      selectInput(inputId = "landkreis",
                  label = "Bitte einen Landkreis auswählen:",
                  choices = c(levels(factor(
                    pull(datenbank[datenbank$Jahr == input$jahr &
                                     datenbank$dataset == "Landkreisergebnis", ], Landkreis)
                  ))))
    }
    
  })
  
  # UI Gemeinde
  output$gSelection <- renderUI({
  
    req(input$jahr)
    
    if (input$radio == 3){
      
      # Updater
      dqshiny::update_autocomplete_input(
        session, id = "gemeinde", 
        label = "Bitte eine Gemeinde eingeben:",
        contains = TRUE,
        structure(autocomplete_gemeinden %>% 
                    filter(Jahr == input$jahr) %>% 
                    pull(Name)
                  ),
        value = g_select())
      
      dqshiny::autocomplete_input(
        id = "gemeinde", 
        label = "Bitte eine Gemeinde eingeben:",
        contains = TRUE,
        structure(autocomplete_gemeinden %>% 
                    filter(Jahr == input$jahr) %>% 
                    pull(Name)
                  )
        )
 
      }

  })
  
  # UI Stadt-/Ortsteile
  output$otSelection <- renderUI({
    
    req(input$jahr)
    
    if (input$radio == 4){
      
      # Updater
      dqshiny::update_autocomplete_input(
        session, 
        id = "ot", 
        label = "Bitte einen Stadt-/Ortsteil eingeben:",
        contains = TRUE,
        structure(autocomplete_ot %>% 
                    filter(Jahr == input$jahr) %>% 
                    pull(Name)
        ),
        value = ot_select())
      
      dqshiny::autocomplete_input(
        id = "ot", 
        label = "Bitte einen Stadt-/Ortsteil eingeben:",
        contains = TRUE,
        structure(autocomplete_ot %>% 
                    filter(Jahr == input$jahr) %>% 
                    pull(Name))
      )
      
    }
    
  })
  
  
  ### Eingaben in UI auf die Datenbank übertragen
  
  # Land
  l_daten <- reactive({
    
    req(input$jahr)
    
    datenbank %>%
      filter(Name == "Land Thüringen" & Jahr == input$jahr)
    
  })
  
  # Wahlkreis
  k_daten <- reactive({
    
    req(input$jahr)
    req(input$wahlkreis)
    req(input$radio == 2)
    
    datenbank %>%
      filter(Name == input$wahlkreis & Jahr == input$jahr)
    
  })
  
  # Landkreis
  lk_daten <- reactive({
    
    req(input$jahr)
    req(input$landkreis)
    req(input$radio == 5)
    
    datenbank %>%
      filter(Landkreis == input$landkreis & Jahr == input$jahr)
    
  })
  
  # Gemeinde
  g_daten <- reactive({
    
    validate(
      need(input$gemeinde != "", "\n\nName der Gemeinde nicht gefunden.")
    )
    
    req(input$jahr)
    req(input$gemeinde)
    req(input$radio == 3)
    
    datenbank %>%
        filter(Name == input$gemeinde & Jahr == input$jahr)
  
  })
  
  # Ortsteil
  ot_daten <- reactive({
    
    validate(
      need(input$ot != "", "\n\nName des Stadt-/Ortsteils nicht gefunden.")
    )
    
    req(input$jahr)
    req(input$ot)
    req(input$radio == 4)
    
    datenbank %>%
      filter(Name == input$ot & Jahr == input$jahr)
    
  })
  
  ### Grafiken
  output$barplot_zweitstimmen <- renderPlot({
    
    if (input$radio == 1){
      
      plot <- 
        l_daten() %>%
        ggplot(data = .,
               aes(x = Partei, 
                   y = Zweitstimmen_relativ, 
                   fill = Partei)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(name = "", values = partei_farben) +
        labs(subtitle = paste0(" Wahlberechtigte: ",
                               format(l_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                               " | ",
                               "Wahlbeteiligung: ",l_daten()$Wahlbeteiligung,"%"
                               ),
             x = "", y = "") + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90),
              axis.text=element_text(size=12)) 
      
    }
    
    if (input$radio == 2){
      
      plot <- 
        k_daten() %>%
        ggplot(data = .,
             aes(x = Partei, 
                 y = Zweitstimmen_relativ, 
                 fill = Partei)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(name = "", values = partei_farben) +
        labs(subtitle = paste0(" Wahlberechtigte: ",
                               format(k_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                               " | ",
                               "Wahlbeteiligung: ",k_daten()$Wahlbeteiligung,"%"
                               ),
             x = "", y = "") + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90),
              axis.text=element_text(size=12)) 
    
  }
    
    if (input$radio == 3){
      
    plot <- 
      g_daten() %>%
      ggplot(data = .,
             aes(x = Partei, 
                 y = Zweitstimmen_relativ, 
                 fill = Partei)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "", values = partei_farben) +
      labs(subtitle = paste0(" Wahlberechtigte: ",
                             format(g_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                             " | ",
                             "Wahlbeteiligung: ",g_daten()$Wahlbeteiligung,"%"
                             ),
           x = "", y = "") + 
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90),
            axis.text=element_text(size=12)) 
    }
    
    
    if (input$radio == 4){
      
      plot <- 
        ot_daten() %>%
        ggplot(data = .,
               aes(x = Partei, 
                   y = Zweitstimmen_relativ, 
                   fill = Partei)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(name = "", values = partei_farben) +
        labs(subtitle = paste0(" Wahlberechtigte: ",
                               format(ot_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                               " | ",
                               "Wahlbeteiligung: ",ot_daten()$Wahlbeteiligung,"%"
                               ),
             x = "", y = "") + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90),
              axis.text=element_text(size=12)) 
    }
    
    if (input$radio == 5){
      
      plot <- 
        lk_daten() %>%
        ggplot(data = .,
               aes(x = Partei, 
                   y = Zweitstimmen_relativ, 
                   fill = Partei)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(name = "", values = partei_farben) +
        labs(subtitle = paste0(" Wahlberechtigte: ",
                               format(lk_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                               " | ",
                               "Wahlbeteiligung: ",lk_daten()$Wahlbeteiligung,"%"
                               ),
             x = "", y = "") + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90),
              axis.text=element_text(size=12)) 
    }
    
    plot
    
    
  }, height = function() {
    if (session$clientData$output_barplot_zweitstimmen_width <= 1000) {
      (session$clientData$output_barplot_zweitstimmen_width)*(3/4)
    } else { (session$clientData$output_barplot_zweitstimmen_width)*(7/16) }
  })
  
  # Barplot Erststimmen
  output$barplot_erststimmen <- renderPlot({
    
    if (input$radio == 1){
      
      plot2 <- 
        l_daten() %>%
        ggplot(data = .,
               aes(x = Partei, 
                   y = Erststimmen_relativ, 
                   fill = Partei)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(name = "", values = partei_farben) +
        labs(subtitle = paste0(" Wahlberechtigte: ",
                               format(l_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                               " | ",
                               "Wahlbeteiligung: ",l_daten()$Wahlbeteiligung,"%"
                               ),
             x = "", y = "") + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90),
              axis.text=element_text(size=12)) 
      
    }
      
      
      if (input$radio == 2){
        
        plot2 <- 
          k_daten() %>%
          ggplot(data = .,
                 aes(x = Partei, 
                     y = Erststimmen_relativ, 
                     fill = Partei)) +
          geom_bar(stat = "identity", position = "dodge", color = "black") +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(name = "", values = partei_farben) +
          labs(subtitle = paste0(" Wahlberechtigte: ",
                                 format(k_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                                 " | ",
                                 "Wahlbeteiligung: ",k_daten()$Wahlbeteiligung,"%"
                                 ),
               x = "", y = "") + 
          theme_bw() +
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 90),
                axis.text=element_text(size=12)) 
        
      }
      
      if (input$radio == 3){
        
        plot2 <- 
          g_daten() %>%
          ggplot(data = .,
                 aes(x = Partei, 
                     y = Erststimmen_relativ, 
                     fill = Partei)) +
          geom_bar(stat = "identity", position = "dodge", color = "black") +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(name = "", values = partei_farben) +
          labs(subtitle = paste0(" Wahlberechtigte: ",
                                 format(g_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                                 " | ",
                                 "Wahlbeteiligung: ",g_daten()$Wahlbeteiligung,"%"
                                 ),
               x = "", y = "") + 
          theme_bw() +
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 90),
                axis.text=element_text(size=12)) 
      }
      
      
      if (input$radio == 4){
        
        plot2 <- 
          ot_daten() %>%
          ggplot(data = .,
                 aes(x = Partei, 
                     y = Erststimmen_relativ, 
                     fill = Partei)) +
          geom_bar(stat = "identity", position = "dodge", color = "black") +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(name = "", values = partei_farben) +
          labs(subtitle = paste0(" Wahlberechtigte: ",
                                 format(ot_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                                 " | ",
                                 "Wahlbeteiligung: ",ot_daten()$Wahlbeteiligung,"%"
                                 ),
               x = "", y = "") + 
          theme_bw() +
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 90),
                axis.text=element_text(size=12)) 
      }
    
    if (input$radio == 5){
      
      plot2 <- 
        lk_daten() %>%
        ggplot(data = .,
               aes(x = Partei, 
                   y = Erststimmen_relativ, 
                   fill = Partei)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(name = "", values = partei_farben) +
        labs(subtitle = paste0(" Wahlberechtigte: ",
                               format(lk_daten()$Wahlberechtigte, big.mark = ".", decimal.mark = ","),
                               " | ",
                               "Wahlbeteiligung: ",lk_daten()$Wahlbeteiligung,"%"
                               ),
             x = "", y = "") + 
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90),
              axis.text=element_text(size=12)) 
    }
      
    
    plot2
    
  }, height = function() {
    if (session$clientData$output_barplot_erststimmen_width <= 1000) {
      (session$clientData$output_barplot_erststimmen_width)*(3/4)
    } else { (session$clientData$output_barplot_erststimmen_width)*(7/16) }
    }
  )
  
  
}

shinyApp(ui, server)
