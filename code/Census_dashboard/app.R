# Shiny application for census dashboard
# Loading the packages needed for the app
library(shiny)
library(ggplot2)
library(haven)
library(dplyr)
library(bslib)
library(leaflet)
library(tidyr)
library(patchwork)
library(shinyWidgets)
library(ragg)
library(shinyjs)
library(cowplot)
# library(thematic)
# library(ragg)

jsCode <- "
shinyjs.getWidth = function() {
  Shiny.onInputChange('screen_width', window.innerWidth);
}
"

# Loading the data
census_data_all <- read_sav("combined_census_data.sav") %>%
  filter(exclude == 0)
pre_snapshot <- read_sav("pre_snapshot.sav")
post_snapshot <- read_sav("post_snapshot.sav")

# Adding duplicate of the data for all cities plots
all_cities_data_census <- census_data_all %>%
  mutate(City = "All Cities")
census_data_all <- bind_rows(census_data_all, all_cities_data_census) %>%
  haven::as_factor()
all_cities_data_pre <- pre_snapshot %>%
  mutate(City = "All Cities")
pre_snapshot <- bind_rows(pre_snapshot, all_cities_data_pre) %>%
  haven::as_factor()


census_n <- census_data_all %>%
  mutate(Q6 = if_any(starts_with("Q6"), ~!is.na(.)),
         Q8 = if_any(starts_with("Q8"), ~!is.na(.)),
         Q12 = if_any(starts_with("Q12"), ~!is.na(.)),
         Q15 = if_any(starts_with("Q15"), ~!is.na(.)),
         Q17 = if_any(starts_with("Q17"), ~!is.na(.))) %>%
  mutate(across(where(is.logical), ~ifelse(. == TRUE, 1, NA)))


pre_snapshot_n2 <-  pre_snapshot %>%
  select(-Q14_15_TEXT) %>%
  mutate(Q8 = if_any(starts_with("Q8"), ~!is.na(.)),
         Q9 = if_any(starts_with("Q9"), ~!is.na(.)),
         Q12 = if_any(starts_with("Q12"), ~!is.na(.)),
         Q14 = if_any(starts_with("Q14"), ~!is.na(.))) %>%
  mutate(across(where(is.logical), ~ifelse(. == TRUE, 1, NA))) %>%
  group_by(City) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  select(City, num_range("Q", range = 1:30), Q15_1, Q15_2, Q15_3) %>%
  select(City, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15_1, Q15_2, Q15_3, Q18) %>%
  setNames(c("City", paste0(names(.)[-1], "_n")))

# The color used by the barplots
global_colors <- c("#007CBB")

# options(shiny.useragg = TRUE)
#
# thematic_shiny(font = "auto")

# Defining the ui for the application
ui <- page_fillable(
  
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "top-left",
                              color = global_colors[1]),
  
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("getWidth")),
  tags$script("$(document).on('shiny:connected', function() { shinyjs.getWidth(); $(window).resize(function() { shinyjs.getWidth(); }); });"),
  
  tags$head(
    HTML("<title>OpenMusE Dashboard</title>"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles_mobile_tables.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  layout_columns(
    
    layout_columns(
      
      card(
        
        card_header("Input", actionButton("toggle_card", "Show / Hide"), 
                    downloadButton("downloadDataCensus", "Download Census"),
                    downloadButton("downloadDataVenue", "Download Venue")),
        
        div(
          
          id = "card_body",
          
          fluidRow(
            
            column(5,
                   
                   shinyWidgets::pickerInput("city", "City:",
                                             c("Helsinki", "Lviv", "Heidelberg", "Mannheim", "Vilnius", "All Cities"),
                                             multiple = TRUE, selected = "Helsinki",
                                             options = list(container = "body"))
            ),
            column(5,
                   
                   shinyWidgets::pickerInput("gender", "Gender:", c("Total" = "total",
                                                                    "Female" , "Male", "Another" = "Another gender identity"),
                                             multiple = TRUE, selected = "total",
                                             options = list(container = "body"))
            )
          ),
          actionButton(
            inputId = "update_data",
            label   = "Update Data",
            icon    = icon("sync"),   
            width   = "90%"), 
        ),
        
        #   fluidRow(
        
        #   column(5, downloadButton("downloadDataCensus", "Download Census")),
        
        #  column(5, downloadButton("downloadDataVenue", "Download Venue"))
        
        # ),
        
      ),
      
      # Add the jump buttons for mobile
      div(class = "mobile-only jump-buttons-container",
          tags$a(href = "#audience_plots_section", class = "jump-button", "Audience Data"),
          tags$a(href = "#venue_plots_section", class = "jump-button", "Venue Data")
      ),
      
      card(
        id = "audience_plots_section",
        full_screen = TRUE,
        card_header("Audience Data Plots"),
        
        mainPanel(
          
          titlePanel("Gender distribution"),
          
          div(class = "mobile-hide", plotOutput("Gender", width = "150%", height = "600px")),
          div(class = "mobile-only", tableOutput("Gender_table"))
          
        ),
        
        titlePanel("Age distribution"),
        
        div(class = "mobile-hide", plotOutput("Age", width = "100%", height = "600px")),
        div(class = "mobile-only", tableOutput("Age_table")),
        
        mainPanel(
          
          titlePanel("Do you live in the city area?"),
          
          div(class = "mobile-hide", plotOutput("CityArea", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("CityArea_table"))
        ),
        
        mainPanel(
          
          titlePanel("Total spending"),
          
          tableOutput("Spending_table"),
          div(class = "mobile-hide", plotOutput("Spending", width = "150%")),
        ),
        
        titlePanel("Spending on: "),
        
        shinyWidgets::pickerInput(inputId = "spend_category", label = "", choices = c(
          "Ticket/entry" = "Q6_1", "Transportation" = "Q6_2",
          "Food/drink" = "Q6_3", "Merchandise" = "Q6_4",
          "Accommondation" = "Q6_5"), multiple = TRUE,
          selected = c("Ticket/entry" = "Q6_1")),
        
        mainPanel(
          
          tableOutput("Spending_on_table"),
          div(class = "mobile-hide", plotOutput("Spending_on", width = "150%"))
        ),
        
        mainPanel(
          
          titlePanel("Will you / did you eat at restaurants?"),
          
          div(class = "mobile-hide", plotOutput("Restaurant", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Restaurant_table"))
        ),
        
        mainPanel(
          
          titlePanel("How will you / did you get home?"),
          
          div(class = "mobile-hide", plotOutput("Transport", width = "150%")),
          div(class = "mobile-only", tableOutput("Transport_table"))
        ),
        
        mainPanel(
          
          titlePanel("How was your night?"),
          
          div(class = "mobile-hide", plotOutput("Night", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Night_table"))
        ),
        
        mainPanel(
          
          titlePanel("How would you rate the live music scene in your city?"),
          
          div(class = "mobile-hide", plotOutput("Scene", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Scene_table"))
        ),
        
        mainPanel(
          
          titlePanel("What kinds of music do you most often go to see live?"),
          
          div(class = "mobile-hide", plotOutput("MusicType", width = "150%")),
          div(class = "mobile-only", tableOutput("MusicType_table"))
        ),
        
        mainPanel(
          
          titlePanel("How many live music events do you attend per month, on average"),
          
          div(class = "mobile-hide", plotOutput("Attend", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Attend_table"))
        ),
        
        mainPanel(
          
          titlePanel("What size of venue you most enjoy seeing live music?"),
          
          div(class = "mobile-hide", plotOutput("VenueSize", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("VenueSize_table"))
        ),
        
        mainPanel(
          
          titlePanel("What has prevented from seeing live music events more often"),
          
          div(class = "mobile-hide", plotOutput("Prevent", width = "150%")),
          div(class = "mobile-only", tableOutput("Prevent_table"))
        ),
        
        mainPanel(
          
          titlePanel("Which of the following would encourage you to see more live music?"),
          
          div(class = "mobile-hide", plotOutput("Encourage", width = "150%")),
          div(class = "mobile-only", tableOutput("Encourage_table"))
        ),
        
        mainPanel(
          
          titlePanel("Do you agree with the following statement?"),
          
          div(class = "mobile-hide", plotOutput("Statement", width = "150%", height = "600px")),
          div(class = "mobile-only", tableOutput("Statement_table"))
        ),
      ),
      
      col_widths = c(12,24)
    ),
    
    layout_columns(
      
      card(
        id = "venue_plots_section",
        full_screen = TRUE,
        card_header("Venue Data Plots"),
        
        # Start venue plots here
        
        # Pre snapshot plots
        
        mainPanel(
          
          titlePanel("On which days is the venue open?"),
          
          div(class = "mobile-hide", plotOutput("Operation_days", width = "100%")),
          div(class = "mobile-only", tableOutput("Operation_days_table"))
        ),
        
        mainPanel(
          
          titlePanel("Is your venue negatively affected by?"),
          
          div(class = "mobile-hide", plotOutput("Negative_effects", width = "150%", height = "500px")),
          div(class = "mobile-only", tableOutput("Negative_effects_table"))
        ),
        
        mainPanel(
          
          titlePanel("Music venues and festivals have a responsibility \n to become environmentally friendly"),
          
          div(class = "mobile-hide", plotOutput("Envi_friendly", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Envi_friendly_table"))
        ),
        
        mainPanel(
          
          titlePanel("The live music scene in my city has discrimination problems"),
          
          div(class = "mobile-hide", plotOutput("Discri_prob", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Discri_prob_table"))
        ),
        
        mainPanel(
          
          titlePanel("The live music scene in my city has corruption problems"),
          
          div(class = "mobile-hide", plotOutput("Corrupt_prob", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Corrupt_prob_table"))
        ),
        
        mainPanel(
          
          titlePanel("Are you optimistic about the future of your venue?"),
          
          div(class = "mobile-hide", plotOutput("Optimistic", width = "150%", height = "400px")),
          div(class = "mobile-only", tableOutput("Optimistic_table"))
        ),
        
        # Post snapshot plots
        
        # End here
      )
    )
  ),
  
  theme = bs_theme()
  
)



# Define server logic
server <- function(input, output) {
  
  observeEvent(input$toggle_card, {
    toggle("card_body")
  })
  
  theme_set(theme_minimal() +
              theme(
                plot.title = element_text(face = "bold", color = "steelblue", hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = "bold", color = "steelblue", hjust = 0.5, size = 16),
                plot.background = element_rect(fill = "white", color = NA),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank()
              ))
  
  selected_gender <- reactive({
    if (input$gender == "total") {
      c("Male", "Female", "Another gender identity")
    } else {
      input$gender
    }
  })
  
  census_n2 <- reactive({
    input$update_data
    isolate({
      req(selected_gender())
      census_n %>%
        filter(Q2 %in% selected_gender()) %>%
        group_by(City) %>%
        summarise(across(everything(), ~sum(!is.na(.)))) %>%
        select(City, num_range("Q", range = 1:30)) %>%
        select(-Q18, -Q10) %>%
        setNames(c("City", paste0(names(.)[-1], "_n")))
    })
  })
  
  
  census_filtered <- reactive({
    input$update_data
    isolate({
      req(selected_gender(), input$city)
      census_data_all %>%
        filter(City %in% input$city, Q2 %in% selected_gender())
    })
  })
  
  pre_snapshot_filtered <- reactive({
    input$update_data
    isolate({
      req(input$city)
      pre_snapshot %>%
        filter(City %in% input$city)
    })
  })
  
  output$downloadDataCensus <- downloadHandler(
    filename = "census_data.csv",
    content = function(file) {
      write.csv(census_filtered(), file, row.names = FALSE)
    }
  )
  
  output$downloadDataVenue <- downloadHandler(
    filename = "venue_data.csv",
    content = function(file) {
      write.csv(pre_snapshot_filtered(), file, row.names = FALSE)
    }
  )
  
  # Census data
  gender_data <- reactive({
    input$update_data
    isolate({
      req(input$city)
      gender_data <- census_data_all %>%
        mutate(Q2 = case_when(Q2 == "Another gender identity" ~ "Another",
                              TRUE ~ Q2)) %>%
        filter(City %in% input$city) %>%
        count(Q2, City) %>%
        na.omit() %>%
        arrange(Q2)
    })
  })
  
  age_data <- reactive({
    census_filtered() %>%
      select(Age, City) %>%
      mutate(
        Age_group = case_when(
          Age > 17 & Age < 30 ~ "18-29",
          Age > 29 & Age < 40 ~ "30-39",
          Age > 39 & Age < 50 ~ "40-49",
          Age > 49 & Age < 60 ~ "50-59",
          Age > 59 & Age < 70 ~ "60-69",
          Age > 69 ~ "70+"
        )) %>%
      na.omit() %>%
      count(Age_group, City)
  })
  
  city_area <- reactive({
    census_filtered() %>%
      count(Q4, City) %>%
      na.omit() %>%
      arrange(Q4)
  })
  
  music_type <- reactive({
    census_filtered() %>%
      select(starts_with("Q12"), City) %>%
      pivot_longer(!City, names_to = "question", values_to = "value") %>%
      na.omit %>%
      count(value, City)
  })
  
  statement_data <-reactive({
    census_filtered() %>%
      select(starts_with("Q17"), City) %>%
      pivot_longer(!City, names_to = "question", values_to = "value") %>%
      na.omit %>%
      count(value, City)
  })
  
  spending_data <- reactive({
    census_filtered() %>%
      select(starts_with("Q6"), City) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("Q6")))) %>%
      select(sums, City) %>%
      na.omit()
  })
  
  spending_on_data <- reactive({
    census_filtered() %>%
      select(input$spend_category, City) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("Q6")))) %>%
      select(sums, City) %>%
      na.omit()
  })
  
  restaurant_data <- reactive({
    census_filtered() %>%
      count(Q7, City) %>%
      na.omit() %>%
      arrange(Q7)
  })
  
  transport_data <- reactive({
    census_filtered() %>%
      select(starts_with("Q8"), City) %>%
      pivot_longer(!City, names_to = "transport", values_to = "value") %>%
      na.omit() %>%
      count(value, City)
  })
  
  night_data <- reactive({
    census_filtered() %>%
      count(Q9, City) %>%
      na.omit() %>%
      arrange(Q9)
  })
  
  scene_data <- reactive({
    census_filtered() %>%
      count(Q11, City) %>%
      na.omit() %>%
      arrange(Q11)
  })
  
  attend_data <- reactive({
    census_filtered() %>%
      count(Q13, City) %>%
      na.omit() %>%
      arrange(Q13)
  })
  
  venue_size_data <- reactive({
    census_filtered() %>%
      mutate(Q14 = case_when(Q14 == "Very small (less than 200)" ~ "Very small (< 200)",
                             TRUE ~ Q14)) %>%
      count(Q14, City) %>%
      na.omit() %>%
      arrange(Q14)
  })
  
  prevent_data <- reactive({
    census_filtered() %>%
      select(starts_with("Q15"), City) %>%
      pivot_longer(!City, names_to = "transport", values_to = "value") %>%
      na.omit() %>%
      count(value, City)
  })
  
  encourage_data <- reactive({
    census_filtered() %>%
      count(Q16, City) %>%
      na.omit()
  })
  
  # Pre snapshot data
  operation_days_data <- reactive({
    pre_snapshot_filtered() %>%
      select(starts_with("Q8"), City) %>%
      pivot_longer(!City, names_to = "question", values_to = "value") %>%
      na.omit %>%
      count(value, City)
  })
  
  negative_effect_data <- reactive({
    pre_snapshot_filtered() %>%
      select(starts_with("Q14"), City) %>%
      select(!Q14_15_TEXT) %>%
      pivot_longer(!City, names_to = "effect", values_to = "value") %>%
      na.omit %>%
      count(value, City)
  })
  
  envi_friendly_data <- reactive({
    pre_snapshot_filtered() %>%
      count(Q15_1, City) %>%
      na.omit %>%
      arrange(Q15_1)
  })
  
  discri_prob_data <- reactive({
    pre_snapshot_filtered() %>%
      count(Q15_2, City) %>%
      na.omit %>%
      arrange(Q15_2)
  })
  
  corrupt_prob_data <-  reactive({
    pre_snapshot_filtered() %>%
      count(Q15_3, City) %>%
      na.omit %>%
      arrange(Q15_3)
  })
  
  optimistic_data <- reactive({
    pre_snapshot_filtered() %>%
      count(Q18, City) %>%
      na.omit %>%
      arrange(Q18)
  })
  # Post snapshot data
  
  # Census plots
  output$Gender <- renderPlot({
    gender_plot_data <- gender_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q2, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    req(nrow(gender_plot_data) > 0)
    unique_q2 <- unique(gender_plot_data$Q2)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q2)), unique_q2)
    p <- ggplot(gender_plot_data, aes(x = "", y = n, fill = Q2)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(gender_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q2) * 0.08, color = Q2), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q2) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 1.5) + theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) + 
      ylim(-1, 0) + scale_color_manual(values = color_palette) +
      facet_wrap(~City, ncol = 1)
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  # output$Age <- renderPlot({
  #   age_plot_data <- age_data() %>%
  #     group_by(City) %>%
  #     mutate(percentage = n / sum(n) * 100) %>%
  #     mutate(City2 = paste0(City, "\nN = ", sum(n)))
  #   # lim <- max(age_plot_data$n)
  #   ggplot(age_plot_data, aes(x = Age_group, y = n)) +
  #     geom_bar(stat = "identity", fill = global_colors[1]) +
  #     geom_text(aes(y = n - n/5, label = paste0(n, "\n", " (", round(age_plot_data$percentage, 1), "%)")),
  #               vjust = -0.1, size = 2.8) +
  #     labs(x = "Age group", y = "Count") + facet_wrap(~City2, ncol = 3, scales = "free_y") + # ylim(0, lim + 10) +
  #     theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) + coord_cartesian(clip = "off")
  # })
  
  output$Age <- renderPlot({
    age_plot_data <- age_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Age_group, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    ggplot(age_plot_data, aes(x = Age_group, y = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], width = 0.5) +
      geom_text(aes(label = paste0(n, " (", round(age_plot_data$percentage, 1), "%)")),
                vjust = -0.8,
                size = 5,
                fontface = "bold") +
      labs(x = "Age group", y = "Count") +
      facet_wrap(~City2, ncol = 3, scales = "free_y") +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5, size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 60))
      ) +
      coord_cartesian(clip = "off")
  })
  
  output$CityArea <- renderPlot({
    city_area_plot_data <- city_area() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q4, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    unique_q4 <- unique(city_area_plot_data$Q4)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q4)), unique_q4)
    # Plots
    p <- ggplot(city_area_plot_data, aes(x = "", y = n, fill = Q4)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(city_area_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q4) * 0.08, color = Q4), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q4) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 2.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      facet_wrap(~City, ncol = 1) +
      scale_color_manual(values = color_palette)
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  
  output$MusicType <- renderPlot({
    music_type_plot_data <- music_type() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q12_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q12_n))
    req(nrow(music_type_plot_data) > 0)
    lim <- max(music_type_plot_data$n)
    ggplot(music_type_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], color = "white", width = 0.75) +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), hjust = 0.1, size = 3.5, fontface = "bold") +
      labs(x = "Count", y = "Category") +
      xlim(0, lim + 20) +
      facet_wrap(~City2, ncol = 3) +
      coord_cartesian(clip = "off") +
      theme(
        axis.text.x = element_text(size = 12, margin = margin(t = 5)),
        axis.text.y = element_text(size = 12, margin = margin(r = 5)),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  output$Statement <- renderPlot({
    statement_plot_data <- statement_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q17_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q17_n))
    req(nrow(statement_plot_data) > 0)
    lim <- max(statement_plot_data$n)
    ggplot(statement_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], color = "white", width = 0.75) +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), hjust = 0.1, size = 3.5, fontface = "bold") +
      labs(x = "Count", y = "Statement") +
      xlim(0, lim + 30) +
      facet_wrap(~City2, ncol = 2) +
      scale_y_discrete(labels = scales::label_wrap(60)) +
      coord_cartesian(clip = "off") +
      theme(
        axis.text.x = element_text(size = 12, margin = margin(t = 5)),
        axis.text.y = element_text(size = 12, margin = margin(r = 5)),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  output$Spending <- renderPlot({
    spending_plot_data <- spending_data() %>%
      left_join(census_n2(), by = "City") %>%
      mutate(City2 = paste0(City, "\nN = ", Q6_n))
    ggplot(spending_plot_data, aes(x = sums, y = after_stat(density))) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      geom_density(size = 1.5, color = "red") +
      labs(x = "Spending", y = "Density") +
      facet_wrap(~City2, ncol = 3) +
      theme(
        axis.text.x = element_text(size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  output$Spending_table <- renderTable({
    spending_data() %>%
      group_by(City) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Spending_on <- renderPlot({
    spending_on_plot_data <- spending_on_data() %>%
      left_join(census_n2(), by = "City") %>%
      mutate(City2 = paste0(City, "\nN = ", Q6_n))
    ggplot(spending_on_plot_data, aes(x = sums, y = ..density..)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      geom_density(size = 1.5, color = "red") +
      labs(x = "Spending", y = "Density") +
      facet_wrap(~City2, ncol = 3) +
      theme(
        axis.text.x = element_text(size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  output$Spending_on_table <- renderTable({
    spending_on_data() %>%
      group_by(City) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Restaurant <- renderPlot({
    restaurant_plot_data <- restaurant_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q7, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    req(nrow(restaurant_plot_data) > 0)
    unique_q7 <- unique(restaurant_plot_data$Q7)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q7)), unique_q7)
    p <- ggplot(restaurant_plot_data, aes(x = "", y = n, fill = Q7)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(restaurant_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q7) * 0.08, color = Q7), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q7) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 2.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      facet_wrap(~City, ncol = 1) +
      scale_color_manual(values = color_palette)
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Transport <- renderPlot({
    transport_plot_data <- transport_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q8_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q8_n))
    # lim <- max(transport_plot_data$n)
    ggplot(transport_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], color = "white") +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), hjust = 0.1, size = 3.5, fontface = "bold") +
      labs(x = "Count", y = "Mode of transport") +
      coord_cartesian(clip = "off") +
      scale_y_discrete(labels = scales::label_wrap(50)) +
      facet_wrap(~City2, ncol = 3, scales = "free_x") +
      theme(
        axis.text.x = element_text(size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  output$Night <- renderPlot({
    night_plot_data <- night_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q9, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    req(nrow(night_plot_data) > 0)
    unique_q9 <- unique(night_plot_data$Q9)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q9)), unique_q9)
    p <- ggplot(night_plot_data, aes(x = "", y = n, fill = Q9)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(night_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q9) * 0.08, color = Q9), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q9) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 1.5) + theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      ylim(-1, 0) + scale_color_manual(values = color_palette) +
      facet_wrap(~City, ncol = 1)
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Scene <- renderPlot({
    scene_plot_data <- scene_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q11, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    req(nrow(scene_plot_data) > 0)
    unique_q11 <- unique(scene_plot_data$Q11)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q11)), unique_q11)
    p <- ggplot(scene_plot_data, aes(x = "", y = n, fill = Q11)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(scene_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q11) * 0.08, color = Q11), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q11) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 4.5) + theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      ylim(-1, 0) + scale_color_manual(values = color_palette) +
      facet_wrap(~City, ncol = 1) +
      coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Attend <- renderPlot({
    attend_plot_data <- attend_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q13, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    req(nrow(attend_plot_data) > 0)
    unique_q13 <- unique(attend_plot_data$Q13)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q13)), unique_q13)
    p <- ggplot(attend_plot_data, aes(x = "", y = n, fill = Q13)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(attend_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q13) * 0.08, color = Q13), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q13) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 2.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      scale_color_manual(values = color_palette) +
      facet_wrap(~City, ncol = 1)
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$VenueSize <- renderPlot({
    venue_size_plot_data <- venue_size_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q14, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    # Setting color mapping
    req(nrow(venue_size_plot_data) > 0)
    unique_q14 <- unique(venue_size_plot_data$Q14)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q14)), unique_q14)
    p <- ggplot(venue_size_plot_data, aes(x = "", y = n, fill = Q14)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(venue_size_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q14) * 0.08, color = Q14), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q14) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 2.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      scale_color_manual(values = color_palette) +
      facet_wrap(~City, ncol = 1) +
      coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Prevent <- renderPlot({
    prevent_plot_data <- prevent_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q15_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q15_n))
    req(nrow(prevent_plot_data) > 0)
    lim <- max(prevent_plot_data$n)
    ggplot(prevent_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], color = "white", width = 0.75) +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), hjust = 0.1, size = 3.5, fontface = "bold") +
      labs(x = "Count", y = "Reason") +
      xlim(0, lim + 20) +
      facet_wrap(~City2, ncol = 3) +
      scale_y_discrete(labels = scales::label_wrap(70)) +
      coord_cartesian(clip = "off") +
      theme(
        axis.text.x = element_text(size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  output$Encourage <- renderPlot({
    encourage_plot_data <- encourage_data() %>%
      group_by(City) %>%
      left_join(census_n2(), by = "City") %>%
      mutate(percentage = n / Q16_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q16_n))
    req(nrow(encourage_plot_data) > 0)
    lim <- max(encourage_plot_data$n)
    ggplot(encourage_plot_data, aes(y = Q16, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], color = "white", width = 0.75) +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), hjust = 0.1, size = 3.5, fontface = "bold") +
      labs(x = "Count", y = "Reason") +
      xlim(0, lim + 20) +
      facet_wrap(~City2, ncol = 3) +
      scale_y_discrete(labels = scales::label_wrap(70)) +
      coord_cartesian(clip = "off") +
      theme(
        axis.text.x = element_text(size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  # Pre snapshot plots
  output$Operation_days <- renderPlot({
    operation_days <- operation_days_data() %>%
      group_by(City) %>%
      left_join(pre_snapshot_n2, by = "City") %>%
      mutate(percentage = n / Q8_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q8_n))
    req(nrow(operation_days) > 0)
    lim <- max(operation_days$n)
    ggplot(operation_days, aes(x = value, y = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], color = "white", width = 0.6) +
      geom_text(aes(label = paste0(n, "\n", "(", round(percentage, 1), "%)")), vjust = -0.5, size = 3.5, fontface = "bold") +
      labs(x = "Weekday", y = "Count") +
      facet_wrap(~City2, ncol = 3) +
      coord_cartesian(clip = "off") +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5, size = 12, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      ) +
      ylim(0, lim + lim * 0.2)
  })
  
  output$Negative_effects <- renderPlot({
    negative_effects_plot_data <- negative_effect_data() %>%
      group_by(City) %>%
      left_join(pre_snapshot_n2, by = "City") %>%
      mutate(percentage = n / Q14_n * 100) %>%
      mutate(City2 = paste0(City, "\nN = ", Q14_n))
    req(nrow(negative_effects_plot_data) > 0)
    lim <- max(negative_effects_plot_data$n)
    ggplot(negative_effects_plot_data, aes(y = value, x = n)) +
      geom_bar(stat = "identity", fill = global_colors[1], color = "white", width = 0.75) +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), hjust = 0.1, size = 3.5, fontface = "bold") +
      labs(x = "Count", y = "Effect") +
      xlim(0, lim + 10) +
      facet_wrap(~City2, ncol = 3) +
      scale_y_discrete(labels = scales::label_wrap(50)) +
      coord_cartesian(clip = "off") +
      theme(
        axis.text.x = element_text(size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2", margin = margin(b = 40))
      )
  })
  
  output$Envi_friendly <- renderPlot({
    envi_friendly_plot_data <- envi_friendly_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q15_1, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    req(nrow(envi_friendly_plot_data) > 0)
    unique_q15_1 <- unique(envi_friendly_plot_data$Q15_1)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q15_1)), unique_q15_1)
    p <- ggplot(envi_friendly_plot_data, aes(x = "", y = n, fill = Q15_1)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(envi_friendly_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q15_1) * 0.08, color = Q15_1), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q15_1) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 2.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      facet_wrap(~City, ncol = 1) +
      scale_color_manual(values = color_palette) +
      coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Discri_prob <- renderPlot({
    discri_prob_plot_data <- discri_prob_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q15_2, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    req(nrow(discri_prob_plot_data) > 0)
    unique_q15_2 <- unique(discri_prob_plot_data$Q15_2)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q15_2)), unique_q15_2)
    p <- ggplot(discri_prob_plot_data, aes(x = "", y = n, fill = Q15_2)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(discri_prob_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q15_2) * 0.08, color = Q15_2), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q15_2) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 4.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      facet_wrap(~City, ncol = 1) +
      scale_color_manual(values = color_palette) +
      coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Corrupt_prob <- renderPlot({
    corrupt_prob_plot_data <- corrupt_prob_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q15_3, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    req(nrow(corrupt_prob_plot_data) > 0)
    unique_q15_3 <- unique(corrupt_prob_plot_data$Q15_3)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q15_3)), unique_q15_3)
    p <- ggplot(corrupt_prob_plot_data, aes(x = "", y = n, fill = Q15_3)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City2, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(corrupt_prob_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q15_3) * 0.08, color = Q15_3), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q15_3) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 4.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      scale_color_manual(values = color_palette) +
      facet_wrap(~City, ncol = 1) +
      coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Optimistic <- renderPlot({
    optimistic_plot_data <- optimistic_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Q18, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(City2 = paste0(City, "\nN = ", sum(n)))
    req(nrow(optimistic_plot_data) > 0)
    unique_q18 <- unique(optimistic_plot_data$Q18)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_q18)), unique_q18)
    p <- ggplot(optimistic_plot_data, aes(x = "", y = n, fill = Q18)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~City, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      )
    p_text <- ggplot(optimistic_plot_data) +
      geom_point(aes(x = 0, y = -seq_along(Q18) * 0.08, color = Q18), size = 5, shape = 15) +
      geom_text(aes(x = 0.2, y = -seq_along(Q18) * 0.08, label = label), hjust = 0, size = 5, fontface = "bold") +
      theme_void() +
      xlim(0, 4.5) + ylim(-1, 0) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold", color = "#4E6AA2")
      ) +
      scale_color_manual(values = color_palette) +
      facet_wrap(~City2, ncol = 1) +
      coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  # Census data tables
  output$Gender_table <- renderTable({
    gender_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Gender` = Q2, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Age_table <- renderTable({
    age_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Age Group` = Age_group, Count = n, `Percentage (%)` = percentage)
  })
  
  output$CityArea_table <- renderTable({
    city_area() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Lives in City Area?` = Q4, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Restaurant_table <- renderTable({
    restaurant_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Eats at Restaurants?` = Q7, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Transport_table <- renderTable({
    transport_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Transport` = value, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Night_table <- renderTable({
    night_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Night Rating` = Q9, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Scene_table <- renderTable({
    scene_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Live Music Scene Rating` = Q11, Count = n, `Percentage (%)` = percentage)
  })
  
  output$MusicType_table <- renderTable({
    music_type() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Music Type` = value, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Attend_table <- renderTable({
    attend_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Events per Month` = Q13, Count = n, `Percentage (%)` = percentage)
  })
  
  output$VenueSize_table <- renderTable({
    venue_size_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Venue Size` = Q14, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Prevent_table <- renderTable({
    prevent_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Reason Prevented` = value, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Encourage_table <- renderTable({
    encourage_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Reason to Encourage` = Q16, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Statement_table <- renderTable({
    statement_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, Statement = value, Count = n, `Percentage (%)` = percentage)
  })
  
  # Venue data tables
  output$Operation_days_table <- renderTable({
    operation_days_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Day of the Week` = value, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Negative_effects_table <- renderTable({
    negative_effect_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Negative Effect` = value, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Envi_friendly_table <- renderTable({
    envi_friendly_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Environmentally Friendly` = Q15_1, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Discri_prob_table <- renderTable({
    discri_prob_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Discrimination Problems?` = Q15_2, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Corrupt_prob_table <- renderTable({
    corrupt_prob_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Corruption Problems?` = Q15_3, Count = n, `Percentage (%)` = percentage)
  })
  
  output$Optimistic_table <- renderTable({
    optimistic_data() %>%
      group_by(City) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      select(City, `Optimistic?` = Q18, Count = n, `Percentage (%)` = percentage)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
