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
library(cowplot)
library(ggh4x)
library(shinyWidgets)
library(shinybusy)
library(ggrepel)
library(ragg)
library(shinyjs)
library(shinyBS)
library(data.table)
# library(thematic)
# library(ragg)


# Set a new global theme for all ggplots: transparent backgrounds
# Set a new global theme for all ggplots: transparent backgrounds & gray gridlines
theme_update(
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.background = element_rect(fill = "transparent", color = NA),
  strip.background = element_blank(),
  
  # === ADD THESE LINES FOR LIGHT GRAY GRIDLINES ===
  panel.grid.major = element_line(color = "#E0E0E0"),
  panel.grid.minor = element_line(color = "#E0E0E0")
)

jsCode <- "
shinyjs.getWidth = function() {
  Shiny.onInputChange('screen_width', window.innerWidth);
}
"

# Loading the data
#culture_survey <- read_sav("12665_2025_06_03_AllResp_spss.sav") %>%
culture_survey <- read_sav("cultural_access_participation.sav") %>%
  # mutate(
  #   "society" = case_when(
  #     M77 < 4 ~ "Low",
  #     M77 > 3 & M77 < 8 ~ "Middle",
  #     M77 > 7 ~ "High"
  #   )) %>%
  # mutate(
  #   "politics" = case_when(
  #     M78 < 5 ~ "Left",
  #     M78 > 4 & M78 < 8 ~ "Middle",
  #     M78 > 7 ~ "Rigth"
  # )) %>%
  rename(M34_1_1 = M34_1t,
         M34_1_2 = M34_2t,
         M34_1_3 = M34_3t,
         M34_2_1 = M34_4t,
         M34_2_2 = M34_5t,
         M34_2_3 = M34_6t,
         M34_3_1 = M34_7t,
         M34_3_2 = M34_8t,
         M34_3_3 = M34_9t,
         M34_4_1 = M34_10t,
         M34_4_2 = M34_11t,
         M34_4_3 = M34_12t,
         M34_5_1 = M34_13t,
         M34_5_2 = M34_14t,
         M34_5_3 = M34_15t,
         M34_6_1 = M34_16t,
         M34_6_2 = M34_17t,
         M34_6_3 = M34_18t,
         M34_7_1 = M34_19t,
         M34_7_2 = M34_20t,
         M34_7_3 = M34_21t,
         M34_8_1 = M34_22t,
         M34_8_2 = M34_23t,
         M34_8_3 = M34_24t)

# Adding duplicate of the data for all cities plots
all_countries_culture_survey <- culture_survey %>%
  mutate(S0 = "All Countries")
culture_survey <- bind_rows(culture_survey %>%
                              mutate(S0 = haven::as_factor(S0)), all_countries_culture_survey) %>%
  haven::as_factor()

culture_nr <- as.data.table(haven::as_factor(culture_survey))

drop_cols <- c("M31_6t", "S8_9t", "M22_9t", "M42_11", "M42_11t", "M44_6", "M44_6t", "M70_21t")
culture_nr[, (drop_cols) := NULL]

prefix_map <- c(
  M79 = "M79", M74 = "M74", M73 = "M73", M71 = "M71", M70 = "M70",
  M69 = "M69", M63 = "M63_", M62 = "M62", M61 = "M61", M57 = "M57",
  M56 = "M56", M55 = "M55", M54 = "M54", M51 = "M51", M50 = "M50",
  M49 = "M49", M47 = "M47", M46 = "M46", M45 = "M45", M44 = "M44",
  M43 = "M43", M42 = "M42", M39 = "M39", M38 = "M38", M37 = "M37",
  M36 = "M36", M35 = "M35", M34 = "M34_", M33 = "M33", M31 = "M31",
  M29 = "M29", M26 = "M26", M25 = "M25", M24 = "M24", M22 = "M22",
  M18 = "M18", M14 = "M14", S8  = "S8",  S7  = "S7",  S6  = "S6"
)

col_names <- names(culture_nr)

for (new_col in names(prefix_map)) {
  prefix   <- prefix_map[[new_col]]
  src_cols <- col_names[startsWith(col_names, prefix)]
  
  if (length(src_cols) == 0) next
  if (length(src_cols) == 1) {
    culture_nr[, (new_col) := fifelse(!is.na(get(src_cols)), 1L, NA_integer_)]
  } else {
    culture_nr[, (new_col) := fifelse(
      Reduce(`|`, lapply(.SD, Negate(is.na))),
      1L, NA_integer_
    ), .SDcols = src_cols]
  }
}

# Evaluation tables
evaluation_table_data <- read.csv("country_tables.csv") %>%
  rename("Av. Expenses" = "Av..Expenses", "Num visits (M)" = "Num.visits..M.", 
         "Yearly tot expenditure (M)" = "Yearly.tot.expenditure..M.",
         "Value added contribution" = "Value.added.contribution", 
         "Employment contribution" = "Employment.contribution")

# The color used by the barplots
# We define the base colors first
raw_colors <- c("#007CBB", "#D95D39", "azure4", "#339FD6", "#B0412D", "#66B8E1", "#D88A77")

# Then we apply 70% opacity (0.7) to them globally using scales::alpha
# This makes the bars lighter so the black text pops out
global_colors <- scales::alpha(raw_colors, 0.6)

# options(shiny.useragg = TRUE)
#
# thematic_shiny(font = "auto")

# Defining the ui for the application
ui <- fluidPage(
  
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "full-page",
                              color = global_colors[1]),
  
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("getWidth")),
  tags$script("$(document).on('shiny:connected', function() { shinyjs.getWidth(); $(window).resize(function() { shinyjs.getWidth(); }); });"),
  
  tags$head(
    HTML("<title>OpenMusE Dashboard</title>"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles_mobile_tables.css"),
    
    tags$style(HTML("
      .shiny-busy-full-page {
        background-color: rgba(255, 255, 255, 0.5) !important; 
      }
    ")),
    
    tags$script(HTML("
    $(document).ready(function() {
      $('[data-toggle=\"popover\"]').popover();
    });
  "))
    
  ),
  
  # tags$head(HTML("<title>OpenMusE Dashboard</title>"),
  #           tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  #'   tags$head(
  #'     HTML("<title>OpenMusE Dashboard</title>"),
  #'     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  #'     
  #'     # ---- add this block ----
  #'     tags$style(HTML("
  #'       /* fix the toggle button in the top‐left corner */
  #'       #toggle_sidebar {
  #'         position: fixed !important;
  #'         top: 10px;
  #'         left: 10px;
  #'         z-index: 1000;
  #'       }
  #'       /* fix the sidebar itself, full‐height and scrollable */
  #'       #sidebar {
  #'         position: fixed !important;
  #'         top: 50px;        /* leave room for the toggle button */
  #'         bottom: 0;
  #'         width: 25%;       /* matches col-sm-3 (3/12 = 25%) */
  #'         overflow-y: auto;
  #'         background: #fff; /* ensure it sits above content */
  #'         z-index: 900;
  #'       }
  #'       /* push the main content over so it never sits beneath the sidebar */
  #'       #main {
  #'         margin-left: 25% !important;
  #'         width:        75% !important;
  #'         transition:   margin-left .3s, width .3s;
  #'       }
  #'       #main.full-width {
  #'         margin-left: 0     !important;
  #'         width:        100% !important;
  #'       }
  #'       /* helper classes added/removed by JS */
  #'       #sidebar.hidden { display: none !important; }
  #'       #main.full-width { margin-left: 0 !important; }
  #'       /* Change background and text color of tab headers */
  #'   .nav-tabs > li > a {
  #'     background-color: #fff;
  #'     color: #333;
  #'     font-family: 'Roboto', sans-serif;
  #'     font-weight: bold;
  #'     font-size: 16px;
  #'   }
  #' 
  #'   /* Active tab styling */
  #'   .nav-tabs > li.active > a,
  #'   .nav-tabs > li.active > a:focus,
  #'   .nav-tabs > li.active > a:hover {
  #'     background-color: #253471;
  #'     color: white;
  #'     border: 1px solid #253471;
  #'   }
  #' 
  #'   /* Hover effect */
  #'   .nav-tabs > li > a:hover {
  #'     background-color: #e6e6e6;
  #'     color: #000;
  #'   }
  #' 
  #'   h2 {
  #'   font-size: 26px !important;
  #'   font-weight: 600 !important; /* Semi-bold */
  #'   color: #253471 !important;
  #'  }
  #' 
  #'   @media (max-width: 768px) {
  #'   /* Stack layout on mobile */
  #'   #sidebar {
  #'     position: relative !important;
  #'     width: 100% !important;
  #'     top: 0;
  #'     z-index: 1000;
  #'   }
  #' 
  #'   #main {
  #'     margin-left: 0 !important;
  #'     width: 100% !important;
  #'   }
  #' 
  #'   #toggle_sidebar {
  #'     top: 10px;
  #'     left: 10px;
  #'     z-index: 1100;
  #'   }
  #' 
  #'   /* Hide sidebar by default on mobile */
  #'   #sidebar.hidden {
  #'     display: none !important;
  #'   }
  #' 
  #'   #main.full-width {
  #'     margin-left: 0 !important;
  #'     width: 100% !important;
  #'   }
  #' }
  #'     "))
  #'   ),
  
  ## now your toggle button and row/columns
  # div(
  #   actionButton(
  #     inputId = "toggle_sidebar",
  #     label   = "Toggle Inputs",
  #     onclick = "
  #       // hide/show sidebar
  #       $('#sidebar').toggleClass('hidden');
  #       // expand/collapse main content
  #       $('#main').toggleClass('full-width');
  #     "
  #   ),
  #   style = "margin: 10px 0;"
  # ),
  
  # div(
  #   actionButton(
  #     inputId = "toggle_sidebar",
  #     label   = "Toggle Inputs",
  #     onclick = "
  #       $('#sidebar').toggle();
  #       $('#main').toggleClass('col-sm-9 col-sm-12');
  #     "
  #   ),
  #   style = "margin: 10px 0;"
  # ),
  
  fluidRow(
    column(
      width = 3,
      id    = "sidebar",
      card(
        card_header("Input"),
        
        pickerInput(
          inputId  = "country",
          label    = "Country:",
          choices  = c("Germany", "Spain", "France", "Italy", "Poland", "All Countries"),
          multiple = TRUE, selected = "Germany",
          options  = list(container = "body")
        ),
        
        pickerInput(
          inputId  = "gender",
          label    = "Gender:",
          choices  = c("Total", "Female", "Male", "Another" = "Another gender identity", "Don't want to say"),
          multiple = TRUE, selected = "Total",
          options  = list(container = "body")
        ),
        
        div(
          actionButton(
            inputId = "update_data",
            label   = "Update Data",
            icon    = icon("sync"),
            width   = "100%"
          ),
          style = "margin-top: 15px;"
        ),
        
        downloadButton("downloadDataCap", "Download data"),
        
        div(
          actionButton(
            inputId = "expand_plots",
            label   = "Expand plots",
            icon    = icon("expand"),
            width   = "100%",
            # This JS simulates a click on the native bslib button
            onclick = "$('#main .bslib-full-screen-enter').click();" 
          ),
          style = "margin-top: 10px;"
        )
      )
    ),
    
    ## Main content
    column(
      width = 9,
      id    = "main",
      card(
        full_screen = TRUE,
        card_header("Cultural Participation Survey Plots"),
        
        tabsetPanel(
          
          tabPanel("Basic demographics",
                   ## Gender & Age
                   h2("Q3: Gender distribution"),
                   plotOutput("Gender", width = "100%", height = "600px"),
                   h2("Q4: Age distribution"),
                   plotOutput("Age", width = "100%"),
                   
                   ## Participation counts
                   h2("Q6: How many times in the last twelve months have you..?"),
                   plotOutput("Culture_participitation", width = "100%", height = "900px"),
                   h2("Q7: How many times in the last twelve months have you..?"),
                   plotOutput("Live_attend", width = "100%", height = "900px"),
                   h2("Q8: Have you done the following activity in the last 12 months?"),
                   plotOutput("Culture_done", width = "100%", height = "600px"),
                   
                   ## Income & Quality
                   h2("Q9: Over the last 12 months, have you earned any income from activity involving music?"),
                   plotOutput("Earn_income_music", width = "100%", height = "600px"),
                   h2("Q10: How important music is for quality of life?"),
                   plotOutput("Music_quality", width = "100%", height = "600px")
          ),
          
          tabPanel("Engaging with live music",
                   h2("Q11: How important are live music events for your?"),
                   plotOutput("Live_Music_quality", width = "100%", height = "600px"),
                   ## Last event & type
                   h2("Q12: When was the last time you attended a live music event?"),
                   plotOutput("Last_event", width = "100%", height = "600px"),
                   h2("Q13: What was the last type of live music event you attended?"),
                   plotOutput("Event_type", width = "100%", height = "800px"),
                   
                   ## Attendance details
                   h2("Q14: For how many days did you attend the music festival, culture festival, fair, or other public event with live music?"),
                   plotOutput("Days_attended", width = "100%", height = "600px"),
                   h2("Q15: Who did you go with?"),
                   plotOutput("Who_go_with", width = "100%", height = "600px"),
                   h2("Q16: Was it required to buy a ticket for the event?"),
                   plotOutput("Ticket_required", width = "100%", height = "600px"),
                   h2("Q17: Did you pay for your ticket, or did someone else?"),
                   plotOutput("Who_paid", width = "100%", height = "600px"),
                   
                   ## Tickets & venue
                   h2("Q18: How many tickets did you buy for the event?"),
                   plotOutput("Ticket_number", width = "100%"),
                   h2("Total spending while out for live music event"),
                   tableOutput("Spending_table"),
                   plotOutput("Spending", width = "100%"),
                   ## Spending breakdown
                   h2("Q18/Q19/Q21: Spending on the following while out for live music event:"),
                   shinyWidgets::pickerInput(
                     inputId  = "spend_category",
                     label    = NULL,
                     choices  = c(
                       "Ticket"                   = "M18_1t_eur_mvr",
                       "Local transport"          = "M19_1t_eur_mvr",
                       "Long-distance transport"  = "M21_1t_eur_mvr",
                       "Accommodation"            = "M21_2t_eur_mvr",
                       "Food/drink at venue"      = "M19_2t_eur_mvr",
                       "Food/drink other"         = "M19_3t_eur_mvr",
                       "Merchandise - recordings" = "M19_4t_eur_mvr",
                       "Merchandise - apparel"    = "M19_5t_eur_mvr",
                       "Merchandise - other"      = "M19_6t_eur_mvr"
                     ),
                     multiple = TRUE,
                     selected = "M18_1t_eur_mvr"
                   ),
                   tableOutput("Spending_on_table"),
                   plotOutput("Spending_on", width = "100%"),
                   
                   h2(
                     "Total value of live music",
                     tags$span(
                       icon("circle-info"),
                       id = "info-icon",
                       style = "color:#007bff; cursor:pointer; margin-left:6px;",
                       `data-toggle` = "popover",
                       `data-trigger` = "click",
                       `data-placement` = "right",
                       `data-title` = "Methodology",
                       `data-content` = "The methodology used to calculate the values differs slightly between the countries.",
                       `data-container` = "body"
                     )
                   ),
                   
                   uiOutput("country_tables"),
                   
                   
                   h2("Q20: Was this event held in your home city or town?"),
                   plotOutput("Where_event", width = "100%", height = "600px"),
                   
                   ## Music type & diversity
                   h2("Q22: What best describes the music you saw?"),
                   plotOutput("Music_type", width = "100%", height = "600px"),
                   h2("Q23: Were there any female or gender-diverse artists on stage?"),
                   plotOutput("Gender_diverse", width = "100%", height = "600px"),
                   h2("Q24: Were any of the artists from...?"),
                   plotOutput("Artists_from", width = "100%", height = "600px"),
                   h2("Q25: Questions about traditional folk music or world music performances"),
                   plotOutput("Traditional_questions", width = "100%", height = "600px"),
                   h2("Q26: Questions about environmental sustainability of the event"),
                   plotOutput("Envi_questions", width = "100%", height = "600px"),
                   h2("Q27: How would you rate the live music scene?"),
                   plotOutput("Music_scene_rating", width = "100%", height = "650px"),
                   h2("Q28: In what size of venue do you most enjoy seeing live music?"),
                   plotOutput("Venue_size", width = "100%", height = "650px"),
                   ## Monthly spend summary
                   h2("Q29: Total spending related to live music events in the last month:"),
                   tableOutput("Spending_month_table"),
                   plotOutput("Spending_month", width = "100%"),
                   h2("Q29: Spending on the following in the last month:"),
                   shinyWidgets::pickerInput(
                     inputId  = "spend_category_month",
                     label    = NULL,
                     choices  = c(
                       "Tickets to live music events" = "M29_1t",
                       "Food and drink at live music events" = "M29_2t",
                       "Merchandise at live music events"    = "M29_3t"
                     ),
                     multiple = TRUE,
                     selected = "M29_1t"
                   ),
                   tableOutput("Spending_on_month_table"),
                   plotOutput("Spending_on_month", width = "100%"),
                   
                   h2("Q31: Why haven't you gone to more live music events in the last 12 months?"),
                   plotOutput("More_events", width = "100%", height = "650px"),
                   h2("Which one thing would most encourage you to see live music more often?"),
                   plotOutput("Encourage", width = "100%", height = "650px")
          ),
          
          tabPanel("Engaging with recorded music",
                   
                   ## Listening habits & background music
                   h2("Q33: Sources you use to listen to music in different places:"),
                   shinyWidgets::pickerInput(
                     inputId  = "place",
                     label    = NULL,
                     choices  = c(
                       "At home"        = "1",
                       "At work/school" = "2",
                       "While traveling"= "3",
                       "None of these"  = "4"
                     ),
                     selected = "1"
                   ),
                   plotOutput("Listen_where", width = "100%", height = "600px"),
                   
                   h2("Q34: Minutes spent listening to recorded music yesterday:"),
                   fluidRow(
                     column(
                       width = 6,
                       shinyWidgets::pickerInput(
                         inputId  = "place2",
                         label    = NULL,
                         choices  = c(
                           "At home"        = "1",
                           "At work/school" = "2",
                           "While traveling"= "3"
                         ),
                         selected = "1"
                       )
                     ),
                     column(
                       width = 6,
                       shinyWidgets::pickerInput(
                         inputId  = "medium",
                         label    = NULL,
                         choices  = c(
                           "Streaming service" = "1",
                           "YouTube"           = "2",
                           "Downloaded music"  = "3",
                           "CD"                = "4",
                           "Cassette tapes"    = "5",
                           "LPs"               = "6",
                           "Radio"             = "7",
                           "Television"        = "8"
                         ),
                         selected = "1"
                       )
                     )
                   ),
                   tableOutput("Listen_where_amount_table"),
                   plotOutput("Listen_where_amount", width = "100%", height = "600px"),
                   
                   h2("Q35: Yesterday, did you visit following places where live music is played in the background?"),
                   plotOutput("Background", width = "100%", height = "650px"),
                   h2("Q36: Minutes spent in places with background music:"),
                   shinyWidgets::pickerInput(
                     inputId  = "background",
                     label    = NULL,
                     choices  = c(
                       "Restaurants"     = "M36_1t",
                       "Cafés"           = "M36_2t",
                       "Bars or pubs"    = "M36_3t",
                       "Hotels or hostels" = "M36_4t",
                       "Retail shops"    = "M36_5t",
                       "Gyms, fitness centres" = "M36_6t",
                       "Dance studios"   = "M36_7t"
                     ),
                     selected = "M36_3t"
                   ),
                   tableOutput("Background_amount_table"),
                   plotOutput("Background_amount", width = "100%", height = "650px"),
                   
                   ## Monthly music spend
                   h2("Q37; Total spending on music & streaming in the last month:"),
                   tableOutput("Spending_month_listen_table"),
                   plotOutput("Spending_month_listen", width = "100%"),
                   h2("Q37: Spending breakdown:"),
                   shinyWidgets::pickerInput(
                     inputId  = "spend_category_month_listen",
                     label    = NULL,
                     choices  = c(
                       "Physical music recordings"             = "M37_1t",
                       "Music streaming service subscriptions" = "M37_2t",
                       "Video streaming service subscription"  = "M37_3t",
                       "Music file downloads"                  = "M37_4t"
                     ),
                     multiple = TRUE,
                     selected = "M37_1t"
                   ),
                   tableOutput("Spending_on_month_listen_table"),
                   plotOutput("Spending_on_month_listen", width = "100%"),
                   ## AI & discovery
                   h2("Q38: Supporting an artist in the last 12 months"),
                   plotOutput("Artist_support", width = "100%", height = "650px"),
                   h2("Q39: Have you listened to AI-generated music?"),
                   plotOutput("Ai_music", width = "100%", height = "650px"),
                   h2("Q40: Compensating human artists in AI training"),
                   plotOutput("Human_compensated", width = "100%", height = "650px")
          ),
          
          tabPanel("Music discovery / education",
                   ## Discovery
                   h2("Q41: Importance of discovering new music"),
                   plotOutput("Discover_music", width = "100%", height = "650px"),
                   h2("Q42: Importance of different discovery sources"),
                   plotOutput("Discovey_sources", width = "100%", height = "800px"),
                   h2("Q43: How likely are you to listen to music in the following languages?"),
                   plotOutput("Music_language", width = "100%", height = "650px"),
                   h2("Q44: What are the reasons why you are unlikely to listen to music in European languages other than your own or English?"),
                   plotOutput("No_unfamiliar", width = "100%", height = "650px"),
                   ## Streaming & recommendations
                   h2("Q45: When using the streaming service, what kind of playlists provided by the service do you use?"),
                   plotOutput("Playlists", width = "100%", height = "650px"),
                   h2("Q46:	In a typical week, how often do you…"),
                   plotOutput("Weekly_discovery", width = "100%", height = "650px"),
                   h2("Q47:	When using the streaming service how easy would you say it is for you to…"),
                   plotOutput("Difficulty_finding", width = "100%", height = "650px"),
                   h2("Q48: Typically, how satisfied are you with the automated music recommendations you receive in the streaming service?"),
                   plotOutput("Recommendation_satisfaction", width = "100%", height = "600px"),
                   h2("Q49: To what extent do you agree with the following statements about automated recommendations in the streaming service? Thanks to automated recommendations…"),
                   plotOutput("Recommendation_statements", width = "100%", height = "800px"),
                   ## Education & attitudes
                   h2("Q50: Did your school education include classes on music (musical instruments, singing, music history or theory, etc.)?"),
                   plotOutput("School_music", width = "100%", height = "650px"),
                   h2("Q51: Did your classes on music include material on any of the following?"),
                   plotOutput("School_content", width = "100%", height = "650px"),
                   h2("Q52: Did you take classes on music in [VOCATIONAL SCHOOL / UNIVERSITY]?"),
                   plotOutput("Uni_classes", width = "100%", height = "650px"),
                   h2("Q53: If you plan to attend vocational school or university, do you plan to take classes on music?"),
                   plotOutput("Uni_classes_plan", width = "100%", height = "650px"),
                   h2("Q54: Have you ever studied music outside of school (musical instruments, singing, music history or theory, etc.)?"),
                   plotOutput("Studied_outside", width = "100%", height = "650px"),
                   ## Music study spend
                   h2("Q55: Over the last 12 months, about how much money have you spend on studying music outside of school?"),
                   tableOutput("Spending_study_table"),
                   plotOutput("Spending_study", width = "100%"),
          ),
          
          tabPanel("Sociocultural aspects of music",
                   
                   ## Personal & cultural statements
                   h2("Q56: Do you agree or disagree with the following statements about music in your personal life?"),
                   plotOutput("Statement_personal", width = "100%", height = "800px"),
                   h2("Q57: And do you agree or disagree with the following statements about music in social, cultural, and political life?"),
                   plotOutput("Statement_culture", width = "100%", height = "800px")
          ),
          
          tabPanel("Music and cultural policy",
                   
                   ## Sector & policy
                   h2("Q58: On a scale of 1 to 5, how informed do you feel about the music sector?"),
                   plotOutput("Music_sector", width = "100%", height = "650px"),
                   h2("Q59: On a scale of 1 to 5, how informed do you feel about cultural policy?"),
                   plotOutput("Cultural_policy", width = "100%", height = "650px"),
                   h2("Q60: On a scale of 1 to 5, what do you think about using public funding to support music and other cultural activities?"),
                   plotOutput("Cultural_funding", width = "100%", height = "650px"),
                   h2("Q61: And do you agree or disagree with the following statements about public funding for cultural activities? Public funding should…"),
                   plotOutput("Culture_funding_statement", width = "100%", height = "800px"),
                   
                   ## Quotas & measures
                   h2("Q62: Another important part of cultural policy is broadcasting quotas: rules that ensure a minimum amount of certain kinds of content appears on the radio and television. Would you oppose or support setting the following quotas? A quota on…"),
                   plotOutput("Quotas", width = "100%", height = "650px"),
                   h2("Q63: A range of other cultural policy measures have been tried in different countries. Would you oppose or support trying the following cultural policy measures?"),
                   plotOutput("Policy_measures", width = "100%", height = "750px")
          ),
          tabPanel("Music-making",
                   
                   ## Musician identity & occupation
                   h2("Q64: You mentioned that you’ve played a musical instrument or sung in the last 12 months. Do you consider yourself…"),
                   plotOutput("Musician_level", width = "100%", height = "650px"),
                   h2("Q68: If you had the opportunity, would you pursue music as your main or only occupation?"),
                   plotOutput("Main_occupation", width = "100%", height = "650px"),
                   
                   ## Instrument & equipment spending
                   h2("Q69: Average yearly spending on instruments/lessons/etc."),
                   tableOutput("Yearly_instrument_total_table"),
                   plotOutput("Yearly_instrument_total", width = "100%"),
                   h2("Q69: In a typical year, about how much money do you spend on the following?"),
                   shinyWidgets::pickerInput(
                     inputId  = "yearly_instrument",
                     label    = NULL,
                     choices  = c(
                       "Instruments & equipment"           = "M69_1t",
                       "Lessons & materials"               = "M69_2t",
                       "Other music services (studio rental, memberships, etc.)" = "M69_3t"
                     ),
                     multiple = TRUE,
                     selected = "M69_1t"
                   ),
                   tableOutput("Yearly_instrument_table"),
                   plotOutput("Yearly_instrument", width = "100%"),
                   
                   ## Income activities & totals
                   h2("Q70: Over the last 12 months, have you earned any income from any of the following activities?"),
                   plotOutput("Income_activity", width = "100%", height = "1200px"),
                   h2("Q71: Over the last 12 months, average income from music related activities?"),
                   tableOutput("Income_yearly_table"),
                   plotOutput("Income_yearly", width = "100%"),
                   h2("Q71: Over the last 12 months, around how much income have you earned from the following activities?"),
                   shinyWidgets::pickerInput(
                     inputId  = "income_yearly",
                     label    = NULL,
                     choices  = c(
                       "Singing"                 = "M71_1t",
                       "Instrument playing"      = "M71_2t",
                       "DJing"                   = "M71_3t",
                       "Composing/arranging"     = "M71_4t",
                       "Lyric writing"           = "M71_5t",
                       "Producing/mixing"        = "M71_6t",
                       "Sound engineering"       = "M71_7t",
                       "Visual engineering"      = "M71_8t",
                       "Live-event support"      = "M71_9t",
                       "Venue staff"             = "M71_10t",
                       "Making/selling goods"    = "M71_11t",
                       "Music services"          = "M71_12t",
                       "Other music org"         = "M71_13t",
                       "Music projects"          = "M71_14t",
                       "Media supervision"       = "M71_15t",
                       "Playlist curation"       = "M71_16t",
                       "Teaching in school"      = "M71_17t",
                       "Teaching outside school" = "M71_18t",
                       "Music therapy"           = "M71_19t",
                       "Social media activities" = "M71_20t"
                     ),
                     multiple = TRUE,
                     selected = "M71_1t"
                   ),
                   tableOutput("Income_from_yearly_table"),
                   plotOutput("Income_from_yearly", width = "100%"),
                   h2("Q73: And about what percentage is that of your total personal income over the last 12 months?"),
                   plotOutput("Total_income", width = "100%"),
                   
                   ## Demographics & socio-political
                   h2("Q74: Do you consider any of these to be your “main” professions?"),
                   plotOutput("Profession", width = "100%", height = "800px"),
                   
          ),
          tabPanel("Extended demographics",
                   
                   h2("Q75: Which of these best describes your own current situation?"),
                   plotOutput("Marital_status", width = "100%"),
                   h2("Q76: During the last twelve months, how often have you had difficulties in paying your bills at the end of the month…?"),
                   plotOutput("Paying_difficulty", width = "100%"),
                   h2("Q77: Could you please tell me where you would place yourself on the following scale? Where '1' corresponds to 'the lowest level in society' and '10' corresponds to 'the highest level in society.'"),
                   plotOutput("Society_place", width = "100%"),
                   h2("Q78: In politics people sometimes talk of 'left' and 'right'. Where would you place yourself on this scale, where 0 means the left and 10 means the right?"),
                   plotOutput("Political_scale", width = "100%"),
                   h2("Q79. How much like you is this person?"),
                   plotOutput("Person_description", width = "100%", height = "1200px")
          )
          
        )  # end of tabsetPanel
        
      )  # end of content card
    )    # end of main column
    
  )      # end of fluidRow
)        # end of fluidPage


# Define server logic
server <- function(input, output) {
  
  selected_gender <- reactive({
    if ("Total" %in% input$gender) {
      c("Male", "Female", "Another gender identity", "Don't want to say")
    } else {
      input$gender
    }
  })
  
  culture_n <- reactive({
    input$update_data
    isolate({
      req(selected_gender())
      
      m_cols <- c("M79", "M74", "M73", "M71", "M70", "M69", "M63", "M62", "M61", "M57",
                  "M56", "M55", "M54", "M51", "M50", "M49", "M47", "M46", "M45", "M44",
                  "M43", "M42", "M39", "M38", "M37", "M36", "M35", "M34", "M33", "M31",
                  "M29", "M26", "M25", "M24", "M22", "M18", "M14", "S8", "S7", "S6")
      
      culture_nr[
        S3 %in% selected_gender(),                          
        lapply(.SD, function(x) sum(!is.na(x))),            
        by = S0,                                            
        .SDcols = m_cols
      ][, c("S0", m_cols), with = FALSE]                  
    })
  })
  
  culture_survey_filt <- reactive({
    input$update_data
    isolate({
      req(input$country, selected_gender())
      
      as.data.table(culture_survey)[S0 %in% input$country & S3 %in% selected_gender()]
    })
  })
  
  evaluation_table_filt <- reactive({
    input$update_data
    isolate({
      req(input$country)
      if ("All Countries" %in% input$country) {
        evaluation_table_data 
      } else {
        evaluation_table_data %>%
          filter(Country %in% input$country)
      }
    })
  })
  
  output$downloadDataCap <- downloadHandler(
    filename = "cultural_access_participitation.csv",
    content = function(file) {
      write.csv(culture_survey_filt(), file, row.names = FALSE)
    }
  )
  
  # Census data
  gender_data <- reactive({
    input$update_data
    isolate({
      req(input$country)
      culture_survey %>%
        mutate(S3 = case_when(S3 == "Another gender identity" ~ "Another",
                              TRUE ~ S3)) %>%
        filter(S0 %in% input$country) %>%
        count(S3, S0) %>%
        na.omit() %>%
        arrange(S3)
    })
  })
  
  age_data <- reactive({
    culture_survey_filt() %>%
      count(SREC_Age, S0) %>%
      na.omit()
    
  })
  
  # culture_participitation_data <- reactive({
  #   req(input$country, input$gender, input$culture_part)
  #   culture_participitation_data <- culture_survey %>%
  #     haven::as_factor() %>%
  #     filter(S0 %in% input$country, S3 %in% input$gender) %>%
  #     count(!!sym(input$culture_part), S0) %>%
  #     na.omit() %>%
  #     rename(S6 = !!sym(input$culture_part))
  # })
  
  culture_participitation_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("S6"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "S6_1" = "Seen a ballet, a dance performance or an opera",
                             "S6_2" = "Been to the cinema",
                             "S6_3" = "Been to the theatre",
                             "S6_4" = "Been to a concert",
                             "S6_5" = "Visited a public library",
                             "S6_6" = "Visited a historical monument or site",
                             "S6_7" = "Visited a museum or gallery",
                             "S6_8" = "Watched or listened to a cultural programme on TV, on the radio or in a podcast",
                             "S6_9" = "Read a book"))
  })
  
  # live_attend_data <- reactive({
  #   req(input$country, input$gender, input$live_attend)
  #   live_attend_data <- culture_survey %>%
  #     haven::as_factor() %>%
  #     filter(S0 %in% input$country, S3 %in% input$gender) %>%
  #     count(!!sym(input$live_attend), S0) %>%
  #     na.omit() %>%
  #     rename(S7 = !!sym(input$live_attend))
  # })
  
  live_attend_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("S7"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "S7_1" = "Been to a DJ night at a nightclub, bar, disco, etc.",
                             "S7_2" = "Been to bar or pub with live music",
                             "S7_3" = "Been to a restaurant with live music",
                             "S7_4" = "Been to a music festival",
                             "S7_5" = "Been to a culture festival, fair, or other public event with live music",
                             "S7_6" = "Been to a dinner show, variety show, theatre performance, etc. with live music",
                             "S7_7" = "Been to a private event with live music",
                             "S7_8" = "Live-streamed a music performance online",
                             "S7_9" = "Attended a virtual music event in the Metaverse/in augmented or virtual reality"))
  })
  
  culture_done_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("S8"), S0) %>%
      select(-c(S8_9, S8_9t)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "S8_1" = "Played a musical instrument",
                             "S8_2" = "Sung",
                             "S8_3" = "Acted on the stage or in a film",
                             "S8_4" = "Danced",
                             "S8_5" = "Written poem/essay/novel/etc",
                             "S8_6" = "Made a film",
                             "S8_7" = "Done sculpture/painting/handicrafts/drawing",
                             "S8_8" = "Creative computing (designing websites/blogs etc)",
                             "S8_10" = "None",
                             "S8_11" = "Don't know"))
  })
  
  earn_income_music_data <-reactive({
    culture_survey_filt()%>%
      count(S9, S0) %>%
      na.omit()
  })
  
  last_event_data <-reactive({
    culture_survey_filt() %>%
      count(M12, S0) %>%
      na.omit()
  })
  
  music_quality_live_data <- reactive({
    culture_survey_filt() %>%
      count(S10, S0) %>%
      na.omit()
  })
  
  live_music_quality_data <- reactive({
    culture_survey_filt() %>%
      count(M11, S0) %>%
      mutate(M11 = case_when(
        M11 == " 1 Not very important – honestly, I can do without live music" ~ " 1 Not very important",
        M11 == " 5  Very important –  I don’t want to imagine a life without live music" ~ " 5 Very important",
        TRUE ~ M11
      )) %>%
      na.omit()
  })
  
  type_last_event_data <- reactive({
    culture_survey_filt() %>%
      count(M13, S0) %>%
      na.omit()
  })
  
  days_attended_data <- reactive({
    culture_survey_filt() %>%
      count(M14_1t, S0) %>%
      na.omit()
  })
  
  who_go_with_data <- reactive({
    culture_survey_filt() %>%
      count(M15, S0) %>%
      na.omit()
  })
  
  ticket_required_data <- reactive({
    culture_survey_filt() %>%
      count(M16, S0) %>%
      na.omit()
  })
  
  who_paid_data <- reactive({
    culture_survey_filt() %>%
      count(M17, S0) %>%
      na.omit()
  })
  
  ticket_number_data <- reactive({
    culture_survey_filt() %>%
      count(M18_2t, S0) %>%
      na.omit()
  })
  
  where_event_was_data <- reactive({
    culture_survey_filt() %>%
      count(M20, S0) %>%
      na.omit()
  })
  
  music_type_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M22"), S0, S3) %>%
      select(-c(S3, M22_9, M22_9t)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M22_1" = "Western classical music",
                             "M22_2" = "Jazz",
                             "M22_3" = "Traditional folk music from country",
                             "M22_4" = "Traditional folk music from abroad",
                             "M22_5" = "Popular music (pop/rock/etc)",
                             "M22_6" = "Electronic dance music",
                             "M22_7" = "World music",
                             "M22_8" = "Other hyprid styles"))
  })
  
  gender_diverse_data <- reactive({
    culture_survey_filt() %>%
      count(M23, S0) %>%
      na.omit()
  })
  
  artists_from_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M24"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M24_1" = "Country",
                             "M24_2" = "Another European country",
                             "M24_3" = "Country outside of Europe"))
  })
  
  traditional_questions_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M25"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M25_1" = "The event featured performers from the cultural group, whose music was played",
                             "M25_2" = "The event left me with a positive impression",
                             "M25_3" = "There was informational material available about the music and its cultural context"))
  })
  
  envi_questions_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M26"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M26_1" = "Disposable cups and/or plates were used",
                             "M26_2" = "Water in disposable bottles was sold",
                             "M26_3" = "Waste could be recycled/sorted",
                             "M26_4" = "The event was advertised as environmentally sustainable",
                             "M26_5" = "The event featured messages about environmental sustainability"))
  })
  
  music_scene_rating_data <- reactive({
    culture_survey_filt() %>%
      count(M27, S0) %>%
      na.omit()
  })
  
  venue_size_data <- reactive({
    culture_survey_filt() %>%
      count(M28, S0) %>%
      na.omit()
  })
  
  spending_data <- reactive({
    culture_survey_filt() %>%
      filter(!if_all(c(M18_1t_eur_mvr, ends_with("eur_mvr")), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(c(M18_1t_eur_mvr, ends_with("eur_mvr"))), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  spending_on_data <- reactive({
    culture_survey_filt() %>%
      select(input$spend_category, S0, S3) %>%
      filter(!if_all(-c(S0, S3), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(-c(S0, S3)), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  spending_month_data <- reactive({
    culture_survey_filt() %>%
      haven::as_factor() %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("M29")), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  spending_on_month_data <- reactive({
    culture_survey_filt() %>%
      select(input$spend_category_month, S0, S3) %>%
      filter(!if_all(-c(S0, S3), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(-c(S0, S3)), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  encourage_data <- reactive({
    culture_survey_filt() %>%
      count(M32, S0) %>%
      na.omit()
  })
  
  more_events_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M31"), S0) %>%
      select(-c(M31_6, M31_6t)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M31_1" = "Lack of interest",
                             "M31_2" = "Lack of time",
                             "M31_3" = "Too expensive",
                             "M31_4" = "Lack of information",
                             "M31_5" = "Limited choice or poor quality",
                             "M31_7" = "I don't know",
                             "M31_8" = "None of the above"))
  })
  
  listen_where_data <- reactive({
    culture_survey_filt() %>%
      select(matches(paste0("^M33_.*_", input$place)), S0, S3) %>%
      select(-c(S3)) %>%
      rename_with(~ c("Music streaming", "YouTube", "Downloaded music", "Cds", "Casette tapes", "LPs", "The radio", "Television", "S0")) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value)
  })
  
  listen_where_amount_data <- reactive({
    culture_survey_filt() %>%
      select(matches(paste0("^M34_", input$medium, "_", input$place2)), S0, S3) %>%
      select(-c(S3)) %>%
      rename_with(~c("value", "S0")) %>%
      na.omit()
  })
  
  
  background_data <- reactive({
    culture_survey_filt()%>%
      select(starts_with("M35"), S0, S3) %>%
      select(-c(S3)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M35_1" = "Restaurants",
                             "M35_2" = "Cafes",
                             "M35_3" = "Bars/pubs",
                             "M35_4" = "Hotels/hostels",
                             "M35_5" = "Gyms/fitness centers/etc",
                             "M35_6" = "Limited choice or poor quality",
                             "M35_7" = "Dance studios",
                             "M35_8" = "None of these"))
  })
  
  background_amount_data <- reactive({
    culture_survey_filt() %>%
      select(input$background, S0, S3) %>%
      select(-c(S3)) %>%
      rename_with(~c("value", "S0")) %>%
      na.omit()
  })
  
  spending_month_listen_data <- reactive({
    culture_survey_filt() %>%
      filter(!if_all(starts_with("M37"), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("M37")), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  spending_on_month_listen_data <- reactive({
    culture_survey_filt() %>%
      select(input$spend_category_month_listen, S0, S3) %>%
      filter(!if_all(-c(S0, S3), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(-c(S0, S3)), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  artist_support_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M38"), S0, S3) %>%
      select(-c(S3)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M38_1" = "Donation to a music organisation",
                             "M38_2" = "A donation to a musical artist",
                             "M38_3" = "A subscription to a musical artist's members-only profile",
                             "M38_4" = "A private performance by musical artis",
                             "M38_5" = "Special tickets to a live music event",
                             "M38_6" = "Limited-edition physical merchandise",
                             "M38_7" = "Limited-edition digital merchandise",
                             "M38_8" = "A paid personal interaction with a musical artist",
                             "M38_9" = "None of these"))
  })
  
  ai_music_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M39"), S0, S3) %>%
      select(-c(S3)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M39_1" = "Musical deepfakes",
                             "M39_2" = "Music made by human artists who credit AI as a creative partner",
                             "M39_3" = "Music made by human artists who may have used AI, but don't discuss it",
                             "M39_4" = "Music made entirely by AI"))
  })
  
  human_compensated_data <- reactive({
    culture_survey_filt() %>%
      count(M40, S0) %>%
      na.omit()
  })
  
  discover_music_data <- reactive({
    culture_survey_filt() %>%
      count(M41, S0) %>%
      mutate(M41 = case_when(
        M41 == "1  Not very important – I’m happy listening only to artists I already know" ~ "1 Not very important",
        M41 == "5  Very important – I can’t live without new music" ~ "5 Very important",
        TRUE ~ M41
      )) %>%
      na.omit()
  })
  
  discovery_sources_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M42"), S0) %>%
      select(-c(M42_11, M42_11t)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M42_1" = "Music streaming services",
                             "M42_2" = "Music cataloguing/discovery services",
                             "M42_3" = "Online communities/forums",
                             "M42_4" = "Recommendations from friends and family",
                             "M42_5" = "Live events",
                             "M42_6" = "Press",
                             "M42_7" = "AI chats",
                             "M42_8" = "Social media platforms with video sharing",
                             "M42_9" = "Radio",
                             "M42_10" = "Other types of media"))
  })
  
  music_language_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M43"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M43_1" = "Your native language",
                             "M43_2" = "English",
                             "M43_3" = "Another European language"))
  })
  
  no_unfamiliar_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M44"), S0) %>%
      select(-c(M44_6, M44_6t)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M44_1" = "I don’t like listening to music in unfamiliar languages",
                             "M44_2" = "There isn’t much music from other European countries that I like",
                             "M44_3" = "No particular reason, I just don’t often hear or read about such music",
                             "M44_4" = "No particular reason, such music just doesn’t get recommended to me",
                             "M44_5" = "No particular reason, other music is just more easily available",
                             "M44_7" = "I don’t know"))
  })
  
  playlists_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M45"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M45_1" = "Manually curated/editorial playlists",
                             "M45_2" = "Automated playlists",
                             "M45_3" = "I don’t know if they are editorial or automated",
                             "M45_4" = "I don’t use any of the playlists provided by the service"))
  })
  
  weekly_discovery_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M46"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M46_1" = "Manually search the service for a song, album or an artist that you already know",
                             "M46_2" = "Manually search the service for previously unheard music of an artist you already know",
                             "M46_3" = "Select playlists manually curated by the service editors",
                             "M46_4" = "Select automated playlists",
                             "M46_5" = "Select playlists prepared by the streaming service",
                             "M46_6" = "Select own playlists or bookmarked music",
                             "M46_7" = "Listen to recommendations provided on-the-go"))
  })
  
  difficulty_finding_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M47"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M47_1" = "Find artists from your home country",
                             "M47_2" = "Find artists from the UK",
                             "M47_3" = "Find artists from other European countries",
                             "M47_4" = "Find music with lyrics in your native language"))
  })
  
  recommendation_satisfaction_data <- reactive({
    culture_survey_filt() %>%
      count(M48, S0) %>%
      na.omit()
  })
  
  recommendation_statements_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M49"), S0) %>%
      select(-c(M49_4)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M49_1" = "I discover more music in general",
                             "M49_2" = "I discover more music from my home country",
                             "M49_3" = "I discover more music in my native language",
                             "M49_5" = "I discover more music from other European countries",
                             "M49_6" = "I discover more diverse music in terms of artist nationalities",
                             "M49_7" = "I discover more diverse music in terms of lyrics languages"))
  })
  
  school_music_data <- reactive({
    culture_survey_filt()%>%
      select(starts_with("M50"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M50_1" = "In primary school",
                             "M50_2" = "In lower secondary school",
                             "M50_3" = "In upper secondary school"))
  })
  
  school_content_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M51"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M51_1" = "Basic music theory",
                             "M51_2" = "Western classical music",
                             "M51_3" = "Jazz",
                             "M51_4" = "Traditional folk music from respondents country",
                             "M51_5" = "Traditional folk music from other countries",
                             "M51_6" = "Other kinds of music (pop, rock, etc.)"))
  })
  
  uni_classes_data <- reactive({
    culture_survey_filt() %>%
      count(M52, S0) %>%
      na.omit()
  })
  
  uni_classes_plan_data <- reactive({
    culture_survey_filt() %>%
      count(M53, S0) %>%
      na.omit()
  })
  
  studied_outside_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M54"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M54_1" = "Yes, with teacher",
                             "M54_2" = "Yes, by myself",
                             "M54_3" = "No"))
  })
  
  spending_study_data <- reactive({
    culture_survey_filt() %>%
      filter(!if_all(starts_with("M55"), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across("M55"), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  statement_personal_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M56"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M56_1" = "Personal identity",
                             "M56_2" = "The music I choose to listen to says a lot about my ideas, values, and beliefs",
                             "M56_3" = "Now or in the past, I’ve strongly identified with a music subculture",
                             "M56_4" = "When it comes to my favourite musical artists, you could call me a “superfan”",
                             "M56_5" = "Some of the most memorable events in my life are strongly connected to certain songs or musical artists",
                             "M56_6" = "Social connections and community",
                             "M56_7" = "Music is/was an important part of life in my family",
                             "M56_8" = "Music is an important part of life in my local community",
                             "M56_9" = "I often share new music with people I know",
                             "M56_10" = "I’ve met some of my best friends through music",
                             "M56_11" = "Self-regulation",
                             "M56_12" = "Music helps me manage my emotions and cope with difficult situations",
                             "M56_13" = "Music makes everyday life seem more colourful and “alive",
                             "M56_14" = "Music motivates me during physical activities",
                             "M56_15" = "Music is an important escape from the stress of things I can’t control"))
  })
  
  statement_culture_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M57"), S0) %>%
      haven::as_factor() %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M57_1" = "Cultural connections and community",
                             "M57_2" = "Music makes me feel deeply connected to the country",
                             "M57_3" = "Music makes me feel deeply connected to Europe",
                             "M57_4" = "Music connects me with people from different cultural backgrounds",
                             "M57_5" = "Music makes me feel deeply connected to my personal cultural background",
                             "M57_6" = "I often seek to explore music from other countries or cultures",
                             "M57_7" = "Access to music from diverse cultures makes life more interesting",
                             "M57_8" = "Politics",
                             "M57_9" = "Every child and adult should have the right to express themselves musically in all freedom",
                             "M57_10" = "Every child and adult should have the opportunity to learn musical skills",
                             "M57_11" = "Every child and adult should have the right to listen to and participate in any kind of music",
                             "M57_12" = "Under no circumstances should music or other art forms be censored by the government",
                             "M57_13" = "Musicians and other musical professionals should have the right to decent working conditions and fair pay for their work",
                             "M57_14" = "I appreciate it when musicians speak up about important social and political issues"))
  })
  
  music_sector_data <- reactive({
    culture_survey_filt() %>%
      count(M58, S0) %>%
      na.omit()
  })
  
  cultural_policy_data <- reactive({
    culture_survey_filt() %>%
      count(M59, S0) %>%
      na.omit()
  })
  
  cultural_funding_data <- reactive({
    culture_survey_filt() %>%
      count(M60, S0) %>%
      na.omit()
  })
  
  culture_funding_statement_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M61"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M61_1" = "Support the most well-established national cultural traditions",
                             "M61_2" = "Support as diverse a range of new creative activities as possible",
                             "M61_3" = "Support international cultural exchanges",
                             "M61_4" = "Be tied to the recipient’s efforts for environmental sustainability",
                             "M61_5" = "Be tied to the recipient’s efforts against gender discrimination",
                             "M61_6" = "Be tied to the recipient’s efforts against ethnic or cultural discrimination",
                             "M61_7" = "Be tied to the recipient’s support for decent working conditions for artists",
                             "M61_8" = "Be tied to the recipient’s independence from political parties or interest groups",
                             "M61_9" = "Be targeted for groups that have experienced gender discrimination",
                             "M61_10" = "Be targeted for groups that have experienced ethnic or cultural discrimination",
                             "M61_11" = "Be targeted for recipients in small cities, towns, and rural areas"))
  })
  
  quotas_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M62"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M62_1" = "Content by European artists",
                             "M62_2" = "Content by #country_adv# artists",
                             "M62_3" = "Content by artists from many different areas of the country",
                             "M62_4" = "Content by new and emerging artists",
                             "M62_5" = "Content by female artists that identify as female",
                             "M62_6" = "Content by artists from ethnic or cultural minorities"))
  })
  
  policy_measures_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M63_"), S0) %>%
      select(-c(M63_13)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M63_1" = "Redistribution to benefit local music",
                             "M63_2" = "Obligate large music streaming services to support and invest in #country_adv# artists",
                             "M63_3" = "Obligate large online ticket sales platforms to support and invest in local live music scenes in #country#",
                             "M63_4" = "Use a percentage of tourism taxes to support local live music scenes in #country#",
                             "M63_5" = "Fairness and transparency in business practices",
                             "M63_6" = "Require music streaming services to share data on how artists are paid, recommended to listeners, etc.",
                             "M63_7" = "Regulate the resale of music tickets for profit (“scalping”)",
                             "M63_8" = "Regulate practice of making artists pay to play live, appear on playlists, etc.",
                             "M63_9" = "Decent work and economic support",
                             "M63_10" = "Ensure that artists can access social protection",
                             "M63_11" = "Ensure that artists can earn a minimum income, even when they’re self-employed",
                             "M63_12" = "Make it easier for artists to travel across borders and earn income in multiple EU states",
                             "M63_14" = "Sustainability and accessibility",
                             "M63_15" = "Offer vouchers for students and other people with little money to attend live music events",
                             "M63_16" = "Ensure that music education is freely available in all schools"))
  })
  
  quotas_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M62"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M62_1" = "Content by European artists",
                             "M62_2" = "Content by #country_adv# artists",
                             "M62_3" = "Content by artists from many different areas of the country",
                             "M62_4" = "Content by new and emerging artists",
                             "M62_5" = "Content by female artists that identify as female",
                             "M62_6" = "Content by artists from ethnic or cultural minorities"))
  })
  
  musician_level_data <- reactive({
    culture_survey_filt() %>%
      count(M64, S0) %>%
      na.omit()
  })
  
  main_occupation_data <- reactive({
    culture_survey_filt() %>%
      count(M68, S0) %>%
      na.omit()
  })
  
  yearly_instrument_data <- reactive({
    culture_survey_filt() %>%
      select(input$yearly_instrument, S0) %>%
      filter(!if_all(-c(S0), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(-c(S0)), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  yearly_instrument_total_data <- reactive({
    culture_survey_filt() %>%
      filter(!if_all(starts_with("M69"), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("M69")), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  income_activity_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M70"), S0) %>%
      select(-c(M70_21, M70_21t)) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M70_1" = "Singing",
                             "M70_2" = "Playing a musical instrument ",
                             "M70_3" = "DJing",
                             "M70_4" = "Composing or arranging music",
                             "M70_5" = "Writing song lyrics",
                             "M70_6" = "Producing, mixing, or mastering music",
                             "M70_7" = "Sound engineering at a live music event",
                             "M70_8" = "Engineering or producing visuals at a live music event",
                             "M70_9" = "Support staff at a live music event (tech support, roadies, etc.)",
                             "M70_10" = "Venue staff at a live music event (door, bar, security, etc.)",
                             "M70_11" = "Working for a business that makes or sells music-related goods (records, instruments, etc.)",
                             "M70_12" = "Working for a business that provides music-related services (booking, AR, PR, etc.)",
                             "M70_13" = "Working for another type of organisation in the music sector (rights management, advocacy, etc.)",
                             "M70_14" = "Working on a music-related project (programming music software, researching music, etc.)",
                             "M70_15" = "Music supervision in film, TV, advertising, video games, etc.",
                             "M70_16" = "Music playlist curation (in-store, online, etc.)",
                             "M70_17" = "Teaching music in a school/university ",
                             "M70_18" = "Teaching music outside of a school/university ",
                             "M70_19" = "Practicing music therapy or related services",
                             "M70_20" = "Social media activities involving music (TikTok videos, etc.)"))
  })
  
  income_yearly_data <- reactive({
    culture_survey_filt() %>%
      filter(!if_all(starts_with("M71"), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(starts_with("M71")), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  income_from_yearly_data <- reactive({
    culture_survey_filt() %>%
      select(input$income_yearly, S0) %>%
      filter(!if_all(-c(S0), is.na)) %>%
      rowwise() %>%
      mutate(sums = sum(c_across(-c(S0)), na.rm = TRUE)) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  total_income_data <- reactive({
    culture_survey_filt() %>%
      rowwise() %>%
      mutate(sums = sum(c_across(M73))) %>%
      select(sums, S0) %>%
      na.omit()
  })
  
  profession_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M74"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M74_1" = "Playing a musical instrument",
                             "M74_2" = "Singing",
                             "M74_3" = "Acting on the stage or in a film",
                             "M74_4" = "Dancing",
                             "M74_5" = "Writing poems, essays, novels, etc",
                             "M74_6" = "Film making, photography",
                             "M74_7" = "Any other artistic activities like sculpture, painting, handicrafts or drawing",
                             "M74_8" = "Creative computing such as designing websites or blogs, etc.",
                             "M74_9" = "Other",
                             "M74_10" = "None"))
  })
  
  marital_status_data <- reactive({
    culture_survey_filt() %>%
      count(M75, S0) %>%
      na.omit()
  })
  
  paying_difficulty_data <- reactive({
    culture_survey_filt() %>%
      count(M76, S0) %>%
      na.omit()
  })
  
  society_place_data <- reactive({
    culture_survey_filt() %>%
      count(M77, S0) %>%
      na.omit()
  })
  
  political_scale_data <- reactive({
    culture_survey_filt() %>%
      count(M78, S0) %>%
      na.omit()
  })
  
  person_description_data <- reactive({
    culture_survey_filt() %>%
      select(starts_with("M79"), S0) %>%
      pivot_longer(!S0, names_to = "reason", values_to = "value") %>%
      na.omit() %>%
      count(reason, S0, value) %>%
      mutate(reason = recode(reason,
                             "M79_1" = "S/he believes s/he should always show respect to his/her parents and to older people",
                             "M79_2" = "Religious belief is important to him/her",
                             "M79_3" = "It's very important to him/her to help the people around him/her",
                             "M79_4" = "S/he thinks it is important that every person in the world be treated equally",
                             "M79_5" = "S/he thinks it's important to be interested in things",
                             "M79_6" = "S/he likes to take risks",
                             "M79_7" = "S/he seeks every chance he can to have fun",
                             "M79_8" = "Getting ahead in life is important to him/her",
                             "M79_9" = "S/he always wants to be the one who makes the decisions",
                             "M79_10" = "It is important to him/her that things be organized and clean",
                             "M79_11" = "It is important to him/her to always behave properly",
                             "M79_12" = "S/he thinks it is best to do things in traditional ways",
                             "M79_13" = "It is important to him/her to respond to the needs of others",
                             "M79_14" = "S/he believes all the worlds' people should live in harmony",
                             "M79_15" = "Thinking up new ideas and being creative is important to him/her",
                             "M79_16" = "S/he thinks it is important to do lots of different things in life",
                             "M79_17" = "S/he really wants to enjoy life",
                             "M79_18" = "Being very successful is important to him/her",
                             "M79_19" = "It is important to him/her to be in charge and tell others what to do",
                             "M79_20" = "Having a stable government is important to him/her"))
  })
  
  # Plots
  output$Gender <- renderPlot({
    gender_plot_data <- gender_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.25/length(unique(S0)) * row_number(),
             label = paste0(S3, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(gender_plot_data$label_y)
    # Setting color mapping
    unique_S3 <- unique(gender_plot_data$S3)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_S3)), unique_S3)
    # Plots
    p <- ggplot(gender_plot_data, aes(x = "", y = n, fill = S3)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    p_text <- ggplot(gender_plot_data) +
      geom_point(aes(x = 0, y = label_y, color = S3), size = 3, shape = 15) +
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Age <- renderPlot({
    age_plot_data <- age_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(age_plot_data, aes(x = SREC_Age, y = n)) +
      geom_bar(stat = "identity", fill = global_colors[1]) +
      
      # === CHANGE: Increased size from 2.8 to 4 ===
      geom_text(aes(y = n - n/5, label = paste0(n, "\n", " (", round(age_plot_data$percentage, 1), "%)")),
                vjust = -0.1, 
                size = 4,          # <--- Bigger text
                fontface = "bold") +
      
      labs(x = "Age group", y = "Count") + facet_wrap(~S02, ncol = 3, scales = "free_y") +
      
      theme(
        # Kept your existing specific margins/angles for Age, just ensured sizes match standard
        axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5, size = 14, margin = margin(t = 5)),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 20)),
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", margin = margin(b = 60))
      ) + 
      coord_cartesian(clip = "off")
  })
  
  output$Culture_participitation <- renderPlot({
    culture_participitation_plot_data <- culture_participitation_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / S6 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", S6))
    
    ggplot(culture_participitation_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.5) + 
      
      scale_fill_manual(values = c(
        "Not in the  last twelve months" = global_colors[2],
        " 1-2  times" = global_colors[6],
        " 3-5  times" = global_colors[4],
        " More than  5 times" = global_colors[1],
        "  Don’t know" = global_colors[3])) +
      
      geom_text_repel(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        
        # === CHANGE 2: Increased text size on bars ===
        size     = 4, # <-- Increased from 2.5
        
        direction = "y",
        box.padding = 0.2,
        fontface = "bold"
      ) +
      
      # === CHANGE 3: Added theme elements for axis/legend text ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      
      labs(x = "Count", y = "Activity", fill = "Answer") +
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(35)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  # output$Culture_participitation <- renderPlot({
  #   culture_participitation_plot_data <- culture_participitation_data() %>%
  #     group_by(S0) %>%
  #     mutate(percentage = n / sum(n) * 100,
  #            label_y = -0.25/length(unique(S0)) * row_number(),
  #            label = paste0(S6, ": ", n, " (", round(percentage, 1), "%)")) %>%
  #     mutate(S02 = paste0(S0, "\nN = ", sum(n)))
  #   ymin <- min(culture_participitation_plot_data$label_y)
  #   unique_S6 <- unique(culture_participitation_plot_data$S6)
  #   color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_S6)), unique_S6)
  #   # Plots
  #   p <- ggplot(culture_participitation_plot_data, aes(x = "", y = n, fill = S6)) +
  #     geom_bar(stat = "identity", width = 1, position = position_fill()) +
  #     coord_polar("y", start = 0) +
  #     theme_void() + facet_wrap(~S02, ncol = 2) +
  #     scale_fill_manual(values = color_palette) + theme(legend.position = "none")
  #   p_text <- ggplot(culture_participitation_plot_data) +
  #     geom_point(aes(x = 0, y = label_y, color = S6), size = 3, shape = 15) +
  #     geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0) +
  #     theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
  #     facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
  #     ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
  #   plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  # })
  
  output$Live_attend <- renderPlot({
    live_attend_plot_data <- live_attend_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / S7 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", S7))
    
    ggplot(live_attend_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.5) +
      
      scale_fill_manual(values = c(
        "Not in the  last twelve months" = global_colors[2],
        " 1-2  times" = global_colors[6],
        " 3-5  times" = global_colors[4],
        " More than  5 times" = global_colors[1],
        "  Don’t know" = global_colors[3])) +
      
      geom_text_repel(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        
        # === CHANGE 2: Increased text size on bars ===
        size     = 4, # <-- Increased from 2.5
        
        direction = "y",
        box.padding = 0.2,
        fontface = "bold"
      ) +
      
      # === CHANGE 3: Added theme elements for axis/legend text ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      
      labs(x = "Count", y = "Activity", fill = "Answer") +
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(35)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  # output$Live_attend <- renderPlot({
  #   live_attend_plot_data <- live_attend_data() %>%
  #     group_by(S0) %>%
  #     mutate(percentage = n / sum(n) * 100,
  #            label_y = -0.25/length(unique(S0)) * row_number(),
  #            label = paste0(S7, ": ", n, " (", round(percentage, 1), "%)")) %>%
  #     mutate(S02 = paste0(S0, "\nN = ", sum(n)))
  #   ymin <- min(live_attend_plot_data$label_y)
  #   unique_S7 <- unique(live_attend_plot_data$S7)
  #   color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_S7)), unique_S7)
  #   # Plots
  #   p <- ggplot(live_attend_plot_data, aes(x = "", y = n, fill = S7)) +
  #     geom_bar(stat = "identity", width = 1, position = position_fill()) +
  #     coord_polar("y", start = 0) +
  #     theme_void() + facet_wrap(~S02, ncol = 2) +
  #     scale_fill_manual(values = color_palette) + theme(legend.position = "none")
  #   p_text <- ggplot(live_attend_plot_data) +
  #     geom_point(aes(x = 0, y = label_y, color = S7), size = 3, shape = 15) +
  #     geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0) +
  #     theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
  #     facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
  #     ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
  #   plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  # })
  
  output$Culture_done <- renderPlot({
    culture_done_plot_data <- culture_done_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / S8 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", S8))
    
    ggplot(culture_done_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        
        # === CHANGE 2: Increased text size on bars ===
        size     = 4, # <-- Increased from 3.1
        
        fontface = "bold"
      ) +
      
      # === CHANGE 3: Added theme elements for axis/legend text ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      
      labs(x = "Count", y = "Activity", fill = "Answer") +
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Earn_income_music <- renderPlot({
    earn_income_music_plot_data <- earn_income_music_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.25/length(unique(S0)) * row_number(),
             label = paste0(S9, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(earn_income_music_plot_data$label_y)
    unique_S9 <- unique(earn_income_music_plot_data$S9)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_S9)), unique_S9)
    # Plots
    p <- ggplot(earn_income_music_plot_data, aes(x = "", y = n, fill = S9)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none", 
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    p_text <- ggplot(earn_income_music_plot_data) +
      geom_point(aes(x = 0, y = label_y, color = S9), size = 3, shape = 15) +
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0, 
                fontface = "bold") +
      
      # === WE MOVED THE THEME BLOCK FROM HERE... ===
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      
      # === ...TO HERE, SO IT'S AFTER THEME_VOID() ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  
  output$Music_quality <- renderPlot({
    music_quality_live_plot_data <- music_quality_live_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.25/length(unique(S0)) * row_number(),
             label = paste0(S10, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(music_quality_live_plot_data$label_y)
    unique_S10 <- unique(music_quality_live_plot_data$S10)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_S10)), unique_S10)
    # Plots
    p <- ggplot(music_quality_live_plot_data, aes(x = "", y = n, fill = S10)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(legend.position = "none", 
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    p_text <- ggplot(music_quality_live_plot_data) +
      geom_point(aes(x = 0, y = label_y, color = S10), size = 3, shape = 15) +
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      ggh4x::force_panelsizes(rows = unit(3.5, "cm")) + coord_cartesian(clip = "off")
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  
  
  output$Live_Music_quality <- renderPlot({
    live_music_quality_plot_data <- live_music_quality_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.25/length(unique(S0)) * row_number(),
             label = paste0(M11, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(live_music_quality_plot_data$label_y)
    # Setting color mapping
    unique_M11 <- unique(live_music_quality_plot_data$M11)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M11)), unique_M11)
    # Plots
    p <- ggplot(live_music_quality_plot_data, aes(x = "", y = n, fill = M11)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none", 
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(live_music_quality_plot_data) +
      geom_point(aes(x = 0, y = label_y, color = M11), size = 3, shape = 15) +
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      # === FIX: Moved the theme() call to AFTER theme_void() ===
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      
      # This theme() call is now *after* theme_void() and won't be erased
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      ggh4x::force_panelsizes(rows = unit(3.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Last_event <- renderPlot({
    last_event_plot_data <- last_event_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(last_event_plot_data, aes(y = M12, x = n)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", fill = global_colors[1], width = 0.7) +
      
      # === CHANGE 2: Increased size from 3.1 to 4 ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 4, fontface = "bold") +
      
      # Changed "Activity" to "Answer" for consistency
      labs(x = "Count", y = "Answer", fill = "Answer") + 
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  # output$Dayes_attended <- renderPlot({
  #   dayes_attended_plot_data <- days_attended_data() %>%
  #     group_by(S0) %>%
  #     mutate(percentage = n / sum(n) * 100) %>%
  #     mutate(S02 = paste0(S0, "\nN = ", sum(n)))
  #   ggplot(dayes_attended_plot_data, aes(y = as.factor(M14_1t), x = n)) +
  #     geom_bar(stat = "identity", fill = global_colors[1]) +
  #     geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
  #               hjust = 0.1, size = 3.1) +
  #     labs(x = "Count", y = "Number of days", fill = "Answer") +
  #     facet_wrap(~S02, ncol = 3, scales = "free_x") +
  #     scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
  #     scale_x_continuous(breaks = scales::breaks_pretty())
  # })
  
  output$Days_attended <- renderPlot({
    days_attended_plot_data <- days_attended_data() %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M14))
    
    ggplot(days_attended_plot_data, aes(x = M14_1t)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      labs(x = "Number of days", y = "Count") + facet_wrap(~S02, ncol = 3) + 
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
      )
  })
  
  output$Event_type <- renderPlot({
    type_last_event_plot_data <- type_last_event_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(type_last_event_plot_data, aes(y = M13, x = n)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", fill = global_colors[1], width = 0.7) +
      
      # === CHANGE 2: Matched text styling to Q12 (size 4, hjust) ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 4, fontface = "bold") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      
      # === CHANGE 4: Swapped X and Y labels to be correct ===
      labs(x = "Count", y = "Type of event") + 
      
      # === CHANGE 5: Changed scales to "free_x" for consistency ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") + 
      
      coord_cartesian(clip = "off") +
      scale_y_discrete(labels = scales::label_wrap(25))
  })
  
  output$Who_go_with <- renderPlot({
    who_go_with_plot_data <- who_go_with_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(who_go_with_plot_data, aes(y = M15, x = n)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", fill = global_colors[1], width = 0.7) +
      
      # === CHANGE 2: Matched text styling (size 4, hjust) ===
      # Original code had vjust, which is for vertical bars
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 4, fontface = "bold") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      
      labs(y = "Answer", x = "Count") + 
      
      # === CHANGE 4: Changed scales to "free_x" ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") + 
      
      coord_cartesian(clip = "off") +
      scale_y_discrete(labels = scales::label_wrap(50))
  })
  
  output$Ticket_required <- renderPlot({
    ticket_required_plot_data <- ticket_required_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M16, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(ticket_required_plot_data$label_y)
    # Setting color mapping
    unique_M16 <- unique(ticket_required_plot_data$M16)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M16)), unique_M16)
    # Plots
    p <- ggplot(ticket_required_plot_data, aes(x = "", y = n, fill = M16)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(ticket_required_plot_data) +
      
      # === CHANGE 1: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M16), size = 3, shape = 15) +
      
      # === CHANGE 2: Increased text size ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === CHANGE 3: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Who_paid <- renderPlot({
    who_paid_plot_data <- who_paid_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M17, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(who_paid_plot_data$label_y)
    # Setting color mapping
    unique_M17 <- unique(who_paid_plot_data$M17)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M17)), unique_M17)
    # Plots
    p <- ggplot(who_paid_plot_data, aes(x = "", y = n, fill = M17)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none", 
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(who_paid_plot_data) +
      
      # === CHANGE 1: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M17), size = 3, shape = 15) +
      
      # === CHANGE 2: Increased text size ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === CHANGE 3: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  # output$Ticket_number <- renderPlot({
  #   ticket_number_plot_data <- ticket_number_data() %>%
  #     group_by(S0) %>%
  #     mutate(percentage = n / sum(n) * 100) %>%
  #     mutate(S02 = paste0(S0, "\nN = ", sum(n)))
  #   ggplot(ticket_number_plot_data, aes(y = as.factor(M18_2t), x = n)) +
  #     geom_bar(stat = "identity", fill = global_colors[1]) +
  #     geom_text(aes(x = n - n/5, label = paste0(n, "\n", " (", round(percentage, 1), "%)")),
  #               vjust = -0.1, size = 2.8) +
  #     labs(y = "Number", x = "Count") + facet_wrap(~S02, ncol = 3, scales = "free_y") +
  #     coord_cartesian(clip = "off") +
  #     scale_y_discrete(labels = scales::label_wrap(50))
  # })
  
  output$Ticket_number <- renderPlot({
    ticket_number_plot_data <- ticket_number_data() %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M18))
    
    ggplot(ticket_number_plot_data, aes(x = M18_2t)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Number of tickets", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Where_event <- renderPlot({
    where_event_was_plot_data <- where_event_was_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M20, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(where_event_was_plot_data$label_y)
    # Setting color mapping
    unique_M20 <- unique(where_event_was_plot_data$M20)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M20)), unique_M20)
    # Plots
    p <- ggplot(where_event_was_plot_data, aes(x = "", y = n, fill = M20)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(where_event_was_plot_data) +
      
      # === CHANGE 1: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M20), size = 3, shape = 15) +
      
      # === CHANGE 2: Increased text size ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === CHANGE 3: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Music_type <- renderPlot({
    music_type_plot_data <- music_type_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M22 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M22))
    
    ggplot(music_type_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4,
        fontface = "bold"
      ) +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      
      labs(x = "Count", y = "Type", fill = "Answer") +
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Gender_diverse <- renderPlot({
    gender_diverse_plot_data <- gender_diverse_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M23, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(gender_diverse_plot_data$label_y)
    # Setting color mapping
    unique_M23 <- unique(gender_diverse_plot_data$M23)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M23)), unique_M23)
    # Plots
    p <- ggplot(gender_diverse_plot_data, aes(x = "", y = n, fill = M23)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(gender_diverse_plot_data) +
      
      # === CHANGE 1: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M23), size = 3, shape = 15) +
      
      # === CHANGE 2: Increased text size ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === CHANGE 3: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Artists_from <- renderPlot({
    artists_from_plot_data <- artists_from_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M24 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M24))
    
    ggplot(artists_from_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", position = "stack", width = 0.5) +
      
      scale_fill_manual(values = c(
        "Yes" = global_colors[1],
        "No" = global_colors[2],
        "I don’t know / don’t remember" = global_colors[3])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4,
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Origin", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Traditional_questions <- renderPlot({
    traditional_questions_plot_data <- traditional_questions_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M25 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M25))
    
    ggplot(traditional_questions_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", position = "stack", width = 0.5) +
      
      scale_fill_manual(values = c(
        " Yes" = global_colors[1],
        " No" = global_colors[2],
        "I don’t know /  don’t remember" = global_colors[3])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4,
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Statement", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(35)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Envi_questions <- renderPlot({
    envi_questions_plot_data <- envi_questions_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M26 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M26))
    
    ggplot(envi_questions_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        " Yes" = global_colors[1],
        " No" = global_colors[2],
        "I don’t know /  don’t remember" = global_colors[3])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4,
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Statement", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(35)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Music_scene_rating <- renderPlot({
    music_scene_rating_plot_data <- music_scene_rating_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M27, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(music_scene_rating_plot_data$label_y)
    # Setting color mapping
    unique_M27 <- unique(music_scene_rating_plot_data$M27)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M27)), unique_M27)
    # Plots
    p <- ggplot(music_scene_rating_plot_data, aes(x = "", y = n, fill = M27)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(music_scene_rating_plot_data) +
      
      # === CHANGE 1: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M27), size = 3, shape = 15) +
      
      # === CHANGE 2: Increased text size ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === CHANGE 3: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # === CHANGE 4: Increased height for 6 legend items ===
      ggh4x::force_panelsizes(rows = unit(4.0, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Venue_size <- renderPlot({
    venue_size_plot_data <- venue_size_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M28, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(venue_size_plot_data$label_y)
    # Setting color mapping
    unique_M28 <- unique(venue_size_plot_data$M28)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M28)), unique_M28)
    # Plots
    p <- ggplot(venue_size_plot_data, aes(x = "", y = n, fill = M28)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(venue_size_plot_data) +
      
      # === CHANGE 1: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M28), size = 3, shape = 15) +
      
      # === CHANGE 2: Increased text size ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === CHANGE 3: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # === CHANGE 4: Increased height for 7 legend items ===
      ggh4x::force_panelsizes(rows = unit(4.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Spending <- renderPlot({
    spending_plot_data <- spending_data()
    ggplot(spending_plot_data, aes(x = sums, y = after_stat(density))) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      geom_density(size = 1.5, color = "grey") +
      labs(x = "Spending") + facet_wrap(~S0, ncol = 3) + 
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
      ) 
  })
  
  output$Spending_table <- renderTable({
    spending_data() %>%
      rename(Country = S0) %>%
      group_by(Country) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Spending_on <- renderPlot({
    spending_on_plot_data <- spending_on_data()
    ggplot(spending_on_plot_data, aes(x = sums, y = after_stat(density))) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      geom_density(size = 1.5, color = "grey") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Spending") + facet_wrap(~S0, ncol = 3)
  })
  
  output$Spending_on_table <- renderTable({
    spending_on_data() %>%
      rename(Country = S0) %>%
      group_by(Country) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Spending_month <- renderPlot({
    spending_month_plot_data <- spending_month_data() %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M29))
    ggplot(spending_month_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$country_tables <- renderUI({
    countries <- unique(evaluation_table_filt()$Country)
    
    lapply(countries, function(ctry) {
      tagList(
        h3(ctry),
        tableOutput(paste0("table_", ctry))
      )
    })
  })
  
  # create one renderTable per country dynamically
  observe({
    countries <- unique(evaluation_table_filt()$Country)
    
    lapply(countries, function(ctry) {
      local({
        country <- ctry
        
        output[[paste0("table_", country)]] <- renderTable({
          evaluation_table_filt()[evaluation_table_filt()$Country == country, ] %>%
            select(-Country)
        })
      })
    })
  })
  
  output$Spending_month_table <- renderTable({
    spending_month_data() %>%
      rename(Country = S0) %>%
      group_by(Country) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Spending_on_month <- renderPlot({
    spending_on_month_plot_data <- spending_on_month_data()  %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M29))
    ggplot(spending_on_month_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  output$Spending_on_month_table <- renderTable({
    spending_on_month_data() %>%
      rename(Country = S0) %>%
      group_by(Country) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Encourage <- renderPlot({
    encourage_plot_data <- encourage_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(encourage_plot_data, aes(y = M32, x = n)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", fill = global_colors[1], width = 0.7) +
      
      # === CHANGE 2: Matched text styling (size 4, hjust) ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                hjust = 0.1, size = 4, fontface = "bold") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Count", y = "Reason") +
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(35)) + coord_cartesian(clip = "off")
  })
  
  output$More_events <- renderPlot({
    more_events_plot_data <- more_events_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M31 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M31))
    
    ggplot(more_events_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 to narrow bars ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4,
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Reason", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Listen_where <- renderPlot({
    listen_where_plot_data <- listen_where_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M33 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M33))
    
    ggplot(listen_where_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Medium", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Listen_where_amount <- renderPlot({
    listen_where_amount_plot_data <- listen_where_amount_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M34))
    ggplot(listen_where_amount_plot_data, aes(x = value)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2")
      ) +
      labs(x = "Minutes", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Listen_where_amount <- renderPlot({
    listen_where_amount_plot_data <- listen_where_amount_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M34))
    
    ggplot(listen_where_amount_plot_data, aes(x = value)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Minutes", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Background <- renderPlot({
    background_plot_data <- background_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M35 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M35))
    
    ggplot(background_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Place", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(70)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Background_amount <- renderPlot({
    background_amount_plot_data <- background_amount_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M36))
    ggplot(background_amount_plot_data, aes(x = value)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2")
      ) +
      labs(x = "Minutes", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Background_amount <- renderPlot({
    background_amount_plot_data <- background_amount_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M36))
    
    ggplot(background_amount_plot_data, aes(x = value)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Minutes", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Spending_month_listen <- renderPlot({
    spending_month_listen_plot_data <- spending_month_listen_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M37))
    
    ggplot(spending_month_listen_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Spending_month_listen_table <- renderTable({
    spending_month_listen_data() %>%
      rename(Country = S0) %>%
      group_by(Country) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Spending_on_month_listen <- renderPlot({
    spending_on_month_listen_plot_data <- spending_on_month_listen_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M37))
    ggplot(spending_on_month_listen_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2")
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Spending_on_month_listen <- renderPlot({
    spending_on_month_listen_plot_data <- spending_on_month_listen_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M37))
    
    ggplot(spending_on_month_listen_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Artist_support <- renderPlot({
    artist_support_plot_data <- artist_support_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M38 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M38))
    
    ggplot(artist_support_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Activity", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Ai_music <- renderPlot({
    ai_music_plot_data <- ai_music_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M39 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M39))
    
    ggplot(ai_music_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Yes" = global_colors[1],
        "No" = global_colors[2],
        "I don’t know" = global_colors[3])) +
      
      # === CHANGE 2: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Statement", fill = "Answer") +
      
      # === CHANGE 3: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Human_compensated <- renderPlot({
    human_compensated_plot_data <- human_compensated_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M40, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(human_compensated_plot_data$label_y)
    # Setting color mapping
    unique_M40 <- unique(human_compensated_plot_data$M40)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M40)), unique_M40)
    # Plots
    p <- ggplot(human_compensated_plot_data, aes(x = "", y = n, fill = M40)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(human_compensated_plot_data) +
      
      geom_point(aes(x = 0, y = label_y, color = M40), size = 3, shape = 15) +
      
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # === CHANGE: Increased height to 3.5cm for 5 items ===
      ggh4x::force_panelsizes(rows = unit(3.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Discover_music <- renderPlot({
    discover_music_plot_data <- discover_music_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M41, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(discover_music_plot_data$label_y)
    # Setting color mapping
    unique_M41 <- unique(discover_music_plot_data$M41)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M41)), unique_M41)
    # Plots
    p <- ggplot(discover_music_plot_data, aes(x = "", y = n, fill = M41)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(discover_music_plot_data) +
      
      geom_point(aes(x = 0, y = label_y, color = M41), size = 3, shape = 15) +
      
      # === FIX: Moved 'size = 2' back INSIDE aes() to match Q9 ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # Kept 3.5cm height for 5 items
      ggh4x::force_panelsizes(rows = unit(3.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Discovey_sources <- renderPlot({
    discovery_sources_plot_data <- discovery_sources_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M42 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M42))
    
    ggplot(discovery_sources_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Don’t use" = global_colors[5],
        "Not important" = global_colors[2],
        "Somewhat important" = global_colors[4],
        "Very important" = global_colors[1])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Source", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Music_language <- renderPlot({
    music_language_plot_data <- music_language_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M43 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M43))
    
    ggplot(music_language_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Not at all likely" = global_colors[2],
        "Somewhat likely" = global_colors[4],
        "Very likely" = global_colors[1],
        "Don’t know" = global_colors[3])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Language", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$No_unfamiliar <- renderPlot({
    no_unfamiliar_plot_data <- no_unfamiliar_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M44 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M44))
    
    ggplot(no_unfamiliar_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Reason", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Playlists <- renderPlot({
    playlists_plot_data <- playlists_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M45 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M45))
    
    ggplot(playlists_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Type", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Weekly_discovery <- renderPlot({
    weekly_discovery_plot_data <- weekly_discovery_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M46 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M46))
    
    ggplot(weekly_discovery_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Never" = global_colors[2],
        "Sometimes" = global_colors[4],
        "Often" = global_colors[1])) +
      
      # === STYLE: Updated geom_text for dodged bars ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                position = position_dodge(width = 0.7), # <-- Matched width
                hjust = 0.1, 
                size = 4, # <-- Increased from 3.1
                fontface = "bold") +
      
      labs(x = "Count", y = "Activity", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Difficulty_finding <- renderPlot({
    difficulty_finding_plot_data <- difficulty_finding_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M47 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M47))
    
    ggplot(difficulty_finding_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Difficult" = global_colors[2],
        "Neither difficult nor easy" = global_colors[4],
        "Easy" = global_colors[1],
        "Don’t know" = global_colors[3])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Activity", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Recommendation_satisfaction <- renderPlot({
    recommendation_satisfaction_plot_data <- recommendation_satisfaction_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M48, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(recommendation_satisfaction_plot_data$label_y)
    # Setting color mapping
    unique_M48 <- unique(recommendation_satisfaction_plot_data$M48)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M48)), unique_M48)
    # Plots
    p <- ggplot(recommendation_satisfaction_plot_data, aes(x = "", y = n, fill = M48)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(legend.position = "none", 
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(recommendation_satisfaction_plot_data) +
      
      # === STYLE: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M48), size = 3, shape = 15) +
      
      # === STYLE: Fixed text size (size = 2 inside aes) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === STYLE: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # === STYLE: Increased height for 6 items ===
      ggh4x::force_panelsizes(rows = unit(4.0, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Recommendation_statements <- renderPlot({
    recommendation_statements_plot_data <- recommendation_statements_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M49 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M49))
    
    ggplot(recommendation_statements_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        " Disagree" = global_colors[2],
        "Neither disagree nor agree" = global_colors[4],
        " Agree" = global_colors[1],
        " I don’t know" = global_colors[3])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Statement", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$School_music <- renderPlot({
    school_music_plot_data <- school_music_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M50 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M50))
    
    ggplot(school_music_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        " Yes" = global_colors[1],
        " No" = global_colors[2],
        "I don’t know / don’t remember" = global_colors[3])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Education level", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$School_content <- renderPlot({
    school_content_plot_data <- school_content_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M51 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M51))
    
    ggplot(school_content_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        " Yes" = global_colors[1],
        " No" = global_colors[2],
        "I don’t know / don’t remember" = global_colors[3])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Type", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Uni_classes <- renderPlot({
    uni_classes_plot_data <- uni_classes_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M52, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(uni_classes_plot_data$label_y)
    # Setting color mapping
    unique_M52 <- unique(uni_classes_plot_data$M52)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M52)), unique_M52)
    # Plots
    p <- ggplot(uni_classes_plot_data, aes(x = "", y = n, fill = M52)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(uni_classes_plot_data) +
      
      # === STYLE: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M52), size = 3, shape = 15) +
      
      # === STYLE: Fixed text size (size = 2 inside aes) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === STYLE: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # === STYLE: Set height for 4 items ===
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Uni_classes_plan <- renderPlot({
    uni_classes_plan_plot_data <- uni_classes_plan_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M53, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(uni_classes_plan_plot_data$label_y)
    # Setting color mapping
    unique_M53 <- unique(uni_classes_plan_plot_data$M53)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M53)), unique_M53)
    # Plots
    p <- ggplot(uni_classes_plan_plot_data, aes(x = "", y = n, fill = M53)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(uni_classes_plan_plot_data) +
      
      # === STYLE: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M53), size = 3, shape = 15) +
      
      # === STYLE: Fixed text size (size = 2 inside aes) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === STYLE: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Studied_outside <- renderPlot({
    studied_outside_plot_data <- studied_outside_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M54 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M54))
    
    ggplot(studied_outside_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "yes" = global_colors[1],
        "no" = global_colors[2])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Answer", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Spending_study <- renderPlot({
    spending_study_plot_data <- spending_study_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M55))
    
    ggplot(spending_study_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === STYLE: Added theme elements for axis fonts ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold")         # Axis titles
        # --- END: Added lines ---
        
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S0, ncol = 3)
  })
  
  output$Spending_study_table <- renderTable({
    spending_study_data() %>%
      rename(Country = S0) %>%
      group_by(Country) %>%
      summarise(mean = mean(sums), median = median(sums))
  })
  
  output$Statement_personal <- renderPlot({
    statement_personal_plot_data <- statement_personal_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M56 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M56))
    
    ggplot(statement_personal_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Strongly disagree" = global_colors[5],
        "Somewhat disagree" = global_colors[2],
        "Neither agree nor disagree" = global_colors[3],
        "Somewhat agree" = global_colors[4],
        "Strongly agree" = global_colors[1])) +
      
      # === STYLE: Updated geom_text_repel ===
      geom_text_repel(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 2.5
        direction = "y",
        box.padding = 0.1, # <-- Reduced from 0.2
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Statement", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Statement_culture <- renderPlot({
    statement_culture_plot_data <- statement_culture_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M57 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M57))
    
    ggplot(statement_culture_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Strongly disagree" = global_colors[5],
        "Somewhat disagree" = global_colors[2],
        "Neither agree nor disagree" = global_colors[3],
        "Somewhat agree" = global_colors[4],
        "Strongly agree" = global_colors[1])) +
      
      # === STYLE: Updated geom_text_repel ===
      geom_text_repel(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 2.5
        direction = "y",
        box.padding = 0.1, # <-- Reduced from 0.2
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Statement", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Music_sector <- renderPlot({
    music_sector_plot_data <- music_sector_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M58, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(music_sector_plot_data$label_y)
    # Setting color mapping
    unique_M58 <- unique(music_sector_plot_data$M58)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M58)), unique_M58)
    # Plots
    p <- ggplot(music_sector_plot_data, aes(x = "", y = n, fill = M58)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(music_sector_plot_data) +
      
      # === STYLE: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M58), size = 3, shape = 15) +
      
      # === STYLE: Fixed text size (size = 2 inside aes) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === STYLE: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # === STYLE: Height for 5 items ===
      ggh4x::force_panelsizes(rows = unit(3.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Cultural_policy <- renderPlot({
    cultural_policy_plot_data <- cultural_policy_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M59, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(cultural_policy_plot_data$label_y)
    # Setting color mapping
    unique_M59 <- unique(cultural_policy_plot_data$M59)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M59)), unique_M59)
    # Plots
    p <- ggplot(cultural_policy_plot_data, aes(x = "", y = n, fill = M59)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(cultural_policy_plot_data) +
      
      # === STYLE: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M59), size = 3, shape = 15) +
      
      # === STYLE: Fixed text size (size = 2 inside aes) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === STYLE: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      # === STYLE: Height for 5 items ===
      ggh4x::force_panelsizes(rows = unit(3.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Cultural_funding <- renderPlot({
    
    cultural_funding_plot_data <- cultural_funding_data() %>%
      group_by(S0) %>%
      mutate(
        percentage = n / sum(n) * 100,
        
        # === CLEAN FIX: automatic equal vertical spacing ===
        item_index = row_number(),
        label_y = -item_index * 0.14,   # adjust 0.14 to tighten/loosen spacing
        
        M60_wrapped = scales::label_wrap(40)(M60),
        label = paste0(M60_wrapped, ": ", n, " (", round(percentage, 1), "%)")
      ) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ymin <- min(cultural_funding_plot_data$label_y)
    
    # Colors
    unique_M60 <- unique(cultural_funding_plot_data$M60)
    color_palette <- setNames(
      scales::brewer_pal(palette = "Blues")(length(unique_M60)),
      unique_M60
    )
    
    # === Left Plot (donut chart) ===
    p <- ggplot(cultural_funding_plot_data, aes(x = "", y = n, fill = M60)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() +
      facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2")
      )
    
    # === Right Plot (labels) ===
    p_text <- ggplot(cultural_funding_plot_data) +
      geom_point(aes(x = 0, y = label_y, color = M60), size = 3, shape = 15) +
      geom_text(
        aes(x = 0.2, y = label_y, label = label, size = 2),
        hjust = 0,
        fontface = "bold"
      ) +
      theme_void() +
      xlim(0, 3) +
      ylim(ymin, 0) +
      theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) +
      scale_color_manual(values = color_palette) +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      ggh4x::force_panelsizes(rows = unit(10.0, "cm")) +
      coord_cartesian(clip = "off")
    
    # === Final Combined Plot ===
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
    
  })
  
  
  output$Culture_funding_statement <- renderPlot({
    culture_funding_statement_plot_data <- culture_funding_statement_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M61 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M61))
    
    ggplot(culture_funding_statement_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Strongly disagree" = global_colors[5],
        "Somewhat disagree" = global_colors[2],
        "Neither agree nor disagree" = global_colors[3],
        "Somewhat agree" = global_colors[4],
        "Strongly agree" = global_colors[1])) +
      
      # === STYLE: Updated geom_text_repel ===
      geom_text_repel(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 2.5
        direction = "y",
        box.padding = 0.1, # <-- Reduced from 0.2
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Statement", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Quotas <- renderPlot({
    quotas_plot_data <- quotas_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M62 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M62))
    
    ggplot(quotas_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Oppose" = global_colors[2],
        "Neither oppose nor support" = global_colors[3],
        "Support" = global_colors[1])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Quota", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Policy_measures <- renderPlot({
    policy_measures_plot_data <- policy_measures_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M63 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M63))
    
    ggplot(policy_measures_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === STYLE: Added 'width = 0.7' to make bars narrower ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Oppose" = global_colors[2],
        "Neither oppose nor support" = global_colors[3],
        "Support" = global_colors[1])) +
      
      # === STYLE: Using geom_text (like Q8) with size 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 3.1
        fontface = "bold"
      ) +
      labs(x = "Count", y = "Measure", fill = "Answer") +
      
      # === STYLE: Added theme elements for axis fonts ===
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- START: Added these lines for bigger fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), # Y-axis labels
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), # X-axis labels
        axis.title = element_text(size = 16, face = "bold"),         # Axis titles
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
        # --- END: Added lines ---
        
      ) +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Musician_level <- renderPlot({
    musician_level_plot_data <- musician_level_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M64, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(musician_level_plot_data$label_y)
    # Setting color mapping
    unique_M64 <- unique(musician_level_plot_data$M64)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M64)), unique_M64)
    # Plots
    p <- ggplot(musician_level_plot_data, aes(x = "", y = n, fill = M64)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(musician_level_plot_data) +
      
      # === STYLE: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M64), size = 3, shape = 15) +
      
      # === STYLE: Fixed text size (size = 2 inside aes) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      
      # === STYLE: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      ggh4x::force_panelsizes(rows = unit(2.2, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Main_occupation <- renderPlot({
    main_occupation_plot_data <- main_occupation_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M68, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(main_occupation_plot_data$label_y)
    # Setting color mapping
    unique_M68 <- unique(main_occupation_plot_data$M68)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M68)), unique_M68)
    # Plots
    p <- ggplot(main_occupation_plot_data, aes(x = "", y = n, fill = M68)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) +
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    p_text <- ggplot(main_occupation_plot_data) +
      
      # === STYLE: Increased point size ===
      geom_point(aes(x = 0, y = label_y, color = M68), size = 3, shape = 15) +
      
      # === STYLE: Fixed text size (size = 2 inside aes) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") +
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === STYLE: Added hjust = 0 to align title ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2", hjust = 0)
      ) +
      
      ggh4x::force_panelsizes(rows = unit(3.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  

  
  output$Yearly_instrument <- renderPlot({
    yearly_instrument_plot_data <- yearly_instrument_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M69))
    
    ggplot(yearly_instrument_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Q14 Style Applied ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts matching Q14 ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")         
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  

  
  output$Yearly_instrument_total <- renderPlot({
    yearly_instrument_total_plot_data <- yearly_instrument_total_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M69))
    
    ggplot(yearly_instrument_total_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Q14 Style Applied ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts matching Q14 ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")         
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Income_activity <- renderPlot({
    income_activity_plot_data <- income_activity_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M70 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M70))
    
    ggplot(income_activity_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 to match Q8 style ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "no" = global_colors[2],
        "yes" = global_colors[1])) +
      
      # === CHANGE 2: Increased text size to 4 ===
      geom_text(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, 
        fontface = "bold"
      ) +
      
      labs(x = "Count", y = "Activity", fill = "Answer") +
      
      # === CHANGE 3: Applied Q8 Theme settings ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold"),         
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
      ) +
      
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(50)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Income_yearly <- renderPlot({
    income_yearly_plot_data <- income_yearly_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M71))
    ggplot(income_yearly_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2")
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Income_yearly <- renderPlot({
    income_yearly_plot_data <- income_yearly_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M71))
    
    ggplot(income_yearly_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Q14 Style Applied ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts matching Q14 ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")         
      ) +
      
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Income_from_yearly <- renderPlot({
    income_from_yearly_plot_data <- income_from_yearly_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M71))
    ggplot(income_from_yearly_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2")
      ) +
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Income_from_yearly <- renderPlot({
    income_from_yearly_plot_data <- income_from_yearly_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M71))
    
    ggplot(income_from_yearly_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Q14 Style Applied ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts matching Q14 ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")         
      ) +
      
      labs(x = "Spending", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Total_income <- renderPlot({
    total_income_plot_data <- total_income_data()%>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(S02 = paste0(S0, "\nN = ", M73))
    
    ggplot(total_income_plot_data, aes(x = sums)) +
      geom_histogram(bins = 20, fill = global_colors[1], color = "white") +
      
      # === CHANGE: Q14 Style Applied ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts matching Q14 ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")         
      ) +
      
      labs(x = "Percentage", y = "Count") + facet_wrap(~S02, ncol = 3)
  })
  
  output$Profession <- renderPlot({
    profession_plot_data <- profession_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M74 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M74))
    
    ggplot(profession_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 ===
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      
      scale_fill_manual(values = c(
        "no" = global_colors[2],
        "yes" = global_colors[1])) +
      
      # === CHANGE 2: Increased text size to 4 ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                position = position_dodge(width = 0.7), # Matched width
                hjust = 0.1, 
                size = 4, 
                fontface = "bold") +
      
      labs(x = "Count", y = "Profession", fill = "Answer") +
      
      # === CHANGE 3: Added Full Theme Block (Big Fonts) ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold"),         
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
      ) +
      
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Marital_status <- renderPlot({
    marital_status_plot_data <- marital_status_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(marital_status_plot_data, aes(y = M75, x = n)) +
      
      # === CHANGE 1: Added width = 0.7 ===
      geom_bar(stat = "identity", position = "dodge", fill = global_colors[1], width = 0.7) +
      
      # === CHANGE 2: Increased text size to 4 ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                position = position_dodge(width = 0.7), 
                fontface = "bold",
                hjust = 0.1, 
                size = 4) + # <-- Increased from 3.1
      
      labs(x = "Count", y = "Status") +
      
      # === CHANGE 3: Added Full Theme Block (Big Fonts) ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        
        # --- Bigger Fonts ---
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")
      ) +
      
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Paying_difficulty <- renderPlot({
    paying_difficulty_plot_data <- paying_difficulty_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100,
             label_y = -0.05 * row_number() ,
             label = paste0(M76, ": ", n, " (", round(percentage, 1), "%)")) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    ymin <- min(paying_difficulty_plot_data$label_y)
    
    unique_M76 <- unique(paying_difficulty_plot_data$M76)
    color_palette <- setNames(scales::brewer_pal(palette = "Blues")(length(unique_M76)), unique_M76)
    
    # Left Plot (Donut) - Unchanged except for theme
    p <- ggplot(paying_difficulty_plot_data, aes(x = "", y = n, fill = M76)) +
      geom_bar(stat = "identity", width = 1, position = position_fill()) +
      coord_polar("y", start = 0) +
      theme_void() + facet_wrap(~S02, ncol = 2) +
      scale_fill_manual(values = color_palette) + 
      theme(legend.position = "none",
            strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"))
    
    # Right Plot (Text) - Updated Sizes
    p_text <- ggplot(paying_difficulty_plot_data) +
      
      # === CHANGE 1: Increased point size to 3 ===
      geom_point(aes(x = 0, y = label_y, color = M76), size = 3, shape = 15) +
      
      # === CHANGE 2: Increased text size to 4 (was 1.4) ===
      geom_text(aes(x = 0.2, y = label_y, label = label, size = 2), hjust = 0,
                fontface = "bold") + # Note: size=2 inside aes often maps to ~6mm, visually matches size 4 fixed
      
      theme_void() + xlim(0, 3) + ylim(ymin, 0) + theme(legend.position = "none") +
      facet_wrap(~S0, ncol = 1) + scale_color_manual(values = color_palette) +
      
      # === CHANGE 3: Added Theme for Strip Text ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2")
      ) +
      ggh4x::force_panelsizes(rows = unit(2.5, "cm")) + coord_cartesian(clip = "off")
    
    plot_grid(p, p_text, ncol = 2, rel_widths = c(3, 2))
  })
  
  output$Society_place <- renderPlot({
    society_place_plot_data <- society_place_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(society_place_plot_data, aes(y = M77, x = n)) +
      
      # === CHANGE 1: Added width = 0.7 ===
      geom_bar(stat = "identity", position = "dodge", fill = global_colors[1], width = 0.7) +
      
      # === CHANGE 2: Increased text size to 4 ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                position = position_dodge(width = 0.7),
                hjust = 0.1, 
                size = 4, 
                fontface = "bold") +
      
      labs(x = "Count", y = "Place") +
      
      # === CHANGE 3: Added Full Theme Block ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")
      ) +
      
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Political_scale <- renderPlot({
    political_scale_plot_data <- political_scale_data() %>%
      group_by(S0) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", sum(n)))
    
    ggplot(political_scale_plot_data, aes(y = M78, x = n)) +
      
      # === CHANGE 1: Added width = 0.7 ===
      geom_bar(stat = "identity", position = "dodge", fill = global_colors[1], width = 0.7) +
      
      # === CHANGE 2: Increased text size to 4 ===
      geom_text(aes(x = n - n/5, label = paste0(n, " (", round(percentage, 1), "%)")),
                position = position_dodge(width = 0.7),
                hjust = 0.1, 
                size = 4, 
                fontface = "bold") +
      
      labs(x = "Count", y = "Scale") +
      
      # === CHANGE 3: Added Full Theme Block ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold")
      ) +
      
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
  output$Person_description <- renderPlot({
    person_description_plot_data <- person_description_data() %>%
      group_by(S0) %>%
      left_join(culture_n(), by = "S0") %>%
      mutate(percentage = n / M79 * 100) %>%
      mutate(S02 = paste0(S0, "\nN = ", M79))
    
    ggplot(person_description_plot_data, aes(y = reason, x = n, fill = value)) +
      
      # === CHANGE 1: Added width = 0.7 ===
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      
      scale_fill_manual(values = c(
        "Mot like me at all" = global_colors[5],
        "Not like me" = global_colors[2],
        "A little like me" = global_colors[7],
        "Somewhat like me" = global_colors[6],
        "Like me" = global_colors[4],
        "Very much like me" = global_colors[1])) +
      
      # === CHANGE 2: Increased text size to 4 and bold ===
      geom_text_repel(
        aes(label = paste0(n, " (", round(percentage, 1), "%)")),
        position = position_stack(vjust = 0.5),
        size     = 4, # <-- Increased from 2.5
        direction = "y",
        box.padding = 0.1, 
        fontface = "bold"
      ) +
      
      labs(x = "Count", y = "Description", fill = "Answer") +
      
      # === CHANGE 3: Added Full Theme Block ===
      theme(
        strip.text = element_text(size = 18, face = "bold", color = "#4E6AA2"),
        axis.text.y = element_text(size = 14, margin = margin(r = 5)), 
        axis.text.x = element_text(size = 14, margin = margin(t = 5)), 
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)
      ) +
      
      facet_wrap(~S02, ncol = 3, scales = "free_x") +
      scale_y_discrete(labels = scales::label_wrap(40)) + coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
