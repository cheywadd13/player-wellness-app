# ============================================================
# PLAYER WELLNESS APPLICATION
# Player-Facing Version
# ============================================================


# ============================================================
# PACKAGES
# ============================================================

library(shiny)
library(DBI)
library(RPostgres)
library(bslib)
library(dotenv)


# ============================================================
# LOAD SUPABASE CREDENTIALS
# ============================================================
# Load local credentials when running on my computer.
# Online deployment will use environment variables stored in Posit Connect Cloud.

local_env <- "C:/Users/cheye/Desktop/SB Webapp/.env"

if (file.exists(local_env)) {
  load_dot_env(local_env)
}


# ============================================================
# DATABASE FUNCTIONS
# ============================================================

get_connection <- function() {
  
  dbConnect(
    RPostgres::Postgres(),
    
    host = Sys.getenv("SUPABASE_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_PORT")),
    dbname = Sys.getenv("SUPABASE_DB"),
    user = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    
    sslmode = "require"
  )
}


# ------------------------------------------------------------
# GET ACTIVE PLAYERS
# ------------------------------------------------------------

get_players <- function() {
  
  con <- get_connection()
  on.exit(dbDisconnect(con))
  
  dbGetQuery(
    con,
    "
    SELECT
      player_id,
      player_name
    FROM players
    WHERE active = TRUE
    ORDER BY player_name
    "
  )
}


# ============================================================
# PLAYER LIST
# ============================================================

player_list <- get_players()

player_choices <- setNames(
  player_list$player_id,
  player_list$player_name
)


# ============================================================
# USER INTERFACE
# ============================================================

ui <- page_fillable(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#17365D"
  ),
  
  
  # ==========================================================
  # CUSTOM PHONE-FRIENDLY DESIGN
  # ==========================================================
  
  tags$head(
    
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$style(
      HTML(
        "
        
        body {
          background-color: #f4f6f9;
        }
        
        .wellness-container {
          width: 100%;
          max-width: 850px;
          margin: auto;
          padding: 25px 15px 60px 15px;
        }
        
        .wellness-header {
          text-align: center;
          margin-bottom: 30px;
        }
        
        .wellness-header h1 {
          font-weight: 700;
          color: #17365D;
          margin-bottom: 8px;
        }
        
        .wellness-header p {
          color: #6c757d;
          font-size: 17px;
        }
        
        .card {
          border: none;
          border-radius: 14px;
          box-shadow: 0 3px 12px rgba(0,0,0,0.06);
        }
        
        .card-header {
          background-color: white;
          border-bottom: 1px solid #eeeeee;
          font-weight: 600;
        }
        
        .form-control,
        .form-select {
          min-height: 48px;
          font-size: 16px;
        }
        
        .btn-lg {
          min-height: 55px;
          font-size: 18px;
          font-weight: 600;
          border-radius: 10px;
        }
        
        .success-box {
          background-color: #eaf7ee;
          border: 1px solid #b7dfc2;
          border-left: 6px solid #198754;
          padding: 30px 20px;
          border-radius: 12px;
          text-align: center;
          margin-top: 25px;
        }
        
        .success-box h2 {
          color: #198754;
          font-weight: 700;
        }
        
        .privacy-note {
          color: #777;
          font-size: 13px;
          text-align: center;
          margin-top: 20px;
        }
        
        @media (max-width: 600px) {
          
          .wellness-container {
            padding: 15px 10px 40px 10px;
          }
          
          .wellness-header h1 {
            font-size: 28px;
          }
          
          .card-body {
            padding: 18px;
          }
        }
        
        "
      )
    )
  ),
  
  
  div(
    
    class = "wellness-container",
    
    
    # ========================================================
    # HEADER
    # ========================================================
    
    div(
      
      class = "wellness-header",
      
      h1("Player Wellness"),
      
      p(
        "Complete your daily wellness check-in."
      )
    ),
    
    
    # ========================================================
    # CHECK-IN FORM
    # ========================================================
    
    uiOutput("checkin_form"),
    
    
    # ========================================================
    # CONFIRMATION
    # ========================================================
    
    uiOutput("confirmation")
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # Tracks whether the player successfully submitted
  submitted <- reactiveVal(FALSE)
  
  
  # ==========================================================
  # CHECK-IN FORM
  # ==========================================================
  
  output$checkin_form <- renderUI({
    
    if (submitted()) {
      return(NULL)
    }
    
    
    tagList(
      
      
      # ======================================================
      # PLAYER
      # ======================================================
      
      card(
        
        card_header(
          h4("Player")
        ),
        
        card_body(
          
          selectInput(
            "player_id",
            "Select Your Name",
            
            choices = c(
              "Select your name" = "",
              player_choices
            ),
            
            selected = "",
            width = "100%"
          )
        )
      ),
      
      
      br(),
      
      
      # ======================================================
      # STRESS
      # ======================================================
      
      card(
        
        card_header(
          h4("Stress Level")
        ),
        
        card_body(
          
          radioButtons(
            "stress_level",
            "How are you feeling today?",
            
            choices = c(
              "🟢 Green - Low Stress" = "Green",
              "🟡 Yellow - Medium Stress" = "Yellow",
              "🔴 Red - High Stress" = "Red"
            ),
            
            selected = character(0)
          ),
          
          
          conditionalPanel(
            
            condition = "input.stress_level == 'Red'",
            
            selectInput(
              "stress_reason",
              "Main Reason for Stress",
              
              choices = c(
                "Select a reason" = "",
                "School",
                "Practice",
                "Competition",
                "Work",
                "Family",
                "Relationships",
                "Sleep",
                "Injury",
                "Physical Soreness",
                "Personal",
                "Other"
              ),
              
              width = "100%"
            ),
            
            
            textAreaInput(
              "stress_note",
              "Additional Note",
              placeholder = "Optional...",
              width = "100%",
              rows = 3
            )
          )
        )
      ),
      
      
      br(),
      
      
      # ======================================================
      # SLEEP
      # ======================================================
      
      card(
        
        card_header(
          h4("Sleep")
        ),
        
        card_body(
          
          selectInput(
            "bed_time",
            "What time did you go to sleep?",
            
            choices = c(
              "Select bedtime" = "",
              "8:00 PM",
              "8:30 PM",
              "9:00 PM",
              "9:30 PM",
              "10:00 PM",
              "10:30 PM",
              "11:00 PM",
              "11:30 PM",
              "12:00 AM",
              "12:30 AM",
              "1:00 AM",
              "1:30 AM",
              "2:00 AM",
              "2:30 AM",
              "3:00 AM",
              "After 3:00 AM"
            ),
            
            width = "100%"
          ),
          
          
          selectInput(
            "wake_time",
            "What time did you wake up?",
            
            choices = c(
              "Select wake time" = "",
              "4:00 AM",
              "4:30 AM",
              "5:00 AM",
              "5:30 AM",
              "6:00 AM",
              "6:30 AM",
              "7:00 AM",
              "7:30 AM",
              "8:00 AM",
              "8:30 AM",
              "9:00 AM",
              "9:30 AM",
              "10:00 AM",
              "10:30 AM",
              "11:00 AM"
            ),
            
            width = "100%"
          ),
          
          
          selectInput(
            "sleep_hours",
            "How many hours did you sleep?",
            
            choices = c(
              "Select hours" = "",
              "Less than 4" = 3.5,
              "4" = 4,
              "4.5" = 4.5,
              "5" = 5,
              "5.5" = 5.5,
              "6" = 6,
              "6.5" = 6.5,
              "7" = 7,
              "7.5" = 7.5,
              "8" = 8,
              "8.5" = 8.5,
              "9" = 9,
              "9.5" = 9.5,
              "10" = 10,
              "More than 10" = 10.5
            ),
            
            width = "100%"
          ),
          
          
          selectInput(
            "sleep_quality",
            "How well did you sleep?",
            
            choices = c(
              "Select sleep quality" = "",
              "Very Poor",
              "Poor",
              "Average",
              "Good",
              "Very Good"
            ),
            
            width = "100%"
          )
        )
      ),
      
      
      br(),
      
      
      # ======================================================
      # BODY
      # ======================================================
      
      card(
        
        card_header(
          h4("Body")
        ),
        
        card_body(
          
          selectInput(
            "body_feeling",
            "How does your body feel today?",
            
            choices = c(
              "Select one" = "",
              "Great",
              "Good",
              "Normal",
              "Sore",
              "Very Sore",
              "In Pain"
            ),
            
            width = "100%"
          )
        )
      ),
      
      
      br(),
      
      
      # ======================================================
      # DAILY NOTE
      # ======================================================
      
      card(
        
        card_header(
          h4("Daily Note")
        ),
        
        card_body(
          
          textAreaInput(
            "daily_note",
            "Anything else Coach Weeks should know?",
            placeholder = "Optional...",
            width = "100%",
            rows = 4
          )
        )
      ),
      
      
      br(),
      
      
      # ======================================================
      # SUBMIT BUTTON
      # ======================================================
      
      actionButton(
        "submit",
        "Submit Check-In",
        class = "btn-primary btn-lg w-100"
      ),
      
      
      div(
        class = "privacy-note",
        "Your check-in is submitted directly to the team wellness database."
      )
    )
  })
  
  
  # ==========================================================
  # SUBMIT CHECK-IN
  # ==========================================================
  
  observeEvent(
    
    input$submit,
    
    {
      
      
      # ======================================================
      # REQUIRED FIELD VALIDATION
      # ======================================================
      
      if (
        is.null(input$player_id) ||
        input$player_id == "" ||
        is.null(input$stress_level) ||
        input$bed_time == "" ||
        input$wake_time == "" ||
        input$sleep_hours == "" ||
        input$sleep_quality == "" ||
        input$body_feeling == ""
      ) {
        
        showNotification(
          "Please complete all required fields.",
          type = "error",
          duration = 5
        )
        
        return()
      }
      
      
      # ======================================================
      # RED STRESS VALIDATION
      # ======================================================
      
      if (
        input$stress_level == "Red" &&
        (
          is.null(input$stress_reason) ||
          input$stress_reason == ""
        )
      ) {
        
        showNotification(
          "Please select the main reason for your stress.",
          type = "error",
          duration = 5
        )
        
        return()
      }
      
      
      # ======================================================
      # CONNECT TO SUPABASE
      # ======================================================
      
      con <- get_connection()
      
      on.exit(
        dbDisconnect(con),
        add = TRUE
      )
      
      
      # ======================================================
      # CHECK FOR EXISTING DAILY CHECK-IN
      # ======================================================
      
      existing <- dbGetQuery(
        
        con,
        
        "
        SELECT checkin_id
        FROM checkins
        WHERE player_id = $1
          AND checkin_date = $2
        ",
        
        params = list(
          as.integer(input$player_id),
          Sys.Date()
        )
      )
      
      
      if (nrow(existing) > 0) {
        
        showNotification(
          "You already completed your check-in today.",
          type = "warning",
          duration = 6
        )
        
        return()
      }
      
      
      # ======================================================
      # STRESS DETAILS
      # ======================================================
      
      stress_reason <- NA_character_
      stress_note <- NA_character_
      
      
      if (input$stress_level == "Red") {
        
        stress_reason <- input$stress_reason
        
        if (
          !is.null(input$stress_note) &&
          nzchar(trimws(input$stress_note))
        ) {
          
          stress_note <- input$stress_note
        }
      }
      
      
      # ======================================================
      # DAILY NOTE
      # ======================================================
      
      daily_note <- NA_character_
      
      
      if (
        !is.null(input$daily_note) &&
        nzchar(trimws(input$daily_note))
      ) {
        
        daily_note <- input$daily_note
      }
      
      
      # ======================================================
      # INSERT INTO SUPABASE
      # ======================================================
      
      result <- tryCatch(
        
        {
          
          dbExecute(
            
            con,
            
            "
            INSERT INTO checkins (
              player_id,
              checkin_date,
              submission_time,
              stress_level,
              stress_reason,
              stress_note,
              bed_time,
              wake_time,
              sleep_hours,
              sleep_quality,
              body_feeling,
              daily_note
            )
            
            VALUES (
              $1,
              $2,
              $3,
              $4,
              $5,
              $6,
              $7,
              $8,
              $9,
              $10,
              $11,
              $12
            )
            ",
            
            params = list(
              as.integer(input$player_id),
              Sys.Date(),
              format(
                Sys.time(),
                "%H:%M:%S"
              ),
              input$stress_level,
              stress_reason,
              stress_note,
              input$bed_time,
              input$wake_time,
              as.numeric(input$sleep_hours),
              input$sleep_quality,
              input$body_feeling,
              daily_note
            )
          )
          
          
          TRUE
          
        },
        
        
        error = function(e) {
          
          message(
            "Database error: ",
            e$message
          )
          
          FALSE
        }
      )
      
      
      # ======================================================
      # SUCCESS / ERROR
      # ======================================================
      
      if (result) {
        
        submitted(TRUE)
        
        
        showNotification(
          "Check-in saved successfully!",
          type = "message",
          duration = 4
        )
        
      } else {
        
        showNotification(
          paste(
            "Your check-in could not be saved.",
            "Please try again."
          ),
          type = "error",
          duration = 7
        )
      }
    }
  )
  
  
  # ==========================================================
  # SUCCESS SCREEN
  # ==========================================================
  
  output$confirmation <- renderUI({
    
    if (!submitted()) {
      return(NULL)
    }
    
    
    div(
      
      class = "success-box",
      
      h2("✓ Check-In Submitted"),
      
      h4("Thank you!"),
      
      p(
        "Your daily wellness check-in was saved successfully."
      ),
      
      p(
        "Coach Weeks will be able to view your response on the wellness dashboard."
      ),
      
      br(),
      
      strong(
        "You may now close this page."
      )
    )
  })
}


# ============================================================
# RUN PLAYER APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)