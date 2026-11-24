## Acute Chronic Workload Ratio Activity Tracker
## Program name: ACWR_Tracker.R
## Author: Erika Mead
## Date: 2026
## Purpose: allow user to input daily activities and intensity and duration
##          calculate acute chronic workload ratio
##          create graphs to visualize
##          allow user to plan next activity duration and intensity to stay within
##          optimal ratio
##########################################################################################

# If the packages below are not already installed, make sure you install them. 

library(shiny)
library(ggplot2)
library(shinyBS)
library(colourpicker)
library(dplyr)
library(DT)
library(googlesheets4)
library(googledrive)
library(lubridate)

#-------------------------------------------------------------------------------------
# Google Service Account Authentication
# Note: This is the name of my json file for my google sheet. It will change based on 
# the json file and google sheet you use. Make sure you change these lines because 
# they will not work for you.
#-------------------------------------------------------------------------------------
gs4_auth(path = "science-fair-25-26-480503-f7adfcfd2402.json")
drive_auth(path = "science-fair-25-26-480503-f7adfcfd2402.json")

#-------------------------------------------------------------------------------------
# Google Sheet Information
# Note: sheet_id is the url of my google sheet. You will have to get the url of your
# google sheet to connect to the app. Make sure you change this line once you have
# created your own google sheet with a data tab, activities tab, and colors tab.
#-------------------------------------------------------------------------------------
sheet_id <-"https://docs.google.com/spreadsheets/d/1iiHv9shnGi-0TNIElZx43ylRRBwuEihofoQ4t-bYDH0/edit?gid=0#gid=0"
data_tab <- "data"
activities_tab <- "activities"
colors_tab <- "colors"

# ------------------------------
# User Interface (UI)
# ------------------------------
ui <- fluidPage(
  titlePanel("🏃 Acute Chronic Workload Ratio Activity Tracker"),
  
  tags$head(
    tags$style(HTML("
      .btn-success { background-color: #28a745; color: white; }
      .btn-primary { background-color: #007bff; color: white; }
      .btn-danger { background-color: #dc3545; color: white; }
      body { background-color: #f9f9f9; }
      .shiny-output-error-validation { color: red; font-weight: bold; }
    "))
  ),
# ------------------------------
# This creates the sidebar, with the instructions for using the app, buttons to add 
# and select activities and assign a color to each activity, buttons to switch between
# the graph, analysis, and table, and buttons to refresh, save, reset, and download data.
# ------------------------------
  
  sidebarLayout(
    sidebarPanel(
      helpText(
        "For each day, enter the intensity, duration, and pain level for each activity
         you completed. You can add multiple activities per day.
         Use the Graph tab to visualize data, Analysis to plan upcoming sessions,
         and History to view saved data."
      ),
      br(),
      
      textInput("new_activity", "Add New Activity"),
      actionButton("add_activity", "Add Activity", class = "btn-success"),
      br(), br(),
      
      uiOutput("activity_selector_ui"),
      uiOutput("color_picker_ui"),
      uiOutput("activityInputs"),
      
      radioButtons(
        "view", "View:",
        choices = c("Graph", "Analysis", "History"),
        selected = "Graph"
      ),
      
      dateRangeInput(
        "view_dates",
        "Select Date Range:",
        start = Sys.Date() - 29,
        end = Sys.Date(),
        format = "mm/dd/yyyy"
      ),
      
      conditionalPanel(
        condition = "input.view == 'Analysis'",
        selectInput("analysis_activities", "Select Activities:",
                    choices = NULL, multiple = TRUE),
        radioButtons("fix_type", "Fix Variable:",
                     choices = c("Intensity", "Duration"),
                     selected = "Intensity"),
        numericInput("fix_value", "Desired Value",
                     value = 5, min = 0.01, step = 0.25)
      ),
      
      actionButton("refresh", "Refresh from Google Sheets"),
      actionButton("save", "Save Entry", class = "btn-primary"),
      actionButton("reset_all", "Reset All Data", class = "btn-danger"),
      
      br(), br(),
      
      textOutput("save_msg"),
      downloadButton("downloadData", "⬇️ Download CSV")
    ),
    
    mainPanel(
      uiOutput("mainOutput"),
      tags$footer(
        HTML("<div style='text-align:center; color:#666;'><em>Erika Mead 2026</em></div>")
      )
    )
  )
)

# ------------------------------
# Server
# ------------------------------
server <- function(input, output, session) {
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  edit_index <- reactiveVal(NULL)
  
  # ------------------------------
  # This reads the google sheet.
  # ------------------------------
  safe_read_sheet <- function(sheet, tab) {
    if (tab %in% sheet_names(sheet)) {
      suppressMessages(read_sheet(sheet, sheet = tab))
    } else data.frame()
  }
  
  vals <- reactiveValues(
    activities = character(0),
    colors = list()
  )
  
  # ------------------------------
  # This loads the data from the google sheet.
  # ------------------------------
  data_rv <- reactiveVal(data.frame(
    date = as.Date(character()),
    activity = character(),
    intensity = numeric(),
    duration = numeric(),
    effort = numeric(),
    pain = numeric(),
    notes = character(),
    stringsAsFactors = FALSE
  ))
  
  reset_with_meta <- reactiveVal(FALSE)
  
  observe({
    isolate({
      act_df <- safe_read_sheet(sheet_id, activities_tab)
      col_df <- safe_read_sheet(sheet_id, colors_tab)
      df <- safe_read_sheet(sheet_id, data_tab)
      
      if (nrow(act_df) > 0) vals$activities <- act_df$activity
      
      if (nrow(col_df) > 0) {
        for (i in seq_len(nrow(col_df))) {
          vals$colors[[col_df$activity[i]]] <- col_df$color[i]
        }
      }
      
      if (nrow(df) > 0) {
        df$date <- as.Date(df$date)
        df$effort <- df$intensity * df$duration
        data_rv(df)
      }
    })
  })
  
  # ------------------------------
  # This saves data to the google sheet
  # ------------------------------
  save_data <- function(df) {
    sheet_write(df, ss = sheet_id, sheet = data_tab)
    data_rv(df)
  }
  
  save_activities <- function() {
    if (length(vals$activities) == 0) return()
    sheet_write(
      data.frame(activity = vals$activities),
      ss = sheet_id, sheet = activities_tab
    )
    sheet_write(
      data.frame(
        activity = vals$activities,
        color = sapply(vals$activities, function(a) vals$colors[[a]])
      ),
      ss = sheet_id, sheet = colors_tab
    )
  }
  
  # ------------------------------
  # This sets up the user interface for adding and saving activities. 
  # You select the date, then add the intensity, duration, pain level, 
  # and notes.
  # ------------------------------
  output$activity_selector_ui <- renderUI({
    if (length(vals$activities) == 0)
      helpText("No activities added yet.")
    else
      selectInput("activity", "Select Activity",
                  choices = vals$activities,
                  selected = vals$activities[1])
  })
  
  output$color_picker_ui <- renderUI({
    tagList(
      lapply(vals$activities, function(act) {
        colourInput(
          paste0("col_", make.names(act)),
          paste("Color for", act),
          value = vals$colors[[act]] %||% "#007bff"
        )
      })
    )
  })
  
  observe({
    for (act in vals$activities) {
      id <- paste0("col_", make.names(act))
      if (!is.null(input[[id]]))
        vals$colors[[act]] <- input[[id]]
    }
  })
  
  output$activityInputs <- renderUI({
    req(input$activity)
    tagList(
      dateInput("date", "Date", Sys.Date()),
      sliderInput("intensity", "Intensity", 0, 10, 1, step = 0.25),
      sliderInput("duration", "Duration (hours)", 0, 5, 1, step = 0.25),
      sliderInput("pain", "Pain Level", 0, 10, 0, step = 0.25),
      textAreaInput("notes", "Notes")
    )
  })
  
  # ------------------------------
  # This allows you to add activities.
  # ------------------------------
  observeEvent(input$add_activity, {
    new_act <- trimws(input$new_activity)
    if (nzchar(new_act) && !(new_act %in% vals$activities)) {
      vals$activities <- c(vals$activities, new_act)
      vals$colors[[new_act]] <- "#007bff"
      save_activities()
      updateTextInput(session, "new_activity", value = "")
    }
  })
  
  # ------------------------------
  # This allows you to save entries.
  # ------------------------------
  observeEvent(input$save, {
    req(input$activity, input$date)
    
    new_entry <- data.frame(
      date = as.Date(input$date),
      activity = input$activity,
      intensity = input$intensity,
      duration = input$duration,
      effort = input$intensity * input$duration,
      pain = input$pain,
      notes = input$notes,
      stringsAsFactors = FALSE
    )
    
    df <- data_rv()
    exists <- df$date == new_entry$date & df$activity == new_entry$activity
    if (any(exists)) df[exists, ] <- new_entry else df <- rbind(df, new_entry)
    
    save_data(df)
    save_activities()
    
    output$save_msg <- renderText(
      paste("✅ Saved for", input$activity)
    )
  })
  
  # ------------------------------
  # This allows you to edit and delete entries.
  # ------------------------------
  editModal <- function(entry) {
    modalDialog(
      title = paste("Edit Entry –", entry$activity, entry$date),
      numericInput("edit_intensity", "Intensity",
                   entry$intensity, 0, 10, 0.25),
      numericInput("edit_duration", "Duration (hours)",
                   entry$duration, 0, 10, 0.25),
      numericInput("edit_pain", "Pain",
                   entry$pain, 0, 10, 0.25),
      textAreaInput("edit_notes", "Notes", entry$notes),
      footer = tagList(
        actionButton("confirm_edit", "Save Changes", class = "btn-primary"),
        actionButton("confirm_delete", "Delete Entry", class = "btn-danger"),
        modalButton("Cancel")
      )
    )
  }
  
  observeEvent(input$dataTable_rows_selected, {
    edit_index(input$dataTable_rows_selected)
    showModal(editModal(data_rv()[edit_index(), ]))
  })
  
  observeEvent(input$plot_click, {
    df <- data_rv()
    near <- nearPoints(df, input$plot_click, threshold = 10, maxpoints = 1)
    if (nrow(near) == 1) {
      i <- which(df$date == near$date & df$activity == near$activity)
      edit_index(i)
      showModal(editModal(near))
    }
  })

# -----------------------------------
# This asks you for confirmation before editing or deleting an entry.
# -----------------------------------
  
  observeEvent(input$confirm_edit, {
    df <- data_rv()
    i <- edit_index()
    if (!is.null(i)) {
      df$intensity[i] <- input$edit_intensity
      df$duration[i] <- input$edit_duration
      df$pain[i] <- input$edit_pain
      df$notes[i] <- input$edit_notes
      df$effort[i] <- input$edit_intensity * input$edit_duration
      save_data(df)
    }
    removeModal()
  })
  
  observeEvent(input$confirm_delete, {
    df <- data_rv()
    i <- edit_index()
    if (!is.null(i)) {
      df <- df[-i, ]
      save_data(df)
    }
    removeModal()
  })

  # -----------------------------------------
  # This refreshes data from the google sheet.
  # -----------------------------------------
      
  observeEvent(input$refresh, {
    act_df <- safe_read_sheet(sheet_id, activities_tab)
    col_df <- safe_read_sheet(sheet_id, colors_tab)
    df     <- safe_read_sheet(sheet_id, data_tab)
    
    vals$activities <- act_df$activity %||% character(0)
    
    vals$colors <- list()
    if (nrow(col_df) > 0) {
      for (i in seq_len(nrow(col_df))) {
        vals$colors[[col_df$activity[i]]] <- col_df$color[i]
      }
    }
    
    if (nrow(df) > 0) {
      df$date <- as.Date(df$date)
      df$effort <- df$intensity * df$duration
    } else {
      df <- data.frame(
        date = as.Date(character()),
        activity = character(),
        intensity = numeric(),
        duration = numeric(),
        effort = numeric(),
        pain = numeric(),
        notes = character(),
        stringsAsFactors = FALSE
      )
    }
    
    data_rv(df)
    output$save_msg <- renderText("🔄 Refreshed from Google Sheets.")
  })
  
  # ------------------------------
  # This asks for confirmation before resetting all data.
  # ------------------------------
  
  observeEvent(input$reset_all, {
    reset_with_meta(FALSE)
    
    showModal(modalDialog(
      title = "⚠️ Reset All Data",
      "Do you also want to reset all activities and colors?",
      footer = tagList(
        actionButton(
          "reset_all_yes",
          "Yes, reset all activities and colors",
          class = "btn-danger"
        ),
        actionButton(
          "reset_all_no",
          "No",
          class = "btn-secondary"
        ),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$reset_all_yes, {
    reset_with_meta(TRUE)
    removeModal()
    
    showModal(modalDialog(
      title = "⚠️ Final Confirmation",
      "Are you sure you want to reset all data, activities, and colors?",
      "This action cannot be undone.",
      footer = tagList(
        actionButton("confirm_reset_all", "Yes, Reset Everything", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$reset_all_no, {
    reset_with_meta(FALSE)
    removeModal()
    
    showModal(modalDialog(
      title = "⚠️ Final Confirmation",
      "Are you sure you want to reset all activity data?",
      "Activities and colors will be kept.",
      footer = tagList(
        actionButton("confirm_reset_all", "Yes, Reset Data Only", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirm_reset_all, {
    removeModal()
    
# -----------------------------------
# This resets all data from the google sheet.
# -----------------------------------
    
    empty_data <- data.frame(
      date = as.Date(character()),
      activity = character(),
      intensity = numeric(),
      duration = numeric(),
      effort = numeric(),
      pain = numeric(),
      notes = character(),
      stringsAsFactors = FALSE
    )
    
    sheet_write(empty_data, ss = sheet_id, sheet = data_tab)
    data_rv(empty_data)
    
  
    if (reset_with_meta()) {
      vals$activities <- character(0)
      vals$colors <- list()
      
      sheet_write(
        data.frame(activity = character()),
        ss = sheet_id, sheet = activities_tab
      )
      
      sheet_write(
        data.frame(activity = character(), color = character()),
        ss = sheet_id, sheet = colors_tab
      )
    }
    
    output$save_msg <- renderText(
      if (reset_with_meta())
        "🗑️ All data, activities, and colors have been reset."
      else
        "🗑️ All activity data has been reset."
    )
    
    reset_with_meta(FALSE)
  })
  
  observe({
    updateSelectInput(
      session,
      "analysis_activities",
      choices = vals$activities,
      selected = vals$activities
    )
  })
  
  # ------------------------------
  # This is the output. This allows you to switch between the graph, analysis, and history.
  # ------------------------------
  output$mainOutput <- renderUI({
    switch(input$view,
           "Graph" = {
             tagList(
               radioButtons("graph_type", "Graph Type:",
                            choices = c("Line", "Bar"), selected = "Line"),
               selectInput("trend_type", "Trend Lines:",
                           choices = c("None","Overall Trend","Per-Activity Trends")),
               selectInput("bar_interval", "Bar Graph Interval:",
                           choices = c("Day","Week","Month"), selected = "Week"),
               checkboxGroupInput("compare_activities",
                                  "Select Activities to Show:",
                                  choices = vals$activities,
                                  selected = vals$activities),
               plotOutput("plot", height = "550px", click = "plot_click")
             )
           },
           "Analysis" = verbatimTextOutput("analysis"),
           "History" = {
             tagList(
               checkboxGroupInput("compare_activities_table",
                                  "Select Activities:",
                                  choices = vals$activities,
                                  selected = vals$activities),
               DTOutput("dataTable")
             )
           }
    )
  })
  
  # ----------------------------------
  # This creates the analysis.
  # ----------------------------------
      
  output$analysis <- renderText({
    all_df <- data_rv()
    
    if (nrow(all_df) == 0) {
      return("No data available.")
    }
    
    req(input$view_dates)
    
    all_df <- all_df[
      all_df$activity %in% input$analysis_activities,
    ]
    
    if (nrow(all_df) == 0) {
      return("No data for selected activities.")
    }
    
    start_date <- as.Date(input$view_dates[1])
    end_date <- as.Date(input$view_dates[2])
    
    df <- all_df[
      all_df$date >= start_date &
        all_df$date <= end_date,
    ]
    
    if (nrow(df) == 0) {
      return("No data in the selected date range.")
    }
    
    # ----------------------------------------
    # This calculates the acute and chronic loads, the acute chronic
    # workload ratio, a recommended acute chronic workload ratio
    # range, and the training zone (undertraining, optimal training, 
    # or overtraining.
    # ----------------------------------------
    daily <- df %>%
      group_by(date) %>%
      summarise(
        total_effort = sum(effort, na.rm = TRUE),
        avg_pain = if (all(is.na(pain))) {
          NA_real_
        } else {
          mean(pain, na.rm = TRUE)
        },
        .groups = "drop"
      )
    
    avgAll <- mean(daily$total_effort, na.rm = TRUE)
    
    daily_all <- all_df %>%
      group_by(date) %>%
      summarise(
        total_effort = sum(effort, na.rm = TRUE),
        .groups = "drop"
      )
    
    avg_daily <- function(days) {
      dates <- seq.Date(
        end_date - (days - 1),
        end_date,
        by = "day"
      )
      
      sum(
        daily_all$total_effort[
          daily_all$date %in% dates
        ],
        na.rm = TRUE
      ) / days
    }
    
    avg7 <- avg_daily(7)
    avg30 <- avg_daily(30)
    
    lower <- 0.8 * avg30
    upper <- 1.3 * avg30
    acwr <- if (avg30 > 0) avg7 / avg30 else NA_real_
    
    complete_pairs <- daily %>%
      filter(
        !is.na(total_effort),
        !is.na(avg_pain)
      )
    
    cor_val <- if (
      nrow(complete_pairs) >= 2 &&
      sd(complete_pairs$total_effort) > 0 &&
      sd(complete_pairs$avg_pain) > 0
    ) {
      cor(
        complete_pairs$total_effort,
        complete_pairs$avg_pain
      )
    } else {
      NA_real_
    }
    
    zone <- if (is.na(acwr)) {
      "⚪ Insufficient baseline data"
    } else if (acwr < 0.8) {
      "🔵 Low"
    } else if (acwr < 1.3) {
      "🟢 Optimal"
    } else if (acwr < 1.5) {
      "🟡 Caution"
    } else {
      "🔴 High"
    }

# ---------------------------------
# This allows you to plan future activities based on a desired intensity or duration.
# For example, if you want an intensity of 5 it can recommend a duration range to stay
# in an optimal training zone based on the acwr.
# ---------------------------------
    if (is.null(input$fix_value) ||
        is.na(input$fix_value) ||
        input$fix_value <= 0) {
      range_text <- "Enter a planning value greater than 0."
    } else if (input$fix_type == "Intensity") {
      range_text <- sprintf(
        "Suggested Duration Range (per session): %.2f – %.2f hours",
        lower / input$fix_value,
        upper / input$fix_value
      )
    } else {
      range_text <- sprintf(
        "Suggested Intensity Range (per session): %.2f – %.2f",
        lower / input$fix_value,
        upper / input$fix_value
      )
    }

    paste0(
      "📊 Total Load Summary (Selected Activities)\n",
      "Average Daily Load (7-day): ", round(avg7, 2), "\n",
      "Average Daily Load (30-day): ", round(avg30, 2), "\n",
      "Average Daily Load (Selected Period): ", round(avgAll, 2), "\n\n",
      "⚖️ ACWR: ", ifelse(is.na(acwr), "N/A", round(acwr, 2)), "\n",      "🚦 Risk Zone: ", zone, "\n\n",
      "🎯 Target Daily Load Range: ", round(lower, 2), " – ", round(upper, 2), "\n\n",
      "🔢 Planning:\n", range_text, "\n\n",
      "📈 Daily Load–Pain Correlation: ",
      ifelse(is.na(cor_val), "N/A", round(cor_val, 2))
    )
    
  })

# --------------------------------
# This sets up the graphs.
# --------------------------------
  
  output$plot <- renderPlot({
    
    df <- data_rv()
    if (nrow(df) == 0) return()
    
    df <- df[df$activity %in% input$compare_activities, ]
    
    req(input$view_dates)
    df <- df[
      df$date >= input$view_dates[1] &
        df$date <= input$view_dates[2],
    ]
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No training records in this date range.")
      return()
    }
    
    cols <- sapply(unique(df$activity), function(a) vals$colors[[a]])
                   
    # This calculates the daily (acute) workload.
    daily_total <- df %>%
      group_by(date) %>%
      summarise(total_effort = sum(effort), .groups = "drop")
    
    
    end_date <- as.Date(input$view_dates[2])

    # This calculates the monthly (chronic) workload.
    last30 <- seq.Date(
      end_date - 29,
      end_date,
      by = "day"
    )
    
    all_df <- data_rv()
    all_df <- all_df[
      all_df$activity %in% input$compare_activities,
    ]
    
    daily_baseline <- all_df %>%
      group_by(date) %>%
      summarise(
        total_effort = sum(effort, na.rm = TRUE),
        .groups = "drop"
      )

    # This calculates the acute chronic workload ratio.
    avg30 <- sum(
      daily_baseline$total_effort[
        daily_baseline$date %in% last30
      ],
      na.rm = TRUE
    ) / 30

    # This is the upper and lower bounds of the optimal training zone
    # based on the acute chronic workload ratio.
    lower <- 0.8 * avg30
    upper <- 1.3 * avg30

  # --------------------------------
  # This sets up the bar graph, which can be grouped by day, week, or month.
  # --------------------------------
    
    if (input$graph_type == "Bar") {
      
      df$group <- switch(
        input$bar_interval,
        "Day"   = df$date,
        "Week"  = lubridate::floor_date(df$date, "week"),
        "Month" = lubridate::floor_date(df$date, "month")
      )
      
      interval_days <- switch(
        input$bar_interval,
        "Day"   = 1,
        "Week"  = 7,
        "Month" = 30
      )
      
      df$group <- as.Date(df$group)
      
      lower_bar <- lower * interval_days
      upper_bar <- upper * interval_days
      
      # This creates different colored zones on the graph that show undertraining, optimal training
      # and overtraining so you can compare your data to these zones.
      zone_df <- data.frame(
        ymin = c(0, lower_bar, upper_bar, 1.5 * upper_bar),
        ymax = c(lower_bar, upper_bar, 1.5 * upper_bar, Inf),
        zone = factor(c("Low", "Optimal", "Caution", "High"),
                      levels = c("Low", "Optimal", "Caution", "High"))
      )
      
      ggplot(df, aes(x = group, y = effort, fill = activity)) +
        geom_bar(stat = "summary", fun = "sum") +
        scale_fill_manual(values = cols, guide = "none") +
        geom_hline(yintercept = c(lower_bar, upper_bar),
                   linetype = "dashed", color = "red", linewidth = 1) +
        scale_x_date(
          date_labels = ifelse(input$bar_interval == "Month", "%b %Y",
                               ifelse(input$bar_interval == "Week", "%d %b", "%d %b")),
          expand = expansion(mult = c(0.01, 0.01))
        ) +
        ylab("Effort") + xlab(NULL) +
        theme_minimal(base_size = 16)
      
    } else {
      
  # -----------------------------------------
  # This sets up the line graph.
  # -----------------------------------------
      zone_df <- data.frame(
        ymin = c(0, 0.8 * avg30, 1.3 * avg30, 1.5 * avg30),
        ymax = c(0.8 * avg30, 1.3 * avg30, 1.5 * avg30, Inf),
        zone = factor(c("Low", "Optimal", "Caution", "High"),
                      levels = c("Low", "Optimal", "Caution", "High"))
      )
      
      p <- ggplot(df, aes(date, effort, color = activity)) +
        geom_rect(
          data = zone_df,
          aes(
            xmin = -Inf,
            xmax = Inf,
            ymin = ymin,
            ymax = ymax,
            fill = zone
          ),
          inherit.aes = FALSE,
          alpha = 0.08
        ) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = cols) +
        scale_fill_manual(
          values = c(
            "Low" = "#4da6ff",
            "Optimal" = "#5cd65c",
            "Caution" = "#ffd966",
            "High" = "#ff6666"
          ),
          guide = "none"
        ) +
        theme_minimal(base_size = 16) +
        geom_hline(
          yintercept = c(lower, upper),
          linetype = "dashed",
          color = "red",
          linewidth = 1
        )
      
      # This creates the overall and per-activity trend lines.
      if (input$trend_type == "Overall Trend") {
        p <- p + geom_smooth(
          data = daily_total,
          aes(date, total_effort),
          inherit.aes = FALSE,
          method = "loess", se = FALSE,
          color = "black", linewidth = 1.4, linetype = "longdash"
        )
      }
      
      if (input$trend_type == "Per-Activity Trends") {
        p <- p + geom_smooth(method = "loess", se = FALSE)
      }
      
      p
    }
  })
  
  
  # ------------------------------
  # This sets up the data table.
  # ------------------------------
  output$dataTable <- DT::renderDT({
    df <- data_rv()
    if (nrow(df) == 0) return(data.frame(Message = "No data available."))
    df <- df[df$activity %in% input$compare_activities_table, ]
    df <- df[order(df$date, decreasing = TRUE), ]
    df$date <- format(df$date, "%m/%d/%y")
    df[, c("date","activity","intensity","duration","effort","pain","notes")]
  }, selection = "single")
  
  # ------------------------------
  # This allows you to download a csv of your data.
  # ------------------------------
  output$downloadData <- downloadHandler(
    filename = function() paste0("activity_data_", Sys.Date(), ".csv"),
    content = function(file)
      write.csv(data_rv(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)



