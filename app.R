# =========================
# Libraries
# =========================
library(shiny)
library(shinydashboard)
library(readr)
library(plotly)
library(ggplot2)
library(dplyr)

# ===== GLOBAL THEME FOR ALL GGPLOT CHARTS =====
theme_set(
  theme_minimal(base_size = 13) +
    theme(
      # Standard Axis styling
      axis.line = element_line(color = "black", linewidth = 1.5),
      axis.ticks = element_line(color = "black", linewidth = 1.5),
      axis.text = element_text(color = "black", face = "bold"),
      axis.title = element_text(color = "black", face = "bold", size = 14),
      
      # Title styling
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      
      # Grid and Backgrounds (FIXED: No duplicates)
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "#F4F4F7", color = NA), # Soft Grey
      panel.background = element_rect(fill = "white", color = NA)   # White Plot Area
    )
)
# =========================
# Data Loading
# =========================
customers_data <- read_csv(
  "final_data.csv",
  show_col_types = FALSE,na = c("", "NA", "NULL"),
  col_types = cols(Gender = "c")
)
print(head(customers_data))


customers_data <- customers_data %>%
  mutate(
    # Fix the "FALSE" issue by renaming it back to "Female"
    Gender = ifelse(Gender == "FALSE" | Gender == "F", "Female", Gender),
    # Ensure Orders_range is treated as a category (Factor)
    Orders_range = as.factor(Orders_range)
  )

# ---- POISSON CALCULATION (GLOBAL) ----
customers_data$Retention_Probability <-
  customers_data$Retained_Customers / customers_data$Total_Customers
lambda <- mean(customers_data$x, na.rm = TRUE)
poisson_df <- data.frame(
  x = 0:max(customers_data$x, na.rm = TRUE),
  Poisson_Probability = dpois(
    0:max(customers_data$x, na.rm = TRUE),
    lambda
  )
)
customers_data$Poisson_Probability <-
  dpois(customers_data$x, lambda)


# =========================
# UI
# =========================
  
  # 2. UI -------------------------------------------------------------------
  ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
      title = tags$span(icon("chart-line"), " GEN-Z ANALYTICS", class = "logo-title"),
      titleWidth = 300
    ),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Dashboard Home", tabName = "home", icon = icon("dashboard")),
        menuItem("Consumer Retention", tabName = "Retention", icon = icon("star")),
        menuItem("Sales Analysis", tabName = "sales", icon = icon("chart-line")),
        menuItem("Consumer Behavior", tabName = "consumer", icon = icon("users")),
        menuItem("Purchase Influence", tabName = "purchase", icon = icon("bullhorn")),
        menuItem("Market Segmentation", tabName = "segment", icon = icon("layer-group")),
        menuItem("Buying Decision", tabName = "decision", icon = icon("check-circle")),
        menuItem("Summary Analysis", tabName = "summary_all", icon = icon("list-alt"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@600&display=swap');

      /* 1. Main Background: Soft Whisper Grey (Easier on eyes) */
      .content-wrapper, .right-side, .tab-pane, .tab-content { 
        background-color: #F4F4F7 !important; 
      }

      /* 2. Dashboard Header & Logo - Deep Maroon Gradient */
      .main-header .logo, .main-header .navbar { 
        background: linear-gradient(90deg, #5c1a1a, #8b2e2e) !important; 
      }
      .logo-title { 
        color: white !important; 
        font-family: 'Poppins', sans-serif;
      }

      /* 3. Box Styling: Clean White boxes on Grey background */
      .box { 
        border-top: 4px solid #5c1a1a !important; 
        background-color: #ffffff !important; 
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05) !important; /* Soft shadow */
      }
      .box-header { 
        background-color: #5c1a1a !important; 
        color: white !important; 
        border-radius: 8px 8px 0 0;
      }

      /* 4. Navigation Sidebar Colors */
      .main-sidebar { background-color: #222d32 !important; }

      /* 5. Typography */
      .heading { 
        font-size: 30px; 
        font-weight: 600; 
        text-align: center; 
        color: #5c1a1a; 
        padding: 20px 0; 
      }
      #ttest_result table {
  font-size: 18px;
  font-weight: bold;
}

#ttest_result th {
  font-size: 19px;
  font-weight: bold;
  background-color: #5c1a1a;
  color: white;
}

#ttest_result td {
  padding: 12px;
}

      /* Results & Stats styling */
      #purchase_stats, #chi_square_result, #ttest_result { 
        background-color: #fcfcfc !important;
        border: 1px solid #eee;
        color: #333333;
        font-family: monospace;
      }
    "))
      ),
      
      tabItems(
        tabItem(tabName = "home",
                fluidRow(
                  valueBoxOutput("box_avg_purchase", width = 3),
                  valueBoxOutput("box_avg_bill", width = 3),
                  valueBoxOutput("box_total_shoppers", width = 3),
                  valueBoxOutput("box_age_range", width = 3)
                ),
                fluidRow(box(width = 12, div("Gen-Z Shopping Analytics Overview", class = "heading"))),
                fluidRow(
                  box(width = 6, title = "Gender Distribution", status = "primary", solidHeader = TRUE, plotOutput("gender_pie", height = "350px")),
                  box(width = 6, title = "Spending Distribution", status = "primary", solidHeader = TRUE, plotOutput("spending_bar", height = "350px"))
                ),
                fluidRow(
                  box(width = 6, title = "Product Categories", status = "primary", solidHeader = TRUE, plotOutput("category_bar", height = "350px")),
                  box(width = 6, title = "Order Frequency", status = "primary", solidHeader = TRUE, plotOutput("orders_pie", height = "350px"))
             
                )
        )
        ,
      
        # ================= POOJA =================
        tabItem(
          tabName = "Retention",
          fluidRow(
            column(
              width = 10, offset = 1,
              # Retention and Poisson side-by-side
              box(
                width = 6, title = "Customer Retention Probability",
                status = "primary", solidHeader = TRUE,
                plotlyOutput("plot_retention", height = "400px")
              ),
              box(
                width = 6, title = "Poisson Distribution of Purchases",
                status = "primary", solidHeader = TRUE,
                plotlyOutput("plot_poisson", height = "400px")
              )
            )
          )
        ),
        
        # ================= MANJIRI =================
        tabItem(
          tabName = "sales",
          fluidRow(
            column(
              width = 10, offset = 1,
              box(
                width = 12,
                title = "Scatter Plot: Price vs Monthly Sales",
                status = "primary",
                solidHeader = TRUE,
                plotOutput("sales_plot", height = "520px")
              ),
              
              box(
                width = 12,
                title = "Price vs Monthly Sales – Correlation Analysis",
                status = "primary",
                solidHeader = TRUE,
                verbatimTextOutput("correlation_text")
              ),
              
              box(
                width = 12,
                title = "Linear Regression Results",
                status = "primary",
                solidHeader = TRUE,
                verbatimTextOutput("regression_text")
              ),
              
              box(
                width = 12,
                title = "Regression Model Summary",
                status = "primary",
                solidHeader = TRUE,
                verbatimTextOutput("model_summary")
              ),
              
              box(
                width = 12,
                title = "Predicted Monthly Sales",
                status = "primary",
                solidHeader = TRUE,
                tableOutput("prediction_table")
              )
            )
          )
        ),
        
      # ================= JIGISHA =================
      tabItem(
        tabName = "consumer",
        # Injecting CSS specifically for the stats output
        tags$head(tags$style(HTML("
    #purchase_stats { 
      font-size: 15px; 
      font-weight: bold; 
      line-height: 1.5; 
      color: #2c3e50;
      background-color: #ffffff;
    }
  "))),
        # Wide centered statistics box
        fluidRow(
          column(
            width = 10, offset = 1,
            box(
              width = 12,
              title = "Summary Statistics: Average Purchases",
              status = "primary",
              solidHeader = TRUE,
              verbatimTextOutput("purchase_stats")
            )
          )
        ),
        
        # Large centered Histogram
        fluidRow(
          column(
            width = 10, offset = 1,
            box(
              width = 12,
              title = "Distribution of Average Purchases",
              status = "primary",
              solidHeader = TRUE,
              plotOutput("purchase_hist", height = "450px") # Increased height for impact
            )
          )
        )
      ),
      # ================= SAEE =================
      tabItem(
        tabName = "purchase",
        
        # CSS to style the Chi-Square text output
        tags$head(tags$style(HTML("
    #chi_square_result { 
      font-size: 18px; 
      font-weight: bold; 
      line-height: 1.5; 
      color: #2c3e50;
      background-color: #ffffff;
      padding: 15px;
    }
  "))),
        
        # Top Row: Chi-Square Result (Centered & Wide)
        fluidRow(
          column(
            width = 10, offset = 1,
            box(
              width = 12,
              title = "Chi-Square Test: Social Media vs Purchase Decision",
              status = "primary",
              solidHeader = TRUE,
              verbatimTextOutput("chi_square_result")
            )
          )
        ),
        
        # Bottom Row: Two Plots side-by-side
        fluidRow(
          column(
            width = 10, offset = 1,
            box(
              width = 6,
              title = "Discount vs Sales Amount",
              status = "primary",
              solidHeader = TRUE,
              plotOutput("discount_sales_plot", height = "400px")
            ),
            box(
              width = 6,
              title = "Sales Amount by Spending Groups (ANOVA)",
              status = "primary",
              solidHeader = TRUE,
              plotOutput("anova_boxplot", height = "400px")
            )
          )
        )
      ),
      
      # ================= AISHWARYA =================
      tabItem(
       
          tabName = "segment",
          
          fluidRow(
            column(
              width = 8, offset = 2,
              box(
                width = 12,
                bordered = TRUE,
                title = "Customer Spending Segmentation",
                status = "primary",
                solidHeader = TRUE,
                tableOutput("segmentation_table")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 8, offset = 2,
              box(
                width = 12,
                title = "Segmentation Distribution",
                status = "primary",
                solidHeader = TRUE,
                plotOutput("segmentation_plot", height = "350px")
              )
            )
          )
        
        
      ),
      
      tabItem(
        tabName = "summary_all",
        fluidRow(
          box(width = 12, div("Comprehensive Summary Analysis Overview", class = "heading"))
        ),
        fluidRow(
          box(width = 4, title = "Gender Distribution", status = "danger", solidHeader = TRUE, 
              plotOutput("sum_gender", height = "250px")),
          box(width = 4, title = "Spending Distribution", status = "danger", solidHeader = TRUE, 
              plotOutput("sum_spending", height = "250px")),
          box(width = 4, title = "Customer Segments", status = "danger", solidHeader = TRUE, 
              plotOutput("sum_segment", height = "250px"))
        ),
        fluidRow(
          box(width = 4, title = "Retention Probability", status = "danger", solidHeader = TRUE, 
              plotlyOutput("sum_retention", height = "250px")),
          box(width = 4, title = "Poisson Distribution", status = "danger", solidHeader = TRUE, 
              plotlyOutput("sum_poisson", height = "250px")),
          box(width = 4, title = "Sales Amount Analysis", status = "danger", solidHeader = TRUE, 
              plotOutput("sum_sales", height = "250px"))
        ),
        fluidRow(
          box(width = 6, title = "ANOVA: Spending Groups", status = "danger", solidHeader = TRUE, 
              plotOutput("sum_anova", height = "250px")),
          box(width = 6, title = "Decision Distribution", status = "danger", solidHeader = TRUE, 
              plotOutput("sum_decision", height = "250px"))
        )
      )
      ,
      # ================= SAKSHI =================
      # ================= SAKSHI =================
      tabItem(
        tabName = "decision",
        
        fluidRow(
          box(
            width = 6,
            title = "Decision Group Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("decision_bar", height = "350px")
          ),
          box(
            width = 6,
            title = "Research Time vs Decision Group",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("decision_box", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "T-Test Result",
            status = "warning",
            solidHeader = TRUE,
            tableOutput("ttest_result")
            
          )
        )
      )
      
    )
  )
)
tags$head(tags$style(HTML('
  .content-wrapper {
    background-color: #f0f0f0; /* Use your desired color or hex code */
  }
')))


# =========================
# SERVER
# =========================
server <- function(input, output) {
  
  # ---- VALUE BOXES ----
  
  output$box_avg_purchase <- renderValueBox({
    valueBox(
      round(mean(customers_data$Avg_Purchases, na.rm = TRUE), 2),
      "Avg Purchases",
      icon = icon("shopping-cart"),
      color = "purple"
    )
  })
  
  output$box_avg_bill <- renderValueBox({
    valueBox(
      paste0("₹", round(mean(customers_data$Sales_Amount, na.rm = TRUE), 0)),
      "Avg Bill Amount",
      icon = icon("rupee-sign"),
      color = "green"
    )
  })
  
  output$box_total_shoppers <- renderValueBox({
    valueBox(
      nrow(customers_data),
      "Total Customers",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$box_age_range <- renderValueBox({
    valueBox(
      paste(min(customers_data$Age), "-", max(customers_data$Age)),
      "Age Range",
      icon = icon("user-clock"),
      color = "orange"
    )
  })
  # =========================
  # Corrected Bar & Pie Charts
  # =========================
 
  output$gender_pie <- renderPlot({
    gender_count <- customers_data %>%
      # Convert FALSE back to "Female" and handle any other text issues
      mutate(Gender = as.character(Gender)) %>%
      mutate(Gender = case_when(
        Gender == "F" ~ "Female",
        Gender == "M" ~ "Male",
        TRUE ~ Gender
      )) %>%
      filter(!is.na(Gender)) %>%
      count(Gender)
    
    ggplot(gender_count, aes(x = "", y = n, fill = Gender)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = n), 
                position = position_stack(vjust = 0.5), 
                size = 6, fontface = "bold") +
      # Ensure every category in your data is mapped to a color
      scale_fill_manual(values = c("Male" = "#1E90FF", "Female" = "#EE6AA7", "Other" = "#D3D3D3")) +
      theme_void() +
      labs(title = "Gender Distribution") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  })
  
  # 2. Spending Bar Chart
  output$spending_bar <- renderPlot({
    spend_summary <- customers_data %>%
      count(Spending) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    
    ggplot(spend_summary, aes(x = Spending, y = n)) +
      geom_bar(stat = "identity", fill = "#5c1a1a") +
      geom_text(
        aes(label = paste0(percentage, "%")),
        vjust = -0.5,
        size = 5,
        fontface = "bold"
      ) +
      labs(
        title = "Yearly Online Shopping Expenditure",
        x = "Spending Range (₹)",
        y = "Number of Customers"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),   
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.line = element_line(linewidth = 1.2, colour = "black"), # Fixed size to linewidth
        axis.ticks = element_line(linewidth = 1.2)                  # Fixed size to linewidth
      )
  })
  
  output$orders_pie <- renderPlot({
    orders_summary <- customers_data %>%
      filter(!is.na(Orders_range)) %>%
      count(Orders_range) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    
    ggplot(orders_summary, aes(x = "", y = n, fill = as.factor(Orders_range))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      geom_text(
        aes(label = paste0(percentage, "%")),
        position = position_stack(vjust = 0.5),
        size = 5, fontface = "bold"
      ) +
      labs(title = "Annual Online Order Frequency", fill = "Orders per Year") +
      theme_void() +
      scale_fill_brewer(palette = "Reds") + # Gives nice shades of maroon/red
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  })
  
  
  # 4. Product Category Bar Chart
  output$category_bar <- renderPlot({
    category_summary <- customers_data %>%
      count(Category)
    
    ggplot(category_summary, aes(x = reorder(Category, -n), y = n)) +
      geom_bar(stat = "identity", fill = "#8b2e2e") +
      geom_text(aes(label = n), vjust = -0.5, size = 6, fontface = "bold") +
      labs(
        x = "Product Category",
        y = "Number of Customers"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(face = "bold", size = 11, hjust = 1), # Added angle for clarity
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),   
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.line = element_line(linewidth = 1.5, colour = "black"),
        axis.ticks = element_line(linewidth = 1.5)
      )
  })
  
  # ---- POOJA (Retention) SERVER ----
  
  output$plot_retention <- renderPlotly({
    plot_ly(
      customers_data,
      x = seq_len(nrow(customers_data)),
      y = ~Retention_Probability,
      type = "scatter",
      mode = "lines+markers",
      marker = list(color = "#5c1a1a"),
      line = list(color = "#5c1a1a", width = 3)
    ) %>%
      layout(
        xaxis = list(
          title = "Customer Index",
          titlefont = list(size = 18)
        ),
        yaxis = list(
          title = "Probability",
          titlefont = list(size = 18)
        )
      )
  })
  
 
  output$plot_poisson <- renderPlotly({
    plot_ly(
      data = poisson_df,
      x = ~x,
      y = ~Poisson_Probability,
      type = "bar",
      marker = list(color = "#8b2e2e")
    ) %>%
      layout(
        xaxis = list(
          title = list(text = "Number of Purchases (x)", font = list(size = 14, family = "Arial Black")),
          tickfont = list(size = 11, family = "Arial Black"),
          showline = TRUE,
          linewidth = 1.5,
          linecolor = "black"
        ),
        yaxis = list(
          title = list(text = "Poisson Probability", font = list(size = 14, family = "Arial Black")),
          tickfont = list(size = 12, family = "Arial Black"),
          showline = TRUE,
          linewidth = 1.5,
          linecolor = "black"
        ),
        bargap = 0.2,
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  
  
  # =========================
  # MANJIRI – SALES ANALYSIS
  # =========================
  
  price_range <- customers_data$Avg_Purchases
  sales_range <- customers_data$Sales_Amount
  
  # -------------------------
  # CORRELATION TEXT
  # -------------------------
  output$correlation_text <- renderText({
    
    r <- cor(price_range, sales_range, method = "pearson")
    
    interpretation <- if (r >= 0.7) {
      "Strong Positive Correlation"
    } else if (r >= 0.4) {
      "Moderate Positive Correlation"
    } else if (r >= 0.1) {
      "Weak Positive Correlation"
    } else if (r > -0.1) {
      "No or Very Weak Correlation"
    } else {
      "Negative Correlation"
    }
    
    paste0(
      "Karl Pearson’s Correlation (r): ", round(r, 3), "\n",
      "Interpretation: ", interpretation
    )
  })
  
  # -------------------------
  # REGRESSION TEXT
  # -------------------------
  output$regression_text <- renderText({
    
    model <- lm(Sales_Amount ~ Avg_Purchases, data = customers_data)
    
    paste0(
      "Monthly Sales = ",
      round(coef(model)[1], 2), " + ",
      round(coef(model)[2], 2), " × Price\n",
      "R-squared Value: ", round(summary(model)$r.squared, 3)
    )
  })
  
  # -------------------------
  # MODEL SUMMARY
  # -------------------------
  output$model_summary <- renderPrint({
    model <- lm(Sales_Amount ~ Avg_Purchases, data = customers_data)
    summary(model)
  })
  
  # -------------------------
  # PREDICTION TABLE
  # -------------------------
  output$prediction_table <- renderTable({
    
    model <- lm(Sales_Amount ~ Avg_Purchases, data = customers_data)
    
    new_data <- data.frame(
      Avg_Purchases = c(750, 1750, 3750)
    )
    
    prediction <- predict(model, newdata = new_data)
    
    data.frame(
      Price = new_data$Avg_Purchases,
      Predicted_Monthly_Sales = round(prediction, 0)
    )
    
  }, bordered = TRUE, striped = TRUE)
  
  # -------------------------
  # SALES PLOT
  # -------------------------
  output$sales_plot <- renderPlot({
    
    model <- lm(Sales_Amount ~ Avg_Purchases, data = customers_data)
    
    plot(
      price_range, sales_range,
      main = "Price vs Monthly Sales (Gen-Z Online Shopping)",
      sub = "Source: Primary Survey Data",
      xlab = "Average Price per Order (₹)",
      ylab = "Monthly Sales Amount (₹)",
      pch = 16,
      col = rgb(30, 144, 255, 150, maxColorValue = 255),
      cex = 1.3,
      font.lab = 2,
      las = 1,
      bty = "l"
    )
    
    grid(col = "lightgray", lty = "dotted")
    abline(model, col = "firebrick", lwd = 3)
    
    pred_prices <- data.frame(
      Avg_Purchases = c(750, 1750, 3750)
    )
    
    points(
      pred_prices$Avg_Purchases,
      predict(model, newdata = pred_prices),
      pch = 17,
      col = "darkgreen",
      cex = 1.5
    )
    
    legend(
      "topleft",
      legend = c("Actual Survey Data", "Regression Line", "Predicted Sales"),
      col = c("dodgerblue", "firebrick", "darkgreen"),
      pch = c(16, NA, 17),
      lwd = c(NA, 3, NA),
      bg = "white"
    )
  })
  
  # ---- JIGISHA (Consumer Behavior) ----
  

  output$purchase_stats <- renderText({
    x <- customers_data$Avg_Purchases
    paste0("--- Central Tendency Measures ---",
           "\nMean (Average):   ", round(mean(x, na.rm = TRUE), 2),
           "\nMedian (Middle):  ", median(x, na.rm = TRUE),
           "\nMode (Frequent):  ", names(sort(table(x), TRUE))[1])
  })
  
  output$purchase_hist <- renderPlot({
    # cex.lab = 1.5 (Axis Titles)
    # cex.axis = 1.3 (Axis Numbers)
    par(mar = c(5, 6, 4, 2), font.lab = 2, font.axis = 2, 
        cex.lab = 1.5, cex.axis = 1.3)
    
    hist(customers_data$Avg_Purchases,
         col = "lightpink", 
         breaks = 6,
         main = "", 
         xlab = "Average Number of Purchases",
         ylab = "Frequency",
         border = "white", 
         axes = FALSE)     
    
    # lwd = 3 makes the actual axis lines thicker
    axis(1, lwd = 3, col = "black")
    axis(2, lwd = 3, col = "black")
    box(lwd = 3, col = "black")
  })
  # ---- SAEE (Purchase Influence) SERVER ----
  
  output$discount_sales_plot <- renderPlot({
    # cex.lab=1.5 and cex.axis=1.3 for high visibility
    par(mar = c(5, 6, 4, 2), font.lab = 2, font.axis = 2, cex.lab = 1.5, cex.axis = 1.3)
    
    plot(customers_data$Discount, customers_data$Sales_Amount,
         pch = 19, col = "blue",
         xlab = "Discount (%)", ylab = "Sales Amount (₹)",
         axes = FALSE) 
    
    abline(lm(Sales_Amount ~ Discount, data = customers_data),
           col = "red", lwd = 4) # Thicker regression line
    
    # Thick dark axes and border
    axis(1, lwd = 3, col = "black")
    axis(2, lwd = 3, col = "black")
    box(lwd = 3, col = "black")
  })
  
  output$chi_square_result <- renderText({
    test <- chisq.test(
      table(customers_data$Social_Media,
            customers_data$Purchase_Decision),
      simulate.p.value = TRUE
    )
    paste0("Chi-square Statistic: ", round(test$statistic, 3),
           "\nP-value: ", round(test$p.value, 4),
           "\nInterpretation: ", ifelse(test$p.value < 0.05, 
                                        "Significant Relationship (Reject H0)", 
                                        "No Significant Relationship (Fail to Reject H0)"))
  })
  
  output$anova_boxplot <- renderPlot({
    par(mar = c(5, 6, 4, 2), font.lab = 2, font.axis = 2, cex.lab = 1.5, cex.axis = 1.3)
    
    boxplot(customers_data$Sales_Amount ~ cut(customers_data$Spending, 3),
            col = c("lightblue", "lightgreen", "lightpink"),
            xlab = "Spending Groups", ylab = "Sales Amount (₹)",
            axes = FALSE, lwd = 2) 
    
    axis(1, lwd = 3, col = "black")
    axis(2, lwd = 3, col = "black")
    box(lwd = 3, col = "black")
  })
  # ---- AISHWARYA ----
  output$segmentation_table <- renderTable({
    
    g <- cut(
      customers_data$Spending,
      breaks = c(
        -Inf,
        quantile(customers_data$Spending, 0.25, na.rm = TRUE),
        quantile(customers_data$Spending, 0.75, na.rm = TRUE),
        Inf
      ),
      labels = c("Low Spenders", "Medium Spenders", "High Spenders"),
      include.lowest = TRUE
    )
    
    Mean <- tapply(customers_data$Spending, g, mean)
    SD   <- tapply(customers_data$Spending, g, sd)
    CV   <- (SD / Mean) * 100
    
    data.frame(
      Segment = names(Mean),
      Mean_Spending = round(Mean, 2),
      SD = round(SD, 2),
      CV = round(CV, 2)
    )
  }, bordered = TRUE, striped = TRUE, hover = TRUE, spacing = "l")
  
  output$segmentation_plot <- renderPlot({
    
    g <- cut(
      customers_data$Spending,
      breaks = c(
        -Inf,
        quantile(customers_data$Spending, 0.25, na.rm = TRUE),
        quantile(customers_data$Spending, 0.75, na.rm = TRUE),
        Inf
      ),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
    
    seg_data <- as.data.frame(table(g))
    
    ggplot(seg_data, aes(x = g, y = Freq, fill = g)) +
      geom_bar(stat = "identity", width = 0.6) +
      labs(
        title = "Customer Segmentation by Spending",
        x = "Segment",
        y = "Number of Customers"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.line = element_line(size = 1.2, colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  
  
  
  # ---- SAKSHI (Decision Analysis) ----
  
  customers_data$Research_Time_num <- as.numeric(
    factor(customers_data$Research_Time,
           levels = c("Lower Price","Fast Delivery","Better Quality","Brand Value"),
           labels = c(1,2,3,4))
  )
  
  customers_data$Decision_Group <- ifelse(
    customers_data$Purchase_Decision == "Habit", "Habit", "Other"
  )
  
  output$decision_bar <- renderPlot({
    barplot(
      table(customers_data$Decision_Group),
      col = c("lightgreen","lightcoral"),
      main = "Decision Group Distribution",
      ylab = "Number of Customers"
    )
  })
  
  output$decision_box <- renderPlot({
    boxplot(
      Research_Time_num ~ Decision_Group,
      data = customers_data,
      col = c("lightblue","lightpink"),
      outline = FALSE,
      ylab = "Research Time Level",
      main = "Decision Group"
    )
    stripchart(
      Research_Time_num ~ Decision_Group,
      data = customers_data,
      vertical = TRUE,
      method = "jitter",
      pch = 19,
      add = TRUE
    )
    output$ttest_result <- renderTable({
      
      test <- t.test(
        Research_Time_num ~ Decision_Group,
        data = customers_data
      )
      
      data.frame(
        Measure = c("t-value","p-value",
                    "Mean Research Time (Habit)",
                    "Mean Research Time (Other)",
                    "Conclusion"),
        
        Result = c(
          round(test$statistic,3),
          round(test$p.value,4),
          round(mean(customers_data$Research_Time_num[
            customers_data$Decision_Group=="Habit"], na.rm=TRUE),2),
          round(mean(customers_data$Research_Time_num[
            customers_data$Decision_Group=="Other"], na.rm=TRUE),2),
          ifelse(test$p.value<0.05,
                 "Significant Difference",
                 "No Significant Difference")
        )
      )
      
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    
  })
  
  
  # ---- SUMMARY TAB OUTPUTS (CORRECTED) ----
  
  # Gender Pie
  output$sum_gender <- renderPlot({
    gender_count <- customers_data %>% count(Gender)
    ggplot(gender_count, aes(x = "", y = n, fill = Gender)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") + theme_void() + labs(title = "Gender")
  })
  
  # Spending Bar
  output$sum_spending <- renderPlot({
    spend_summary <- customers_data %>% count(Spending)
    ggplot(spend_summary, aes(x = Spending, y = n)) +
      geom_bar(stat = "identity", fill = "#5c1a1a") + theme_minimal()
  })
  
  # Segmentation
  output$sum_segment <- renderPlot({
    g <- cut(customers_data$Spending, breaks = 3, labels = c("Low", "Medium", "High"))
    seg_data <- as.data.frame(table(g))
    ggplot(seg_data, aes(x = g, y = Freq, fill = g)) + geom_bar(stat = "identity") + theme_minimal()
  })
  
  output$sum_retention <- renderPlotly({
    plot_ly(
      customers_data,
      x = seq_len(nrow(customers_data)),
      y = ~Retention_Probability,
      type = "scatter",
      mode = "lines+markers",
      marker = list(color = "#5c1a1a"),
      line = list(color = "#5c1a1a", width = 3)
    ) %>%
      layout(
        xaxis = list(
          title = "Customer Index",
          titlefont = list(size = 18)
        ),
        yaxis = list(
          title = "Probability",
          titlefont = list(size = 18)
        )
      )
  })
  
  
  output$sum_poisson <- renderPlotly({
    plot_ly(
      data = poisson_df,
      x = ~x,
      y = ~Poisson_Probability,
      type = "bar",
      marker = list(color = "#8b2e2e")
    ) %>%
      layout(
        xaxis = list(
          title = list(text = "Number of Purchases (x)", font = list(size = 14, family = "Arial Black")),
          tickfont = list(size = 11, family = "Arial Black"),
          showline = TRUE,
          linewidth = 1.5,
          linecolor = "black"
        ),
        yaxis = list(
          title = list(text = "Poisson Probability", font = list(size = 14, family = "Arial Black")),
          tickfont = list(size = 12, family = "Arial Black"),
          showline = TRUE,
          linewidth = 1.5,
          linecolor = "black"
        ),
        bargap = 0.2,
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  
  
  # Sales Amount
  output$sum_sales <- renderPlot({
    par(mar = c(4, 4, 2, 1))
    plot(customers_data$Avg_Purchases, customers_data$Sales_Amount, pch = 16, col = "dodgerblue")
    abline(lm(Sales_Amount ~ Avg_Purchases, data = customers_data), col = "red", lwd = 2)
  })
  
  # ANOVA
  output$sum_anova <- renderPlot({
    boxplot(customers_data$Sales_Amount ~ cut(customers_data$Spending, 3), 
            col = c("lightblue", "lightgreen", "lightpink"))
  })
  
  # Decision Bar
  output$sum_decision <- renderPlot({
    barplot(table(customers_data$Decision_Group), col = c("lightgreen","lightcoral"))
  })
}

# =========================
# Run App
# =========================
shinyApp(ui = ui, server = server)
