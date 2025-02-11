library(shiny)
library(shinyauthr)
library(sodium)
library(tibble)
library(bslib)
library(shinyWidgets)
library(DT)
library(quantmod)      # For getQuote()
library(tidyquant)     # For fetching historical stock prices in tidy format

# Create the user database with proper password hashing
user_base <- tibble::tibble(
  user = "user",
  password = "password",
  permissions = "admin",
  name = "Admin User"
)

ui <- fluidPage(
  # Default Shinyauthr login ui
  shinyauthr::loginUI("login"),
  uiOutput("body")
)

server <- function(input, output, session) {
  # Reactive value to control logout functionality
  logout_init <- reactiveVal(FALSE)
  
  # Call the loginServer module to authenticate users.
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    log_out = logout_init  # This triggers logout when set to TRUE.
  )
  
  # Store the investment data in a reactiveVal so we can update it
  investments_rv <- reactiveVal(
    tibble(
      Stock = c("AAPL", "GOOGL", "AMZN", "TSLA"),
      # Initialize with some placeholder prices
      Price = c(150, 2000, 3000, 700),
      Quantity = c(1, 1, 1, 1)
    )
  )
  
  # Fetch real-time-ish (delayed) quotes for current prices
  fetch_latest_prices <- function(symbols) {
    df <- quantmod::getQuote(symbols, src = "yahoo")
    setNames(df$Last, rownames(df))  # Named vector: c(AAPL=..., GOOGL=..., etc.)
  }
  
  # Update the "Price" column with fresh quotes
  update_investment_prices <- function() {
    cur_data <- investments_rv()
    new_prices <- fetch_latest_prices(cur_data$Stock)
    
    for (i in seq_len(nrow(cur_data))) {
      symbol <- cur_data$Stock[i]
      if (!is.na(new_prices[symbol])) {
        cur_data$Price[i] <- new_prices[symbol]
      }
    }
    investments_rv(cur_data)
  }
  
  # Reactive timer for updating prices every minute
  autoInvalidate <- reactiveTimer(60000)
  observe({
    autoInvalidate()
    update_investment_prices()
    message("Prices updated (timer).")
  })
  
  # Update prices when the user switches tabs
  observeEvent(input$main_navbar, {
    update_investment_prices()
    message("Prices updated (tab switch).")
  })
  
  # Render the data table of investments
  output$investments_table <- DT::renderDataTable({
    DT::datatable(
      investments_rv(),
      options = list(pageLength = 5)
    )
  })
  
  # Calculate and display the total balance
  output$total_balance <- renderText({
    data <- investments_rv()
    total_val <- sum(data$Price * data$Quantity)
    paste("Total Balance:", scales::dollar(total_val))
  })
  
  # -- Timeframe-based historical data fetching --
  
  # A small helper mapping user timeframe choice to "days ago"
  timeframe_days <- function(choice) {
    switch(choice,
           "1 Month" = 30,
           "3 Months" = 90,
           "6 Months" = 180,
           "1 Year" = 365,
           365  # default
    )
  }
  
  # This reactive fetches daily historical prices for stocks (Quantity > 0)
  # based on the timeframe the user selected.
  historical_data <- reactive({
    data <- investments_rv()
    owned_stocks <- data$Stock[data$Quantity > 0]
    
    if (length(owned_stocks) == 0) {
      return(NULL)
    }
    
    # Timeframe: from (X days ago) to today
    req(input$timeframe)  # Make sure the input exists
    num_days <- timeframe_days(input$timeframe)
    
    from_date <- Sys.Date() - num_days
    to_date <- Sys.Date()
    
    # Fetch daily price data
    stock_data <- tq_get(owned_stocks, 
                         get  = "stock.prices",
                         from = from_date, 
                         to   = to_date)
    stock_data
  })
  
  # Render the performance chart
  output$investments_chart <- renderPlot({
    df <- historical_data()
    
    # If no holdings or no data, display a simple message
    if (is.null(df) || nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No current holdings to plot.", cex = 1.2)
      return()
    }
    
    library(ggplot2)
    
    ggplot(df, aes(x = date, y = adjusted, color = symbol)) +
      geom_line(size = 1) +
      labs(
        title = paste("Stock Performance -", input$timeframe),
        x = "Date",
        y = "Adjusted Price (USD)",
        color = "Symbol"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Observe event for 'Buy' button
  observeEvent(input$buy_button, {
    # Determine the selected stock (either from dropdown or text input)
    selected_stock <- if (input$buy_stock != "") input$buy_stock else input$stock_search
    
    # Validate the stock symbol
    if (!selected_stock %in% investments_rv()$Stock) {
      showNotification("Invalid stock symbol. Please enter one of: AAPL, GOOGL, AMZN, TSLA.", type = "error")
      return()
    }
    
    # Show confirmation modal
    showModal(
      modalDialog(
        title = "Confirm Purchase",
        p(paste("You are about to buy", input$buy_quantity, "shares of", selected_stock, ".", " This action cannot be undone.")),
        footer = tagList(
          actionButton("confirm_buy", "Proceed"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  # Handle confirmed buy
  observeEvent(input$confirm_buy, {
    removeModal()
    cur_data <- investments_rv()
    selected_stock <- if (input$buy_stock != "") input$buy_stock else input$stock_search
    idx <- which(cur_data$Stock == selected_stock)
    cur_data$Quantity[idx] <- cur_data$Quantity[idx] + input$buy_quantity
    investments_rv(cur_data)
    showNotification(paste("Bought", input$buy_quantity, selected_stock), type = "message")
  })
  
  # Observe event for 'Sell' button
  observeEvent(input$sell_button, {
    # Determine the selected stock (either from dropdown or text input)
    selected_stock <- if (input$sell_stock != "") input$sell_stock else input$sell_search
    
    # Validate the stock symbol
    if (!selected_stock %in% investments_rv()$Stock) {
      showNotification("Invalid stock symbol. Please enter one of: AAPL, GOOGL, AMZN, TSLA.", type = "error")
      return()
    }
    
    # Show confirmation modal
    showModal(
      modalDialog(
        title = "Confirm Sale",
        p(paste("You are about to sell", input$sell_quantity, "shares of", selected_stock, ".")),
        footer = tagList(
          actionButton("confirm_sell", "Proceed"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  # Handle confirmed sell
  observeEvent(input$confirm_sell, {
    removeModal()
    cur_data <- investments_rv()
    selected_stock <- if (input$sell_stock != "") input$sell_stock else input$sell_search
    idx <- which(cur_data$Stock == selected_stock)
    if (cur_data$Quantity[idx] < input$sell_quantity) {
      showNotification("You do not have enough shares to sell.", type = "error")
    } else {
      cur_data$Quantity[idx] <- cur_data$Quantity[idx] - input$sell_quantity
      investments_rv(cur_data)
      showNotification(paste("Sold", input$sell_quantity, selected_stock), type = "message")
    }
  })
  
  # Handle cancellation of buy/sell
  observeEvent(input$cancel_transaction, {
    showModal(
      modalDialog(
        title = "Cancel Transaction",
        p("Are you sure you want to cancel? You will lose your progress."),
        footer = tagList(
          actionButton("confirm_cancel", "Yes, Cancel"),
          modalButton("No, Go Back")
        )
      )
    )
  })
  
  # Handle confirmed cancellation
  observeEvent(input$confirm_cancel, {
    removeModal()
    showNotification("Transaction canceled.", type = "warning")
  })
  
  output$body <- renderUI({
    # Suspend rendering until the user is authenticated.
    req(credentials()$user_auth)
    
    # Build the main UI once logged in.
    page_navbar(
      # Assign an id so we can observe changes in input$main_navbar
      id = "main_navbar",
      
      title = div("Stock Buy/Sell", style = "font-size: 20px; font-weight: bold;"),
      
      theme = bs_theme(
        version = version_default(),
        bootswatch = "cerulean"
      ),
      
      # Home panel
      nav_panel(
        title = "Home",
        card(
          card_title("Welcome to my website!"),
          p("This is the home page. As of now, this website lets you view your balance, buy and sell stocks, 
            and see graphs showing your total performance and individual stocks performance, putting all your investment needs into one place.")
        )
      ),
      
      # View Investments panel
      nav_panel(
        title = "View Investments",
        card(
          card_title("Your Investments"),
          DT::dataTableOutput("investments_table")
        ),
        card(
          card_title("Total Balance"),
          textOutput("total_balance")
        ),
        card(
          card_title("Adjust Timeframe"),
          # The user can pick 1 Month, 3 Months, 6 Months, or 1 Year
          selectInput(
            inputId = "timeframe",
            label = "Select timeframe:",
            choices = c("1 Month", "3 Months", "6 Months", "1 Year"),
            selected = "1 Year"
          )
        ),
        card(
          card_title("Performance Chart"),
          plotOutput("investments_chart", height = "400px")
        )
      ),
      
      # Buy/Sell Stocks panel
      nav_panel(
        title = "Buy/Sell Stocks",
        layout_column_wrap(
          card(
            card_title("Buy Stocks"),
            selectInput("buy_stock", "Select a stock to buy:", 
                        choices = c("", "AAPL", "GOOGL", "AMZN", "TSLA")),
            textInput("stock_search", "Or enter stock symbol manually:"),
            numericInput("buy_quantity", "Quantity to buy:", value = 1, min = 1, step = 1),
            actionButton("buy_button", "Buy"),
            p("Note: Prices are updated every 10 seconds and may not be exact.")
          ),
          card(
            card_title("Sell Stocks"),
            selectInput("sell_stock", "Select a stock to sell:",
                        choices = c("", "AAPL", "GOOGL", "AMZN", "TSLA")),
            textInput("sell_search", "Or enter stock symbol manually:"),
            numericInput("sell_quantity", "Quantity to sell:", value = 1, min = 1, step = 1),
            actionButton("sell_button", "Sell"),
            p("Note: Prices are updated every 10 seconds and may not be exact.")
          )
        )
      ),
      
      # Personalized Recommendations panel
      nav_panel(
        title = "Personalized Recommendations",
        p("Personalized Recommendations content goes here.")
      ),
      
      # Insert a spacer that pushes the next nav_item to the far right of the navbar
      nav_spacer(),
      
      # Create a nav_item for the logout button on the far right
      nav_item(
        actionButton("logout_button", "Logout", class = "btn btn-danger btn-sm")
      )
    )
  })
  
  # Observe the logout button and trigger logout when clicked.
  observeEvent(input$logout_button, {
    logout_init(TRUE)
  })
}

shinyApp(ui, server)