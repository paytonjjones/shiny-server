require(pacman)
require(shiny)
require(shinydashboard)
require(FinancialMath)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(dplyr)
require(RColorBrewer)
require(scales)
require(tidyr)


header <- dashboardHeader(
    title="Rent vs. Buy Advanced Calculator",
    titleWidth = "95%"
)

# ---- sidebar ----
sidebar <- dashboardSidebar(
    tags$head( 
        # To see the HTML classes, open the HTML via dev tools (command option i) on the shiny page
        tags$style(HTML(".sidebar-menu { font-size: 22px; }")), # default for whole sidebar
        tags$style(HTML(".sidebar-menu .form-group { font-size: 18px; }")), # labels for each input
        tags$style(HTML(".form-control { font-size: 16px; }")), # numericInput font size inside text field
        # Slider bar:
        tags$style(HTML(".irs-bar, .irs-line,.irs-slider {transform: scaleY(1)} 
                         .irs-single {font-size: 15px}
                         .irs-min, .irs-max, .irs-grid-text  {font-size: 13px}"))
        # note: the labels on the sliderInputs seem inaccessible
    ),
    width = "450px",
    # Sidebar with a slider input for number of bins 
        sidebarMenu(
            # Rent
            menuItem(text = "Rent vs. Buy",
                     numericInput("monthly_rent", label = "Monthly Rent", 
                                  value = 2300, min = 0),
                     numericInput("initial_home_price", label = "Home Price", 
                                  value = 440000, min = 0),
                     sliderInput("interest_rate", label = "Interest Rate",
                                 min = 0, max = 10, value = 3, step = 0.1),
                     sliderInput("downpayment_percent", label = "Downpayment (%)",
                                 min = 0, max = 100, value = 5, step = 1),
                     sliderInput("term", label = "Mortgage Term",
                                 min = 5, max = 40, value = 30, step = 1),
                     startExpanded = TRUE
                     ),
            
            # Forecast
            menuItem(text="Forecast",
                     sliderInput("forecast_length", label = "Number of Years to Forecast",
                                 min = 1, max = 80, value = 35, step = 1),
                     checkboxInput("use_historical_data", label = "Use Historical Stock Performance", value = FALSE),
                     uiOutput("historical_data_extras_ui"),
                     uiOutput("annualized_return_ui"),
                     startExpanded = FALSE
                     ),

            
            # Income
            menuItem(text = "Income",
                     numericInput("starting_liquid_net_worth", label = "Liquid Net Worth (Start)", 
                                  value = 150000, min = 0),
                     numericInput("annual_income", label = "Annual Income (Start)", 
                                  value = 125000, min = 0),
                     numericInput("annual_other_expenses", label = "Annual Non-Housing Expenses (Start)", 
                                  value = 20000, min = 0),
                     startExpanded = FALSE
                     ),
            
            
            # Advanced - Home
            menuItem(text = "Advanced",
                     menuItem(text = "Advanced - Home",
                                 sliderInput("closing", label = "Closing Costs (%)",
                                             min = 0, max = 9, value = 5, step = 0.5),
                                 sliderInput("monthly_hoa_fees", label = "Monthly HOA Fees",
                                             min = 0, max = 600, value = 200, step = 20),
                                 sliderInput("pmi", label = "Private Mortgage Insurance Rate (PMI, % home value)",
                                             min = 0, max = 2, value = 0.8, step = 0.1),
                                 sliderInput("homeowners_insurance", label = "Homeowner's Insurance Rate (% home value)",
                                             min = 0, max = 1, value = 0.45, step = 0.05),
                                 checkboxInput("repairs_as_percentage_of_home", label = "Use % of home value for repairs", value = TRUE),
                                 uiOutput("repairs_ui")
                                 ),
                     menuItem(text = "Advanced - Rent",
                              sliderInput("security_deposit", label = "Security Deposit (Months)",
                                          min = 0, max = 5, value = 1, step = 1)
                     ),
                     menuItem(text = "Advanced - Annual Increases",
                                 sliderInput("inflation", label = "Inflation (%)",
                                             min = 0, max = 4, value = 1.8, step = 0.1),
                                 sliderInput("rent_appreciation", label = "Yearly Rent Increase (%)",
                                             min = 0, max = 10, value = 2, step = 0.5),
                                 sliderInput("home_appreciation", label = "Yearly Home Value Appreciation (%)",
                                             min = 0, max = 10, value = 3, step = 0.5),
                                 sliderInput("annual_income_increase", label = "Yearly Raise (%)",
                                             min = 0, max = 10, value = 2, step = 0.5),
                                 sliderInput("lifestyle_inflation", label = "Lifestyle Inflation (%)",
                                             min = 0, max = 10, value = 1, step = 0.5)
                                 ),
                     menuItem(text = "Advanced - Taxes",
                                 sliderInput("property_tax_rate", label = "Yearly Property Tax (%)",
                                             min = 0, max = 4, value = 1.3, step = 0.1),
                                 sliderInput("cap_gains", label = "Capital Gains Tax (%)",
                                             min = 0, max = 25, value = 15, step = 1),
                                 sliderInput("state_tax_rate", label = "State Tax (%)",
                                             min = 0, max = 15, value = 5, step = 0.1),
                                 numericInput("retirement_deductions", label = "Deductible 401k/IRA Contributions", 
                                             value = 6000, min = 0),
                                 numericInput("other_itemized_deductions", label = "Possible Itemized Deductions", 
                                             value = 1000, min = 0),
                                 selectInput("filing_status", label = "Tax Filing Status",
                                             choices = c("Single", 
                                                         "Married Filing Jointly", 
                                                         "Married Filing Separately", 
                                                         "Head of Household"), 
                                             selected = "Married Filing Jointly")
                                 ),
                     menuItem(text = "Advanced - FIRE",
                              checkboxInput("fire", label = "Show Time to FIRE Plot", value = FALSE),
                              sliderInput("withdrawal_rate", label = "FIRE Withdrawal Rate",
                                          min = 0.5, max = 6, value = 3.2, step = 0.1)
                     ),
                     startExpanded = FALSE
                     )
        )
    )

# ---- body ----

body <- dashboardBody(
    conditionalPanel("input.fire", 
                     h3("Time to FIRE"),
                     plotOutput("firePlot", width = "100%", height = "500px")),
    h3("Monthly Housing Expenses"),
    plotOutput("expensePlot", width = "100%", height = "500px"),
    h3("Net Worth"),
    textOutput("net_worth_warnings"),
    plotOutput("worthPlot", width = "100%", height = "500px"),
    h3("Total Costs"),
    plotOutput("comparisonPlot", width = "100%", height = "500px"),

    # tags$div(
    #     tags$br(),
    #     tags$p("Did I save you time or money? Please donate $10 to keep this tool free."),
    #     HTML("<form action=\"https://www.paypal.com/donate\" method=\"post\" target=\"_top\">
    #              <input type=\"hidden\" name=\"business\" value=\"WG8BX27Z3QB92\" />
    #              <input type=\"hidden\" name=\"item_name\" value=\"Creating freely available data analysis tools\" />
    #              <input type=\"hidden\" name=\"currency_code\" value=\"USD\" />
    #              <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif\" 
    #              border=\"0\" name=\"submit\" title=\"PayPal - The safer, easier way to pay online!\" alt=\"Donate with PayPal button\" />
    #              <img alt=\"\" border=\"0\" src=\"https://www.paypal.com/en_US/i/scr/pixel.gif\" width=\"1\" height=\"1\" />
    #              </form>")
    # )
)

# ---- server ----
server <- function(input, output, session) {
    
    # ---- tag management ----
    # from https://stackoverflow.com/questions/32872222/how-do-you-pass-parameters-to-a-shiny-app-via-url
    observe({
        query <- parseQueryString(session$clientData$url_search)
        numeric_inputs <- c("monthly_rent", "initial_home_price", 
                            "starting_liquid_net_worth", "annual_income", 
                            "annual_other_expenses")
        checkbox_inputs <- c("use_historical_data", "repairs_as_percentage_of_home", "fire")
        select_inputs <- c("filing_status")
        for (i in 1:(length(reactiveValuesToList(input)))) {
            nameval = names(reactiveValuesToList(input)[i])
            if (!is.null(query[[nameval]])) {
                if(nameval %in% numeric_inputs){
                    updateNumericInput(session, nameval, value = as.numeric(query[[nameval]]))
                } else if(nameval %in% select_inputs){
                    updateSelectInput(session, nameval, selected = query[[nameval]])
                } else if(nameval %in% checkbox_inputs){
                    updateCheckboxInput(session, nameval, value = query[[nameval]])
                } else {
                    # As a default, assumes tags are slider inputs
                    updateSliderInput(session, nameval, value = as.numeric(query[[nameval]]))
                }
            }
            
        }
        
    })
    
    # ---- input management ----
    output$repairs_ui <- renderUI(
        if(input$repairs_as_percentage_of_home) {
            sliderInput("repairs", label = "Yearly Repairs (% home value)", min = 0, max = 2, value = 0.45, step = 0.05)
        } else {
            numericInput("repairs", label = "Yearly Repairs ($)", 
                                  value = 1500, min = 0)
        })
    output$annualized_return_ui <- renderUI(
        if(!input$use_historical_data) {
            sliderInput("annualized_return", label = "Expected Investment Return (Annual)", min = 0, max = 20, value = 10, step = 0.25)
        })
    
    output$historical_data_extras_ui <- renderUI(
        if(input$use_historical_data) {
            tagList(
                checkboxInput("normalized_returns", label = "Normalize Stock Returns", value = TRUE),
                sliderInput("starting_historical_year", label = "Start Year (Stock Price)",
                            min = 1926, max = 2013, value = 1960, step = 1)
            )
        })
    
    

    # ---- calculator ----
    mydata <- reactive({
        
        # ---- calculator initialization ----
        req(!is.null(input$repairs))
        req((input$use_historical_data && !is.null(input$normalized_returns)) |
                (!input$use_historical_data && !is.null(input$annualized_return))    
            )
        
        monthly_rent <- input$monthly_rent
        
        initial_home_price <- input$initial_home_price
        interest_rate <- input$interest_rate
        downpayment_percent <- input$downpayment_percent
        term <- as.numeric(input$term)
        
        forecast_length <- input$forecast_length
        starting_historical_year <- input$starting_historical_year
        annualized_return <- input$annualized_return
        if(input$use_historical_data){
            returns <- read.csv("history.csv")
            colnames(returns) <- c("year", "percent_change")
            returns <- returns[returns$year %in% starting_historical_year:(starting_historical_year+forecast_length), ]
            returns$year <- returns$year - starting_historical_year + 1
            if(input$normalized_returns){
                returns_as_prod <- (returns$percent_change * .01) + 1
                total_change <- prod(returns_as_prod)
                annualized_return <- total_change^(1/forecast_length)
                print(annualized_return)
                returns$percent_change <- (annualized_return-1) * 100
            }
        } else {
            returns <- data.frame("year"=1:(forecast_length+1),
                                  "percent_change"=annualized_return)
        }
        
        starting_liquid_net_worth <- input$starting_liquid_net_worth
        annual_income <- input$annual_income
        annual_other_expenses <- input$annual_other_expenses
        
        closing <- input$closing
        monthly_hoa_fees <- input$monthly_hoa_fees
        pmi <- input$pmi
        homeowners_insurance <- input$homeowners_insurance
        repairs <- input$repairs
        
        security_deposit <- input$security_deposit
        
        inflation <- input$inflation
        rent_appreciation <- input$rent_appreciation
        home_appreciation <- input$home_appreciation
        annual_income_increase <- input$annual_income_increase
        lifestyle_inflation <- input$lifestyle_inflation

        property_tax_rate <- input$property_tax_rate
        cap_gains <- input$cap_gains
        state_tax_rate <- input$state_tax_rate
        other_itemized_deductions <- input$other_itemized_deductions
        retirement_deductions <- input$retirement_deductions
        filing_status <- input$filing_status
        
        results_table <- tibble(year = numeric(),
                                liquid_net_worth_rent = numeric(),
                                monthly_rent = numeric(),
                                monthly_other_expenses = numeric(),
                                liquid_net_worth_buy = numeric(), 
                                equity = numeric(), 
                                monthly_piti = numeric(), 
                                mortgage = numeric(), 
                                pmi = numeric(), 
                                property_tax = numeric(), 
                                homeowners_insurance = numeric(), 
                                hoa = numeric(), 
                                repair = numeric(),
                                net_worth_free_housing = numeric())
        
        net_worth_free_housing <- starting_liquid_net_worth 
        
        # ---- Rent Setup ----
        
        current_income <- annual_income
        current_expenses <- annual_other_expenses
        current_rent <- monthly_rent
        equity <- 0
        liquid_net_worth_rent <- starting_liquid_net_worth - security_deposit*monthly_rent
        
        # ---- Buy Setup ----
        
        downpayment_amount <- initial_home_price * (downpayment_percent/100)
        closing_amount <- initial_home_price * (closing/100)
        monthly_interest_percent <- .01 * interest_rate/12
        principal <- max(initial_home_price - downpayment_amount, 0.01)
        payment_periods <- term * 12
        amort <- amort.table(Loan = principal,
                             n = payment_periods,
                             i = monthly_interest_percent)
        
        liquid_net_worth_buy <- starting_liquid_net_worth - downpayment_amount - closing_amount
        current_income <- annual_income
        current_expenses <- annual_other_expenses
        equity <- downpayment_amount
        current_home_value <- initial_home_price

        # ---- Year by Year Calculator ----
        
        for(year in 1:forecast_length){
            
            current_income <- current_income * (1 + annual_income_increase*.01)
            current_expenses <- current_expenses * (1 + lifestyle_inflation*.01)
            
            # Free Housing Calculations (to Compare)
            
            net_worth_free_housing <- net_worth_free_housing * (1 + returns[returns$year == year,"percent_change"]*.01*(1-cap_gains/100))
            net_worth_free_housing <- net_worth_free_housing / (1 + inflation*.01)
            taxable_income_free <- taxable_income(gross_income = current_income, 
                                                  retirement_deductions = retirement_deductions, 
                                                  itemized_deductions = other_itemized_deductions,
                                                  filing_status = filing_status)
            federal_tax_free <- federal_tax_calculator(taxable_income = taxable_income_free,
                                                       filing_status = filing_status)
            state_tax_free <- taxable_income_free * state_tax_rate * .01
            extra_invested_free_housing <- current_income - federal_tax_free - state_tax_free - current_expenses
            net_worth_free_housing <- net_worth_free_housing + extra_invested_free_housing
            
            # Rent Calculations
            
            liquid_net_worth_rent <- liquid_net_worth_rent * (1 + returns[returns$year == year,"percent_change"]*.01*(1-cap_gains/100))
            liquid_net_worth_rent <- liquid_net_worth_rent / (1 + inflation*.01)
            current_rent <- current_rent * (1 + rent_appreciation/100)
            extra_invested_rent <- current_income - federal_tax_free - state_tax_free - current_expenses - current_rent*12
            liquid_net_worth_rent <- liquid_net_worth_rent + extra_invested_rent
            
            # Buy Calculations
            
            liquid_net_worth_buy <- liquid_net_worth_buy * (1 + returns[returns$year == year,"percent_change"]*.01*(1-cap_gains/100))
            liquid_net_worth_buy <- liquid_net_worth_buy / (1 + inflation*.01)
            current_home_value <-  current_home_value * (1 + home_appreciation/100)
            current_mortgage <- ifelse(year <= term, amort$Schedule[year*12,"Payment"], 0)
            balance_remaining <- ifelse(year <= term, amort$Schedule[year*12,"Balance"], 0)
            if(balance_remaining < 0.8 * initial_home_price){
                pmi <- 0
            }
            current_pmi <- (.01*pmi*balance_remaining)/12
            current_property_tax <- (.01*property_tax_rate*current_home_value)/12
            current_homeowners_insurance <-  (.01*homeowners_insurance*current_home_value)/12
            current_piti <- current_mortgage + current_pmi + current_property_tax + current_homeowners_insurance + monthly_hoa_fees
            percentage_home_ownership <- (initial_home_price - balance_remaining) / initial_home_price
            current_equity <- current_home_value * percentage_home_ownership
            repair_cost <- if(input$repairs_as_percentage_of_home) { 
                req(repairs <= 1) # Upon change to repairs_as_percentage_of_home, wait until repairs value updates from dollar amount
                (repairs/100) * current_home_value
            } else {
                repairs * (1+inflation*.01)^year
            }
            mortgage_interest_deduction <- mortgage_deduction_calculator(amort_table = amort, 
                                                                         year = year,
                                                                         filing_status = filing_status)
            buy_itemized_deductions <- mortgage_interest_deduction + other_itemized_deductions
            taxable_income_buy <- taxable_income(gross_income = current_income, 
                                                  retirement_deductions = retirement_deductions, 
                                                  itemized_deductions = buy_itemized_deductions,
                                                  filing_status = filing_status)
            federal_tax_buy <- federal_tax_calculator(taxable_income = taxable_income_buy,
                                                  filing_status = filing_status)
            state_tax_buy <- taxable_income_buy * state_tax_rate * .01
            extra_invested_buy <- current_income - federal_tax_buy - state_tax_buy - current_expenses - current_piti*12 - repair_cost
            liquid_net_worth_buy <- liquid_net_worth_buy + extra_invested_buy

            
            if(length(liquid_net_worth_buy)>0){
                results_table <- results_table %>% 
                    add_row(year = year, 
                            liquid_net_worth_rent = liquid_net_worth_rent,
                            monthly_rent = current_rent,
                            monthly_other_expenses = current_expenses/12,
                            liquid_net_worth_buy = liquid_net_worth_buy,
                            equity = current_equity,
                            monthly_piti = current_piti,
                            mortgage = current_mortgage,
                            pmi = current_pmi,
                            property_tax = current_property_tax,
                            homeowners_insurance = current_homeowners_insurance,
                            hoa = monthly_hoa_fees,
                            repair = repair_cost/12,
                            net_worth_free_housing = net_worth_free_housing)
            }
        }
        
        # combine into a single tibble
        save(results_table, file="results_table.Rdata") # for debugging
        # load("advanced_rent_buy_calculator/results_table.Rdata") # to load
        
        return(results_table)
    })
    
    output$net_worth_warnings <- renderText({
        results_table <- mydata()
        warning <- ""
        if(TRUE %in% (0 > results_table$liquid_net_worth_buy) |
           TRUE %in% (0 > results_table$liquid_net_worth_rent)) {
            bankruptcy_year <- min(which(0 > results_table$liquid_net_worth_buy),
                                   which(0 > results_table$liquid_net_worth_rent))
            warning <- paste(c(warning, 
                               "Warning: You ran out of money in year #",
                               bankruptcy_year,
                               ". Increase Liquid Net Worth or Yearly Income"),
                             collapse= "")
        }
    })
    
    # ---- comparison plot ----
    output$comparisonPlot <- renderPlot({
        results_table <- mydata()
        comparison_plot_data <- results_table %>%
            mutate(buy_cost = net_worth_free_housing - liquid_net_worth_buy - equity,
                   rent_cost = net_worth_free_housing - liquid_net_worth_rent) %>%
            pivot_longer(cols = c(buy_cost, rent_cost),
                         names_to = "choice") %>%
            mutate(choice = factor(choice, levels = c("buy_cost", "rent_cost"), labels = c("Buy", "Rent")))
        
        comparison_plot_data %>% select(c(choice, value))
        
        compare_boolean <- (comparison_plot_data %>% 
            filter(choice == "Buy") %>% 
            select(value)) > 
                (comparison_plot_data %>% 
                    filter(choice == "Rent") %>% 
                    select(value))
        
        # At start_point: TRUE means buying is more expensive, FALSE means renting is more expensive 
        #                 (almost always TRUE for first year)
        switch_points <- comparison_switch_points(compare_boolean)
        
        if(is.null(switch_points$change_indices)){
            comp_title <- ifelse(switch_points$start_point,
                                "Renting is always better than buying", 
                                "Buying is always better than renting")
        } else {
            comp_title <- ifelse(switch_points$start_point,
                            "Buying becomes advantageous after ", 
                            "Renting becomes advantageous after ")
            comp_title <- paste(c(comp_title, switch_points$change_indices[1], " years"), 
                                collapse="")
            if(length(switch_points$change_indices)>1){
                for(i in 2:length(switch_points$change_indices)){
                    but_then <- ifelse(i > 2,
                                       ", and then ",
                                       ", but then ")
                    comp_title <- paste(c(comp_title, 
                                          but_then,
                                          c("renting", "buying")[switch_points$start_point + (i %% 2)],
                                          " becomes advantageous after ",
                                          switch_points$change_indices[i],
                                          " years"), 
                                        collapse="")
                }
            }
        }
        
        intersection <- switch_points$change_indices[1]
        
        title_wrapper <- function(x, width) {
            paste(strwrap(x, width=width), collapse = "\n")
        }
        
        comp_plot <- ggplot(comparison_plot_data, aes(x=year, y=value/1000000, col=choice)) +
            geom_line() + 
            ggtitle(title_wrapper(comp_title, width = 80)) +
            theme(legend.title = element_blank(),
                  text = element_text(size = 16)) +
            ylab("Expenses + Opportunity Cost (Millions)") +
            xlab("Year") + 
            theme(legend.position="bottom") +
            scale_color_brewer(palette = 'Dark2') 
        
        # Add lines demarcating when renting/buying becomes advantageous
        for(i in switch_points$change_indices){
            comp_plot <- comp_plot +
                geom_vline(xintercept=switch_points$change_indices, linetype="dashed", color="grey")
        }
        
        comp_plot
    })
    
    # ---- worth plot ----
    output$worthPlot <- renderPlot({
        results_table <- mydata()
        worth_plot_data <- results_table %>% 
            pivot_longer(cols = c(liquid_net_worth_buy, 
                                  liquid_net_worth_rent, 
                                  equity),
                         names_to = "asset_type") %>%
            mutate(choice = ifelse(asset_type == "liquid_net_worth_rent",
                                   "Rent",
                                   "Buy")) %>%
            mutate(asset_type = factor(asset_type,
                                       levels = c("liquid_net_worth_buy", "liquid_net_worth_rent", "equity"),
                                       labels = c("Liquid Net Worth","Liquid Net Worth", "Equity")))
        
        
        ggplot(worth_plot_data, aes(x=year, y=value/1000000, fill=asset_type)) +
            geom_area() +
            facet_wrap(~choice) + 
            ylab("Net Worth (Millions)") +
            xlab("Year") +
            labs(fill="Asset Type") + 
            theme(legend.position="bottom",
                  text = element_text(size = 16))
    })
    
    # ---- expense plot ----
    output$expensePlot <- renderPlot({
        results_table <- mydata()
        all_monthly_expenses <- c("mortgage",
                                  "monthly_rent",
                                  "pmi",
                                  "property_tax",
                                  "homeowners_insurance",
                                  "hoa",
                                  "repair")
        monthly_expense_labels <- c("Mortgage / Rent",
                                    "Mortgage / Rent",
                                    "PMI",
                                    "Property Taxes",
                                    "Homeowners Insurance",
                                    "HOA Fees",
                                    "Home Repairs")
        expense_plot_data <- results_table %>%
            pivot_longer(cols = any_of(all_monthly_expenses),
                         names_to = "expense_type") %>%
            mutate(choice = ifelse(expense_type %in% c("monthly_rent"),
                                   "Rent",
                                   "Buy")) %>%
            mutate(expense_type = factor(expense_type,
                                         levels = all_monthly_expenses,
                                         labels = monthly_expense_labels)) 
        
        # Duplicate rows for "All Other Expenses"
        expense_plot_data <- rbind(expense_plot_data,
                                   expense_plot_data %>% 
                                       filter(expense_type == "All Other Expenses") %>% 
                                       mutate(choice = "Buy"))
        
        ggplot(expense_plot_data, aes(x=year, y=value, fill=expense_type)) +
            geom_area() +
            facet_wrap(~choice) +
            ylab("Monthly Expenses") +
            xlab("Year") +
            scale_fill_brewer(palette = 'Spectral') +
            theme(legend.title = element_blank(),
                  text = element_text(size = 16),
                  legend.position="bottom") 
    })
    
    # ---- fire plot ----

    output$firePlot <- renderPlot({
        results_table <- mydata()
        input_required_withdrawal_rate <- input$withdrawal_rate
        fire_plot_data <- results_table %>%
            mutate(total_expenses_rent = 12*(monthly_rent + monthly_other_expenses),
                   total_expenses_buy = 12*(mortgage + pmi + property_tax + homeowners_insurance + hoa + repair + monthly_other_expenses),
                   investments_rent = liquid_net_worth_rent,
                   investments_buy = liquid_net_worth_buy,
                   required_withdrawal_rate_rent = 100 * total_expenses_rent / investments_rent,
                   required_withdrawal_rate_buy = 100 * total_expenses_buy / investments_buy) %>%
            pivot_longer(cols = c(required_withdrawal_rate_rent, required_withdrawal_rate_buy),
                   names_to = "choice",
                   values_to = "required_withdrawal_rate") %>%
            mutate(choice = ifelse(choice == "required_withdrawal_rate_rent",
                                   "Rent",
                                   "Buy")) 
        
        rent_to_fire_years <- fire_plot_data %>%
            filter(required_withdrawal_rate <= input_required_withdrawal_rate &
                       choice == "Rent") %>%
            filter(year == min(year)) %>%
            select(year)
        buy_to_fire_years <- fire_plot_data %>%
            filter(required_withdrawal_rate <= input_required_withdrawal_rate &
                       choice == "Buy") %>%
            filter(year == min(year)) %>%
            select(year)
        if(nrow(buy_to_fire_years)==0){
            buy_to_fire_years <- buy_to_fire_years %>%
                add_row(year = Inf)
        }  
        if(nrow(rent_to_fire_years)==0){
            rent_to_fire_years <- rent_to_fire_years %>%
                add_row(year = Inf)
        } 
        if(buy_to_fire_years == Inf && rent_to_fire_years == Inf){
            fire_title <- "You cannot fire within the timeline"
        } else {
            fire_title <- paste(ifelse(rent_to_fire_years <= buy_to_fire_years,
                                       "Rent",
                                       "Buy"),
                                " to FIRE in ",
                                min(rent_to_fire_years, buy_to_fire_years),
                                " years",
                                sep="")
        }

        x_max <- min(max(as.numeric(rent_to_fire_years), as.numeric(buy_to_fire_years)),
                     input$forecast_length)
        ggplot_fire <- 
            ggplot(fire_plot_data, aes(x=year, y=required_withdrawal_rate*.01, col=choice)) +
            geom_line() + 
            ggtitle(fire_title) +
            theme(legend.title = element_blank(),
                  text = element_text(size = 16)) +
            ylab("Expenses / Investments (%)") +
            scale_y_continuous(labels = percent) +
            xlab("Year") + 
            theme(legend.position="bottom") +
            scale_color_brewer(palette = 'Dark2') +
            geom_hline(yintercept=input$withdrawal_rate*.01, linetype="dashed", color="grey") +
            xlim(1, x_max) +
            annotate(geom="text", x=1.5, y=input$withdrawal_rate*1.3*.01, 
                     label=paste(round(input$withdrawal_rate, 2), "%", sep=""), 
                     color="black")
        
        if(as.numeric(buy_to_fire_years) < input$forecast_length) {
            ggplot_fire <- ggplot_fire + 
                geom_vline(xintercept=as.numeric(buy_to_fire_years)-0.5, linetype="dashed", color=brewer.pal(3, "Dark2")[1]) 
        }
        if(as.numeric(rent_to_fire_years) < input$forecast_length) {
            ggplot_fire <- ggplot_fire +
                geom_vline(xintercept=as.numeric(rent_to_fire_years)-0.5, linetype="dashed", color=brewer.pal(3, "Dark2")[2]) 
        }
        

        return(ggplot_fire)
            
    })
    
    # Ensure these load on startup
    outputOptions(output, "historical_data_extras_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "annualized_return_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "repairs_ui", suspendWhenHidden = FALSE)
    
    
}


# ---- helpers ----

read_tax_brackets <- function(){
    tax_brackets <- list()
    tax_brackets$`Single` <- read.csv('tax_brackets/single_filers.csv')
    tax_brackets$`Married Filing Jointly` <- read.csv('tax_brackets/married_filing_jointly.csv')
    tax_brackets$`Married Filing Separately` <- read.csv('tax_brackets/married_filing_separately.csv')
    tax_brackets$`Head of Household` <- read.csv('tax_brackets/head_of_household.csv')
    return(tax_brackets)
}

taxable_income <- function(gross_income,
                           retirement_deductions,
                           itemized_deductions,
                           filing_status){
    tax_brackets <- read_tax_brackets()
    bracket_info <- tax_brackets[[filing_status]]
    best_deduction <- max(c(itemized_deductions, 
                            bracket_info$standard_deduction))
    taxable_income <- gross_income - retirement_deductions - best_deduction
}

federal_tax_calculator <- function(taxable_income,
                                   filing_status){
    tax_brackets <- read_tax_brackets()
    bracket_info <- tax_brackets[[filing_status]]
    tax <- 0
    for(i in 1:nrow(bracket_info)){
        if(taxable_income > bracket_info[i, "bracket_low"]){
            if(taxable_income > bracket_info[i, "bracket_high"]){
                tax <- tax + (bracket_info[i, "bracket_high"] - bracket_info[i, "bracket_low"]) * bracket_info[i, "rate"]
            } else {
                tax <- tax + (taxable_income - bracket_info[i, "bracket_low"]) * bracket_info[i, "rate"]
            }
        }
    }
    return(tax)
}

mortgage_deduction_calculator <- function(amort_table, year, filing_status){
    amort_table <- rbind(amort_table$Schedule,
                         matrix(0, nrow = 99999, ncol=ncol(amort_table$Schedule)))
    total_mortgage_interest_paid <- amort_table[((year-1)*12 + 1):(year*12), "Interest Paid"] %>% sum
    principal <- amort_table[((year-1)*12 + 1):(year*12), "Balance"] %>% mean
    deductible_principal <- switch(filing_status,
                                   "Single" = 750000,
                                   "Married Filing Jointly" = 750000,
                                   "Married Filing Separately" = 375000,
                                   "Head of Household" = 750000)
    mortgage_deduction <- min(total_mortgage_interest_paid,
                              total_mortgage_interest_paid * (deductible_principal / principal),
                              na.rm=T)
    return(mortgage_deduction)
}

comparison_switch_points <- function(boolean_vector){
    ref_bool <- start_bool <- boolean_vector[1]
    change_indices <- c()
    for(i in 1:length(boolean_vector)){
        bool <- boolean_vector[i]
        if(bool != ref_bool){
            ref_bool <- bool
            change_indices <- c(change_indices, i)
        }
    }
    return(list("start_point" = start_bool,
                "change_indices" = change_indices))
}

# ---- call ----
shinyApp(ui = dashboardPage(header, sidebar, body), 
         server = server)
