# TODO: rent and buy to a single tibble
# TODO: add "opportunity cost" parameter in home buying output table
# TODO: make line graph of total costs over time (real + opportunity) like NerdWallet
# TODO: in line graph, detect "point at which buying gains advantage"
require(pacman)
p_load(shiny, FinancialMath, ggplot2, reshape2, gridExtra, dplyr)

ui <- fluidPage(

    # Application title
    titlePanel("Rent vs. Buy Advanced Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Rent
            h3("Rent"),
            numericInput("monthly_rent", label = "Monthly Rent", 
                         value = 1300, min = 0),
            
            # Buy
            h3("Buy"),
            numericInput("initial_home_price", label = "Home Price", 
                         value = 250000, min = 0),
            sliderInput("interest_rate", label = "Interest Rate",
                        min = 0, max = 10, value = 3, step = 0.1),
            sliderInput("downpayment_percent", label = "Downpayment (%)",
                        min = 0, max = 30, value = 5, step = 1),
            selectInput("term", label = "Mortgage Term",
                        choices = c(15,30), selected = 30),
            
            # Forecast
            h3("Forecast"),
            sliderInput("forecast_length", label = "Number of Years to Forecast",
                        min = 1, max = 80, value = 20, step = 1),
            checkboxInput("use_historical_data", label = "Use Historical Stock Performance", value = TRUE),
            uiOutput("historical_data_extras_ui"),
            uiOutput("annualized_return_ui"),
            
            # Income
            h3("Income"),
            numericInput("starting_liquid_net_worth", label = "Liquid Net Worth (Start)", 
                         value = 50000, min = 0),
            numericInput("annual_income", label = "Annual Income (Start)", 
                         value = 55000, min = 0),
            numericInput("annual_other_expenses", label = "Annual Non-Housing Expenses (Start)", 
                         value = 20000, min = 0),
            
            # Advanced - Home
            h3("Advanced - Home"),
            sliderInput("closing", label = "Closing Costs (%)",
                        min = 0, max = 9, value = 5, step = 0.5),
            sliderInput("monthly_hoa_fees", label = "Monthly HOA Fees",
                        min = 0, max = 600, value = 200, step = 20),
            sliderInput("pmi", label = "Private Mortgage Insurance Rate (PMI, % home value)",
                        min = 0, max = 2, value = 0.8, step = 0.1),
            sliderInput("homeowners_insurance", label = "Homeowner's Insurance Rate (% home value)",
                        min = 0, max = 1, value = 0.45, step = 0.05),
            checkboxInput("repairs_as_percentage_of_home", label = "Use % of home value for repairs", value = FALSE),
            uiOutput("repairs_ui"),
            

            # Advanced - Annual Increases
            h3("Advanced - Annual Increases"),
            sliderInput("inflation", label = "Inflation (%)",
                        min = 0, max = 4, value = 1, step = 0.1),
            sliderInput("rent_appreciation", label = "Yearly Rent Increase (%)",
                        min = 0, max = 10, value = 2, step = 0.5),
            sliderInput("home_appreciation", label = "Yearly Home Value Appreciation (%)",
                        min = 0, max = 10, value = 3, step = 0.5),
            sliderInput("annual_income_increase", label = "Yearly Raise (%)",
                        min = 0, max = 10, value = 2, step = 0.5),
            sliderInput("lifestyle_inflation", label = "Lifestyle Inflation (%)",
                        min = 0, max = 10, value = 1, step = 0.5),
            
            # Advanced - Taxes
            h3("Advanced - Taxes"),
            sliderInput("property_tax_rate", label = "Yearly Property Tax (%)",
                        min = 0.8, max = 2, value = 1.3, step = 0.1),
            sliderInput("effective_tax_rate", label = "Effective Tax Rate (%)",
                        min = 0, max = 50, value = 21, step = 1),
            sliderInput("cap_gains", label = "Capital Gains Tax (%)",
                        min = 0, max = 25, value = 15, step = 1),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Net Worth over Time"),
            plotOutput("worthPlot"),
            h3("Expenses over Time"),
            plotOutput("expensePlot")
           
        )
    )
)

server <- function(input, output) {
    
    # Input Management
    output$repairs_ui <- renderUI(
        if(input$repairs_as_percentage_of_home) {
            sliderInput("repairs", label = "Yearly Repairs (% home value)", min = 0, max = 2, value = 0.35, step = 0.05)
        } else {
            numericInput("repairs", label = "Yearly Repairs ($)", 
                                  value = 1500, min = 0)
        })
    output$annualized_return_ui <- renderUI(
        if(!input$use_historical_data) {
            sliderInput("annualized_return", label = "Expected Investment Return (Annual)", min = 0, max = 20, value = 7, step = 0.25)
        })
    
    output$historical_data_extras_ui <- renderUI(
        if(input$use_historical_data) {
            tagList(
                checkboxInput("normalized_returns", label = "Normalize Stock Returns", value = TRUE),
                sliderInput("starting_historical_year", label = "Start Year (Stock Price)",
                            min = 1926, max = 2013, value = 1965, step = 1)
            )
        })
    
    

    # Calculator
    mydata <- reactive({
        req(!is.null(input$repairs))
        req(input$use_historical_data | !is.null(input$annualized_return))
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
            req(!is.null(input$normalized_returns))
            if(input$normalized_returns){
                returns_as_prod <- (returns$percent_change * .01) + 1
                total_change <- prod(returns_as_prod)
                annualized_return <- total_change^(1/forecast_length)
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
        
        inflation <- input$inflation
        rent_appreciation <- input$rent_appreciation
        home_appreciation <- input$home_appreciation
        annual_income_increase <- input$annual_income_increase
        lifestyle_inflation <- input$lifestyle_inflation

        property_tax_rate <- input$property_tax_rate
        effective_tax_rate <- input$effective_tax_rate
        cap_gains <- input$cap_gains
        
        # ---- Rent ----
        
        rent_results <- tibble(year = numeric(),
                               liquid_net_worth_rent = numeric(), 
                               monthly_rent = numeric(),
                               monthly_other_expenses = numeric())
        # rent_results <- matrix(NA, nrow=forecast_length, ncol=4)
        # colnames(rent_results) <- c("liquid_net_worth", "equity", "monthly_rent", "monthly_other_expenses")
        
        liquid_net_worth <- starting_liquid_net_worth 
        current_income <- annual_income
        current_expenses <- annual_other_expenses
        current_rent <- monthly_rent
        equity <- 0
        
        for(year in 1:forecast_length){
            liquid_net_worth <- liquid_net_worth * (1 + returns[returns$year == year,"percent_change"]*.01*(1-cap_gains/100))
            liquid_net_worth <- liquid_net_worth / (1 + inflation*.01)
            current_income <- current_income * (1+annual_income_increase*.01)
            current_expenses <- current_expenses * (1 + lifestyle_inflation/100)
            current_rent <- current_rent * (1 + rent_appreciation/100)
            extra_invested <- current_income*(1 - effective_tax_rate*.01) - current_expenses - current_rent*12
            liquid_net_worth <- liquid_net_worth + extra_invested
            if(length(liquid_net_worth)>0){
                rent_results <- rent_results %>% 
                    add_row(year = year, 
                            liquid_net_worth_rent = liquid_net_worth,
                            monthly_rent = current_rent,
                            monthly_other_expenses = current_expenses/12)
            }
        }
        
        # ---- Buy ----
        
        buy_results <- tibble(year = numeric(),
                              liquid_net_worth_home = numeric(), 
                              equity = numeric(), 
                              monthly_piti = numeric(), 
                              mortgage = numeric(), 
                              pmi = numeric(), 
                              property_tax = numeric(), 
                              homeowners_insurance = numeric(), 
                              hoa = numeric(), 
                              repair = numeric())
        # buy_results <- matrix(NA, nrow=forecast_length, ncol=4)
        # colnames(buy_results) <- c("liquid_net_worth", "equity", "monthly_payment", "monthly_other_expenses",
        #                            "mortgage", "pmi", "property_tax", "homeowners_insurance", "hoa", "repair")
        
        downpayment_amount <- initial_home_price * (downpayment_percent/100)
        closing_amount <- initial_home_price * (closing/100)
        monthly_interest_percent <- .01 * interest_rate/12
        principal <- initial_home_price - downpayment_amount
        payment_periods <- term * 12
        amort <- amort.table(Loan = principal,
                             n = payment_periods,
                             i = monthly_interest_percent)
        
        liquid_net_worth <- starting_liquid_net_worth - downpayment_amount - closing_amount
        current_income <- annual_income
        current_expenses <- annual_other_expenses
        equity <- downpayment_amount
        current_home_value <- initial_home_price
        
        for(year in 1:forecast_length){
            liquid_net_worth <- liquid_net_worth * (1 + returns[returns$year == year,"percent_change"]*.01*(1-cap_gains/100))
            liquid_net_worth <- liquid_net_worth / (1 + inflation*.01)
            current_income <- current_income * (1+annual_income_increase*.01)
            current_expenses <- current_expenses * (1 + lifestyle_inflation/100)
            current_home_value <-  current_home_value * (1 + home_appreciation/100)
            current_mortgage <- ifelse(year <= term, amort$Schedule[year*12,"Payment"], 0)
            balance_remaining <- ifelse(year <= term, amort$Schedule[year*12,"Balance"], 0)
            if(balance_remaining < 0.8 * initial_home_price){
                pmi <- 0
            }
            current_pmi <- (pmi*balance_remaining)/(12*100)
            current_property_tax <- (property_tax_rate*current_home_value)/(12*100)
            current_homeowners_insurance <-  (homeowners_insurance*current_home_value)/(12*100)
            current_piti <- current_mortgage + current_pmi + current_property_tax + current_homeowners_insurance + monthly_hoa_fees
            percentage_home_ownership <- (initial_home_price - balance_remaining) / initial_home_price
            current_equity <- current_home_value * percentage_home_ownership
            repair_cost <- if(input$repairs_as_percentage_of_home) { 
                # Wait until repairs updates to percentage value
                req(repairs <= 1)
                (repairs/100) * current_home_value
            } else {
                repairs * (1+inflation*.01)^year
            }
            extra_invested <- current_income*(1 - effective_tax_rate*.01) - current_expenses - current_piti*12 - repair_cost
            liquid_net_worth <- liquid_net_worth + extra_invested 
            if(length(liquid_net_worth)>0){
                buy_results <- buy_results %>% 
                    add_row(year = year, 
                            liquid_net_worth_home = liquid_net_worth,
                            equity = current_equity,
                            monthly_piti = current_piti,
                            mortgage = current_mortgage,
                            pmi = current_pmi,
                            property_tax = current_property_tax,
                            homeowners_insurance = current_homeowners_insurance,
                            hoa = monthly_hoa_fees,
                            repair = repair_cost/12)
                
            }
        }
        
        # combine into a single tibble
        results_table <- inner_join(rent_results, buy_results, by="year")
        save(results_table, file="results_table.Rdata") # for debugging
        
        return(results_table)
    })
    
    output$worthPlot <- renderPlot({
        results_table <- mydata()
        worth_plot_data <- results_table %>% 
            pivot_longer(cols = c(liquid_net_worth_home, 
                                  liquid_net_worth_rent, 
                                  equity),
                         names_to = "asset_type") %>%
            mutate(choice = ifelse(asset_type == "liquid_net_worth_rent",
                                   "Rent",
                                   "Buy")) %>%
            mutate(asset_type = factor(asset_type,
                                       levels = c("liquid_net_worth_home", "liquid_net_worth_rent", "equity"),
                                       labels = c("Liquid Net Worth","Liquid Net Worth", "Equity")))
        
        max_val <- results_table %>%
            select(starts_with("liquid_net_worth")) %>% 
            max * 1.05 # * b/c ggplot cuts off values >= max
        
        ggplot(worth_plot_data, aes(x=year, y=value/1000000, fill=asset_type)) +
            geom_area() +
            facet_wrap(~choice) + #r
            ylim(c(0,max_val/1000000)) +
            ylab("Net Worth (Millions)") +
            labs(fill="Asset Type")
        

    })
    
    output$expensePlot <- renderPlot({
        # return_vals <- mydata()
        # rent_results <- return_vals[[1]]
        # buy_results <- return_vals[[2]]
        # 
        # all_monthly_expenses <- c("monthly_rent",
        #                           "monthly_other_expenses",
        #                           "mortgage",
        #                           "pmi",
        #                           "property_tax",
        #                           "homeowners_insurance",
        #                           "hoa",
        #                           "repair")
        # monthly_expense_labels <- c("Rent",
        #                             "All Other Expenses",
        #                             "Mortgage",
        #                             "Private Mortgage Insurance (PMI)",
        #                             "Property Taxes",
        #                             "Homeowners Insurance",
        #                             "Homeowners Association (HOA) Fees",
        #                             "Home Repairs")
        # buy_expense_gg <- buy_results %>% 
        #     pivot_longer(cols = any_of(all_monthly_expenses),
        #                  names_to = "expense_type") %>%
        #     mutate(expense_type = factor(expense_type,
        #                                  levels = all_monthly_expenses,
        #                                  labels = monthly_expense_labels))
        # colnames(rent_expense_gg) <- c("year", "expense_type", "value")
        # rent_expense_gg$expense_type <- factor(rent_expense_gg$expense_type, levels=c("monthly_other_expenses", "monthly_rent"))
        # 
        # 
        # max_val_expense <- max(sum(rent_expense_gg[rent_expense_gg$year == max(rent_expense_gg$year), "value"], 1),
        #                        sum(buy_expense_gg[buy_expense_gg$year == max(buy_expense_gg$year), "value"], 1)) 
        # 
        # rent_expense <- ggplot(rent_expense_gg, aes(x=year, y=value, fill=expense_type)) +
        #     geom_area() +
        #     ylim(c(0,max_val_expense)) +
        #     ggtitle("Rent") +
        #     ylab("Monthly Expenses") + 
        #     scale_fill_hue(l=40, c=35) 
        # 
        # buy_expense <- ggplot(buy_expense_gg, aes(x=year, y=value, fill=expense_type)) +
        #     geom_area() +
        #     ylim(c(0,max_val_expense)) +
        #     ggtitle("Buy") +
        #     ylab("Monthly Expenses") + 
        #     scale_fill_hue(l=40, c=35) +
        #     theme(legend.title = element_blank())
        # 
        # grid.arrange(rent_expense, buy_expense, nrow=1)
    })
    
    
}

shinyApp(ui = ui, server = server)
