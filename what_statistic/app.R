require(shinydashboard)

ui <- fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
            tags$title("What statistic?")),
  dashboardPage(
    dashboardHeader(title = "What statistic should I use?", titleWidth = "95%"),
    dashboardSidebar(
      selectInput(inputId = "IV", label = "Independent Variable(s)", 
                  choices = c("",
                              "0 IVs",
                              "1 interval IV",
                              "1 IV with 2 levels (independent groups)",
                              "1 IV with 3+ levels (independent groups)",
                              "1 IV with 2 levels (dependent/matched groups)",
                              "1 IV with 3+ levels (dependent/matched groups)",
                              "2+ IVs (interval or mixed types)",
                              "2+ IVs (all categorical with independent groups)"), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      selectInput(inputId = "DV", label = "Dependent Variable(s)", 
                  choices = c("",
                              "1 interval DV",
                              "1 ordinal DV",
                              "1 categorical DV (2 categories)",
                              "1 categorical DV (3+ categories)",
                              "2+ interval DVs"), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      checkboxInput(inputId = "normal", label = "Data meets normality assumptions", value = TRUE)
    ),
    dashboardBody(
      column(4,
             uiOutput("onesamplet"),
             uiOutput("onesamplem"),
             uiOutput("binom"),
             uiOutput("chisqgof"),
             uiOutput("indept"),
             uiOutput("wmw"),
             uiOutput("chisq"),
             uiOutput("fisher"),
             uiOutput("anova"),
             uiOutput("kw"),
             uiOutput("pairedt"),
             uiOutput("wsrt"),
             uiOutput("mcnemar"),
             uiOutput("rmanova"),
             uiOutput("friedman"),
             uiOutput("rmlog"),
             uiOutput("facanova"),
             uiOutput("ordlog"),
             uiOutput("faclog"),
             uiOutput("corr"),
             uiOutput("reg"),
             uiOutput("npc"),
             uiOutput("log"),
             uiOutput("multreg"),
             uiOutput("ancova"),
             uiOutput("multlog"),
             uiOutput("disc"),
             uiOutput("manova"),
             uiOutput("manova2"),
             uiOutput("mmreg"),
             uiOutput("factora")
      ),
      column(6,
             htmlOutput("style"),
             htmlOutput("Wikipedia")
      )
    ))
)

server <- function(input, output) {
  wiki <- readRDS("wiki.RDS")
  
  output$onesamplet <- renderUI({
    if((input$IV == "0 IVs" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("onesamplet_link", "One-sample t test") }
  })
  output$onesamplem <- renderUI({
    if((input$IV == "0 IVs" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" & input$normal == FALSE | 
        input$DV == "1 ordinal DV" | 
        input$DV == ""))  
    { actionLink("onesamplem_link", "One-sample median")}
  })
  output$binom <- renderUI({
    if((input$IV == "0 IVs" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == ""))  
    { actionLink("binom_link","Binomial test") }
  })
  output$chisqgof <- renderUI({
    if((input$IV == "0 IVs" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("chisqgof_link", "Chi-square goodness-of-fit test") }
  })
  output$indept <- renderUI({
    if((input$IV == "1 IV with 2 levels (independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" |
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("indept_link", "Independent samples t-test") }
  })
  output$wmw <- renderUI({
    if((input$IV == "1 IV with 2 levels (independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" & input$normal == FALSE | 
        input$DV == "1 ordinal DV" | 
        input$DV == ""))  
    { actionLink("wmw_link","Wilcoxon-Mann Whitney test") }
  })
  output$chisq <- renderUI({
    if((input$IV == "1 IV with 2 levels (independent groups)" | 
        input$IV == "1 IV with 3+ levels (independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("chisq_link", "Chi-squared test") }
  })
  output$fisher <- renderUI({
    if((input$IV == "1 IV with 2 levels (independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("fisher_link", "Fisher's exact test") }
  })
  output$anova <- renderUI({
    if((input$IV == "1 IV with 3+ levels (independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("anova_link", "One-way ANOVA") }
  })
  output$kw <- renderUI({
    if((input$IV == "1 IV with 3+ levels (independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" & input$normal == FALSE | 
        input$DV == "1 ordinal DV" | input$DV == ""))  
    { actionLink("kw_link", "Kruskal Wallis test") }
  })
  output$pairedt <- renderUI({
    if((input$IV == "1 IV with 2 levels (dependent/matched groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("pairedt_link","Paired t-test") }
  })
  output$wsrt <- renderUI({
    if((input$IV == "1 IV with 2 levels (dependent/matched groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" & 
        input$normal == FALSE| 
        input$DV == "1 ordinal DV" | 
        input$DV == ""))  
    { actionLink("wsrt_link", "Wilcoxon signed ranks test") }
  })
  output$mcnemar <- renderUI({
    if((input$IV == "1 IV with 2 levels (dependent/matched groups)" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("mcnemar_link", "McNemar test") }
  })
  output$rmanova <- renderUI({
    if((input$IV == "1 IV with 3+ levels (dependent/matched groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("rmanova_link","One-way repeated measures ANOVA") }
  })
  output$friedman <- renderUI({
    if((input$IV == "1 IV with 3+ levels (dependent/matched groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" & input$normal == FALSE | 
        input$DV == "1 ordinal DV" | 
        input$DV == ""))  
    { actionLink("friedman_link","Friedman test") }
  })
  output$rmlog <- renderUI({
    if((input$IV == "1 IV with 3+ levels (dependent/matched groups)" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("rmlog_link","Repeated measures logistic regression") }
  })
  output$facanova <- renderUI({
    if((input$IV == "2+ IVs (all categorical with independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("facanova_link","Factorial ANOVA") }
  })
  output$ordlog <- renderUI({
    if((input$IV == "2+ IVs (all categorical with independent groups)" | 
        input$IV == "2+ IVs (interval or mixed types)" | 
        input$IV == "1 interval IV" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" & 
        input$normal == FALSE | 
        input$DV == "1 ordinal DV" | 
        input$DV == ""))
    { actionLink("ordlog_link","Ordered logistic regression") }
  })
  output$faclog <- renderUI({
    if((input$IV == "2+ IVs (all categorical with independent groups)" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("faclog_link","Factorial logistic regression") }
  })
  output$corr <- renderUI({
    if((input$IV == "1 interval IV" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("corr_link","Correlation") }
  })
  output$reg <- renderUI({
    if((input$IV == "1 interval IV" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)  
    { actionLink("reg_link","Linear regression") }
  })
  output$npc <- renderUI({
    if((input$IV == "1 interval IV" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" & 
        input$normal == FALSE | 
        input$DV == "1 ordinal DV" | 
        input$DV == ""))
    { actionLink("npc_link","Non-parametric correlation") }
  })
  output$log <- renderUI({
    if((input$IV == "1 interval IV" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("log_link","Logistic regression") }
  })
  output$multreg <- renderUI({
    if((input$IV == "2+ IVs (interval or mixed types)" | 
        input$IV == "") & (input$DV == "1 interval DV" | 
                           input$DV == "") & 
       input$normal == TRUE)
    { actionLink("multreg_link","Multiple regression") }
  })
  output$ancova <- renderUI({
    if((input$IV == "2+ IVs (interval or mixed types)" | 
        input$IV == "") & 
       (input$DV == "1 interval DV" | 
        input$DV == "") & 
       input$normal == TRUE)
    { actionLink("ancova_link","ANCOVA") }
  })
  output$multlog <- renderUI({
    if((input$IV == "2+ IVs (interval or mixed types)" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("multlog_link","Multiple logistic regression") }
  })
  output$disc <- renderUI({
    if((input$IV == "2+ IVs (interval or mixed types)" | 
        input$IV == "") & 
       (input$DV == "1 categorical DV (2 categories)" | 
        input$DV == "1 categorical DV (3+ categories)" | 
        input$DV == ""))  
    { actionLink("disc_link","Discriminant analysis") }
  })
  output$manova <- renderUI({
    if((input$IV == "1 IV with 2 levels (independent groups)" | 
        input$IV == "1 IV with 3+ levels (independent groups)" | 
        input$IV == "") & 
       (input$DV == "2+ interval DVs" | 
        input$DV == "") & 
       input$normal== TRUE)  
    { actionLink("manova_link","One-way MANOVA") }
  })
  output$manova2 <- renderUI({
    if((input$IV == "1 IV with 2 levels (dependent/matched groups)" | 
        input$IV == "1 IV with 3+ levels (dependent/matched groups)" | 
        input$IV == "") & 
       (input$DV == "2+ interval DVs" | 
        input$DV == "") & 
       input$normal== TRUE)  
    { actionLink("manova2_link","Repeated-measures MANOVA") }
  })
  output$mmreg <- renderUI({
    if((input$IV == "2+ IVs (interval or mixed types)" | 
        input$IV == "2+ IVs (all categorical with independent groups)" | 
        input$IV == "1 interval IV"  | 
        input$IV == "") & 
       (input$DV == "2+ interval DVs" | 
        input$DV == "") & 
       input$normal== TRUE)  
    { actionLink("mmreg_link","Multivariate multiple linear regression") }
  })
  output$factora <- renderUI({
    if((input$IV == "0 IVs" | 
        input$IV == "") & 
       (input$DV == "2+ interval DVs" | 
        input$DV == "") & 
       input$normal== TRUE)  
    { actionLink("factora_link","Factor analysis") }
  })
  
  explain <- reactiveValues(data = NULL)
  
  # create an observeEvent for each wiki entry
  lapply(1:length(wiki), function(x){
    name <- names(wiki)[x]
    observeEvent(input[[paste(name, "link", sep="_")]], {
      explain$wiki <- HTML(wiki[[name]])
    })
  })
  
  output$Wikipedia <- renderUI({
    explain$wiki
  })
  output$style <- renderUI({
    HTML("<style> code { 
         font-family: monospace;
         color:black;
         background-color:#F0F8FF}</style>")
  })
  }

shinyApp(ui = ui, server = server)
