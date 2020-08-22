###########################################################################
#                                                                         #
#             Shiny App: Left ventricular hypertrophy Dataset             # 
#                                                                         #
###########################################################################

# Patients receiving treatment for severe kidney disease sometimes also show 
# signs of an enlargement of the heart muscle, known as left ventricular 
# hypertrophy.  A study was carried out in a group of kidney patients to 
# identify factors which might be associated with left ventricular
# hypertrophy and also to identify whether this condition was a risk factor 
# for early death of the patient. The patients in the study were all adults
# between 19 and 50 years of age. The size of the heart muscle was assessed 
# through measurement of the left ventricular mass index. All patients were 
# then followed up for at least two years, or until death if that occurred 
# earlier.

# The left ventricular hypertrophy dataset contains the following variables:
#- llvmi: left ventricle mass index, on a log scale
#- sbp: systolic blood pressure
#- gender: 1 for male, 2 for female
#- card: previous cardiac disease
#- dgp: type of kidney disease
#- surv: patient death within two years (no = 0, yes = 1)

# set the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import the left ventricular hypertrophy dataset into R 
lvh <- read.table("./data/lvh.dat", header = T)

# Examine the dataset for any issues that may cause problems later in this project 

# Load the plyr library 
library(plyr)

# Some of the variables need to be converted to binary variables 
lvh$card <- revalue(as.factor(lvh$card), c("1" = "0" , "2"="1")) # Where 1 is yes and no is 0
lvh$dgp <- revalue(as.factor(lvh$dgp), c("2"="1", "1"="0")) # Previous card arrest = 1,
lvh$sex <- revalue(as.factor(lvh$sex), c("1" = "1", "2" = "0")) # Males = 1, Females = 0

# Convert surv to a factor variable 
lvh$surv <- as.factor(lvh$surv)

# Examine the lvh dataset 
summary(lvh)
str(lvh)


#  ------------------------------------------------------------------------
# Shiny App ---------------------------------------------------------------
#  ------------------------------------------------------------------------

# Load the shiny library 
library(shiny)
library(ggplot2)
library(ggExtra)

# User interface 
ui <- (
  
  # Create a title for the Shiny app 
  navbarPage("ADA Shiny Application",
             
             # Create the first tab 
             tabPanel("Summary Statistics",
                      
                      # Allos for fluid page layout 
                      fluidPage(
                        
                        # First page title 
                        titlePanel(title = (h2("Summary Statistics of the Left Ventricular Hypertrophy dataset", align = "center"))),
                      
                        # Sidebar layout with a input and output definitions 
                        sidebarLayout(
                          
                          # Sidebar panel for inputs 
                          sidebarPanel(
                            
                            # Choose a variable to find the summary statistics 
                            selectInput(inputId = "variable",
                                        label = "Choose a variable:",
                                        choices = c("Llvmi", "Sbp", "Sex", "Card", "Dgp", "Surv"))
                            
                            # Decide on the number of observations of the variable to view 
                            # numericInput(inputId = "obs",
                            #              label = "Number of observations to view:",
                            #              value = 10)
                          ),
                          
                          # Main panel for displaying outputs 
                          mainPanel(
                            
                            # Output some text about the variable 
                            tableOutput("variable_info"),
                            tags$head(tags$style("#selected_var{
                                                 font-size: 20px;}")),
                            
                            # Print the summary statistics 
                            verbatimTextOutput("summary")
                            
                            # Output a table with requested number of observations 
                            # tableOutput("view")
                          )
                        )
                      )
                    ),
             
             # Create a new tab for the marginal histograms 
             tabPanel("Marginal Histograms",
                      
                      # Create a fluid layout 
                      fluidPage(
                        
                        # Page title 
                        titlePanel(title = (h2("Marginal Histogram of two variables contained in the LVH dataset", align = "center"))),
                        
                        # Sidebar layout with a input and output definitions 
                        sidebarLayout(
                          
                          # Sidebar panel for inputs 
                          sidebarPanel(
                            
                            # The first variable to be inputted 
                            selectInput(inputId = "variable_m_hist1",
                                        label = "Choose the first variable:",
                                        choices = c("Llvmi", "Sbp", "Sex", "Card", "Dgp", "Surv")),
                            
                            # The second variable to be inputted 
                            selectInput(inputId = "variable_m_hist2",
                                        label = "Choose the second variable:",
                                        choices = c("Llvmi", "Sbp", "Sex", "Card", "Dgp", "Surv")),
                          
                            # Create a slider that will allow for the binwidths to be adjusted 
                            sliderInput("x_binwidth",
                                        label = "Binwidth for the X-axis:",
                                        min = 0.01, max = 20, value = 1),
                            
                            sliderInput("y_binwidth",
                                        label = "Binwidth for the Y-axis:",
                                        min = 0.01, max = 20, value = 1)
                          ),
                            
                          # Main panel for displaying outputs 
                          mainPanel(
                            
                            # Output the marginal histogram 
                            plotOutput("marginal_hist")
                            
                         )
                       )
                      )
             ),
            
             # Create a tab for the boxplots           
             tabPanel("Boxplots",
                      
              # Create a fluid layout 
              fluidPage(
               
               # Boxplot tab 
               titlePanel(title = (h2("Boxplot of a variable from the LVH dataset split by a second variable", align = "center"))),
               
               # Sidebar layout with a input and output definitions 
               sidebarLayout(
                 
                 # Sidebar panel for inputs 
                 sidebarPanel(
                   
                   # Input a boxplot variable 
                   selectInput(inputId = "boxplot_variable",
                               label = "Choose a variable to create the boxplot:",
                               choices = c("Llvmi", "Sbp")),
                   
                   # Input the variable to split the boxplots 
                   selectInput(inputId = "boxplot_split_variable",
                               label = "Choose a variable to group the boxplot:",
                               choices = c("Sex", "Card", "Dgp", "Surv"))
        
                 ),
                 
                 # Main panel for displaying outputs 
                 mainPanel(
                   
                   # Output the boxplot 
                   plotOutput("boxplot")
                   
                 )
               )
              )
             ),
             
             # Create a tab for the density plot 
             tabPanel("Density Plot",
                    
                      # Create a fluid page layout 
                      fluidPage(
                        
                        # Application title 
                        titlePanel(title = (h2("Density plot of one variable from the LVH dataset split by a second variable", align = "center"))),
                        
                        # Sidebar layout with a input and output definitions 
                        sidebarLayout(
                          
                          # Sidebar panel for inputs 
                          sidebarPanel(
                            
                            # Input variable for the density plot 
                            selectInput(inputId = "density_plot_variable",
                                        label = "Choose a variable to create the density plot:",
                                        choices = c("Llvmi", "Sbp")),
                            
                            # Input a variable to split the dnesity plot 
                            selectInput(inputId = "density_plot_split_variable",
                                        label = "Choose a variable to group the density plot:",
                                        choices = c("Sex", "Card", "Dgp", "Surv"))
                            
                          ),
                          
                          # Main panel for displaying outputs 
                          mainPanel(
                            
                            # Output the density plot 
                            plotOutput("density_plot")
                            
                          )
                        )
                      )
             ),
             
             # Create a tab for the linear model 
             tabPanel("Linear Model",
                      
                      # Allow for a fluid page layout 
                      fluidPage(
                        
                        # Title for the linear model tab 
                        titlePanel(title = (h2("Fit a linear model to the response variable Llvmi", align = "center"))),
                        
                        # Sidebar layout with a input and output definitions 
                        sidebarLayout(
                          
                          # Sidebar panel for; inputs 
                          sidebarPanel(
                            
                            # Choose the variables to be included 
                            # Add a list of tick boxes 
                            checkboxGroupInput("independent", "Choose the explanatory variables:",c("Spb" = "sbp", "Sex" = "sex", "Card" = "card", "Dgp" = "dgp", "Surv" = "surv"))
                          ),
                          
                            #  Main panel for displaying outputs ----
                            mainPanel(
                              tableOutput("regTab"), plotOutput("summary_plot")
                            )
                        )
                      )
             ),
             
             # Create a tab for the other types of regression models 
             tabPanel("Other forms of Regression Model",
                      
                      # Allow for a fluid page layout 
                      fluidPage(
                        
                        # Title for the other regression tab 
                        titlePanel(title = (h2("Fit a regression model between a chosen response variable and explanatory variables", align = "center"))),
                        
                        # Sidebar layout with a input and output definitions 
                        sidebarLayout(
                          
                          # Sidebar panel for inputs 
                          sidebarPanel(
                            
                            # Choose the type of regression model 
                            selectInput(inputId = "model_type",
                                        label = "Choose the type of regression model:",
                                        choices = c("Linear Regression" = "gaussian",
                                                    "Logistic Regression" = "binomial",
                                                    "Poisson Regression" = "poisson")),
                            
                            # Choose the response variable 
                            selectInput(inputId = "response_var",
                                        label = "Choose the response variable of the model:",
                                        choices = c("Llvmi" = "llvmi", "Sbp" = "sbp", "Sex" = "sex", "Card" = "card", "Dgp" = "dgp", "Surv" = "surv")),
                            
                            # Choose the explanatory variables 
                            checkboxGroupInput("exp_vars", "Choose the explanatory varialbes:",c("Llvmi"= "llvmi", "Sbp" = "sbp", "Sex" = "sex", "Card" = "card", "Dgp" = "dgp", "Surv" = "surv"))
                            ),
                          
                          # Main panel for displaying outputs 
                          mainPanel(
                            
                            # some errors still show, so hide all errors 
                            tags$style(type = "text/css", 
                                       ".shiny-output-error{visibility:hidden;}",
                                       ".shiny-output-error:before{visibility:hidden;}"),
                            
                            # Output a error message, the diagnostic plots and  
                            tableOutput("warn_message"),
                            tableOutput("warn_message_2"),
                            tableOutput("gen_reg_tab"), plotOutput("diagnostic_plots")
                          )
                        )
                      )
             )
  )
)

# Define the server function for the Shiny app 
server <- function(input, output){
  
  # Create some summary statistics 
  variableInput <- reactive({
    switch(input$variable,
           "Llvmi" = lvh$llvmi,
           "Sbp" = lvh$sbp,
           "Sex" = lvh$sex,
           "Card" = lvh$card,
           "Dgp" = lvh$dgp,
           "Surv" = lvh$surv)
  })
  
  # Output some general information about each variable 
  output$variable_info <- renderTable({
    if(input$variable == "Llvmi"){
      print(data.frame(Information = "The Llvmi variable represents the left ventricular mass index of a patient, which was measured on a log scale.", row.names = F))}
    
    else if(input$variable == "Sbp"){
      print(data.frame(Information = "The Sbp variable represents the systolic blood pressure of a patient."))}
    
    else if(input$variable == "Sex"){
      print(data.frame(Information = "The variable Sex represents the gender of a patient, where 1 represents Males and 0 Females."))}
    
    else if(input$variable == "Card"){
      print(data.frame(Information ="The Card variable is a binary variable, where 1 represents those patients who have had previous cardiac disease and 0 represents those who have not had previous cardiac disease."))}
    
    else if(input$variable == "Dgp"){
      print(data.frame(Information = "The Dgp is a binary variable which represents two different types of kidney disease."))}
    
    else if(input$variable == "Surv"){
      print(data.frame(Information = "The Surv variable is a binary variable which indicates whether a patient died within two years of the study or not."))}
  })
  
  # Generate a summary of the dataset 
  output$summary <- renderPrint({
    variable <- variableInput()
    summary(variable)
  })
  
  # Show the first "n" observations 
  # output$view <- renderTable({
  #   head(variableInput(), n = input$obs)
  # })
  
  # Also output some summary information about the variable 
  # output$selected_var <- renderText({ 
  #   paste("You have selected the", input$variable, " variable. I would like to also output some info about the variale too but I am not sure how to do this yet ", input$variable)
  # })
  
  # Create the marginal histogram 
  output$marginal_hist <- renderPlot({
    
    # Allow for easier changing of variables 
    var1 <- switch(input$variable_m_hist1, 
                   "Llvmi" = lvh$llvmi,
                   "Sbp" = lvh$sbp,
                   "Sex" = as.numeric(lvh$sex),
                   "Card" = as.numeric(lvh$card),
                   "Dgp" = as.numeric(lvh$dgp),
                   "Surv" = as.numeric(lvh$surv))
    
    var2 <- switch(input$variable_m_hist2, 
                   "Llvmi" = lvh$llvmi,
                   "Sbp" = lvh$sbp,
                   "Sex" = as.numeric(lvh$sex),
                   "Card" = as.numeric(lvh$card),
                   "Dgp" = as.numeric(lvh$dgp),
                   "Surv" = as.numeric(lvh$surv))
    
    # Create the a ggplot object 
    p <- ggplot(lvh, aes(x = var1, y = var2)) +# , color=cyl, size=cyl)) +
    geom_point() + xlab(paste(input$variable_m_hist1)) + ylab(paste(input$variable_m_hist2)) +
    theme(legend.position="none", axis.title=element_text(size=14,face="bold"))
  
  # Create a marginal histogram 
  ggMarginal(p, type="histogram", xparams = list(binwidth = input$x_binwidth, fill = "#F8766D"), yparams = list(binwidth = input$y_binwidth, fill = "#00BFC4"))
  })
  
  # Create a boxplot 
  output$boxplot <- renderPlot({
    
    # Allow for easier changing of variables 
    var1 <- switch(input$boxplot_variable, 
                   "Llvmi" = lvh$llvmi,
                   "Sbp" = lvh$sbp)
    
    var2 <- switch(input$boxplot_split_variable, 
                   "Sex" = lvh$sex,
                   "Card" = lvh$card,
                   "Dgp" = lvh$dgp,
                   "Surv" = lvh$surv)
    
    # Create a split boxplot 
    ggplot(lvh, aes(x = var2, y = var1, fill = var2)) + geom_boxplot() +
    xlab(paste(input$boxplot_split_variable)) + ylab(paste(input$boxplot_variable)) +
    theme(legend.position="none", axis.title=element_text(size=14,face="bold"))
  })
  
  # Create a density plot 
  output$density_plot <- renderPlot({
    
    # Allow for easier changing of variables 
    var_density1 <- switch(input$density_plot_variable, 
                   "Llvmi" = lvh$llvmi,
                   "Sbp" = lvh$sbp)
    
    var_density2 <- switch(input$density_plot_split_variable, 
                   "Sex" = as.factor(lvh$sex),
                   "Card" = as.factor(lvh$card),
                   "Dgp" = as.factor(lvh$dgp),
                   "Surv" = as.factor(lvh$surv))
    
    # Add the new variables to a new data frame 
    lvh2 <- cbind(lvh, var_density1, var_density2)
    
    # Create a split density plot 
    ggplot(lvh2, aes(x = var_density1, fill = as.factor(var_density2))) + geom_density() +
      facet_wrap(~as.factor(var_density2)) +
    xlab(paste(input$density_plot_variable)) + ylab("Density") +
      theme(legend.position="none", axis.title=element_text(size=14,face="bold"))
  })
  
  # Run the linear regression model 
  runRegression <- reactive(lm(as.formula(paste("llvmi"," ~ ",paste(input$independent,collapse="+"))),data=lvh))

  # Create an error message that informs the user to select some paramaters 
  output$regTab <- renderTable({
    if(!is.null(input$independent)){
      summary(runRegression())$coefficients
    } else {
      print(data.frame(Warning="Please select model parameters."))
      }
  }, include.rownames = T)
  
  # Output the diagnostics plots 
  output$summary_plot <- renderPlot(({
    if(!is.null(input$independent)){
      par(mfrow = c(2,2))
      plot(runRegression())}
    else {print(data.frame(warning = " "))}
  }))
  
  # Create a glm regression model    
  gen_Regression <- reactive({glm(as.formula(paste(input$response_var," ~ ",paste(input$exp_vars,collapse="+"))),data=lvh, family = input$model_type)})
  
  # Create an error message that informs the user to select some paramaters 
  output$gen_reg_tab <- renderTable({
    
    if(!is.null(input$exp_vars)){
      summary(gen_Regression())$coefficients
    } else {
      print(data.frame(Warning="Please select model parameters."), include.rownames = FALSE)
    }
  },include.rownames = T)
  
  # Output the diagnostics plots 
  output$diagnostic_plots <- renderPlot(({
    if(!is.null(input$exp_vars)){
      par(mfrow = c(2,2))
      plot(gen_Regression())}
    else {print(data.frame(warning = " "))}
  }))
  
  # Create error warning messages to inform the doctor which models to use and not 
  output$warn_message <- renderTable({
     if(input$model_type == "binomial" && input$response_var == "llvmi"){
       print(data.frame(Warning = "A logistic regression model is not suitable for the contineous variable Llvmi."))}
     
     else if(input$model_type == "binomial" && input$response_var == "sbp"){
       print(data.frame(Warning = "A logistic regression model is not suitable for the contineous variable Sbp."))}
     
     else if(input$model_type == "poisson" && input$response_var == "sex"){
       print(data.frame(Warning = "A poisson regression model is not suitable for the binary variable Sex."))}
     
     else if(input$model_type == "poisson" && input$response_var == "dgp"){
       print(data.frame(Warning = "A poisson regression model is not suitable for the binary variable Dgp."))}
     
     else if(input$model_type == "poisson" && input$response_var == "card"){
       print(data.frame(Warning = "A poisson regression model is not suitable for the binary variable Card."))}
     
     else if(input$model_type == "poisson" && input$response_var == "surv"){
       print(data.frame(Warning = "A poisson regression model is not suitable for the binary variable Surv."))}
     
     else if(input$model_type == "gaussian" && input$response_var == "surv"){
       print(data.frame(Warning = "A linear regression model is not suitable for the binary variable Surv."))}
     
     else if(input$model_type == "gaussian" && input$response_var == "sex"){
       print(data.frame(Warning = "A linear regression model is not suitable for the binary variable Sex."))}
     
     else if(input$model_type == "gaussian" && input$response_var == "dgp"){
       print(data.frame(Warning = "A linear regression model is not suitable for the binary variable Dgp."))}
     
     else if(input$model_type == "gaussian" && input$response_var == "card"){
       print(data.frame(Warning = "A linear regression model is not suitable for the binary variable Card."))}
     })
  
  # Supress all R errors 
  options(shiny.sanitize.errors = T)
  
  # Create another warning message about including the response variable in the model 
  output$warn_message_2 <- renderTable({
    if(input$response_var == "sbp" && "sbp" %in% input$exp_vars){
      print(data.frame(Warning = "The response variable Sbp should not be included as a explanatory variable in the model."))}
    
    else if(input$response_var == "llvmi" && "llvmi" %in% input$exp_vars){
      print(data.frame(Warning = "The response variable Llvmi should not be included as a explanatory variable in the model."))}
    
    else if(input$response_var == "card" && "card" %in% input$exp_vars){
      print(data.frame(Warning = "The response variable Card should not be included as a explanatory variable in the model."))}
    
    else if(input$response_var == "dgp" && "dgp" %in% input$exp_vars){
      print(data.frame(Warning = "The response variable Dgp should not be included as a explanatory variable in the model."))}
    
    else if(input$response_var == "surv" && "surv" %in% input$exp_vars){
      print(data.frame(Warning = "The response variable Surv should not be included as a explanatory variable in the model."))}
    
    else if(input$response_var == "sex" && "sex" %in% input$exp_vars){
      print(data.frame(Warning = "The response variable Sex should not be included as a explanatory variable in the model."))}
  })
}

# Create my Shiny app 
shinyApp(ui = ui, server = server)


