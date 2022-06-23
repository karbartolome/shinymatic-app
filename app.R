# install.packages("devtools")
#devtools::install_github("karbartolome/shinymatic")

library(shinymatic)
library(shiny)
library(shinydashboard)
library(janitor)
library(dplyr)
library(recipes)
library(rsample)
library(parsnip)
library(workflows)

df <- read.csv('https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv')

datos <- df %>% 
  clean_names() %>% 
  mutate(
    survived = factor(survived),
    age = age + runif(nrow(.),0,0.99),
    birthdate=Sys.Date()-age) %>% 
  select(survived, pclass, fare, sex, birthdate, embarked)

# Modelo ------------------------------------------------------------------

preproc <- recipe(survived~., data=datos) %>% 
  step_mutate(age = as.numeric(Sys.Date()-birthdate)) %>% 
  step_rm(birthdate) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.1) %>% 
  step_dummy(all_nominal_predictors())

preproc %>% prep() %>% juice()

model <- logistic_reg() %>%
  set_engine('glm') %>%
  set_mode('classification')

wf <- workflow() %>% 
  add_recipe(preproc) %>% 
  add_model(model) %>% 
  fit(datos)


# Shiny app ---------------------------------------------------------------

ui <- shiny::fluidPage(fluidRow(
  column(3,
         h3('Inputs based on df'),
         autoinputs(.df=datos, .dec_places = 0)
  ),
  column(3,
         h3('Outputs based on inputs'),
         tableOutput(outputId = 'data_test'),
         # valueBoxOutput("box_inferencia_prob"),
         # valueBoxOutput("box_inferencia_clase")
         
  )
))


server <- function(input, output) {
  
  data_preds <- reactive({
    autooutput_df(.df=datos, .inputs=input, .dates_as_str=TRUE)
  })
  
  output$data_test <- renderTable ({
    data_preds()
  })
  
  # output$box_inferencia_prob <- renderValueBox({
  #   
  #   valueBox(
  #     value = round(predict(wf, data_preds(), type='prob')$.pred_1*100,2),
  #     subtitle = "Survival probability",
  #     icon = icon("percent"),
  #     width = 6,
  #     color = "blue")
  # })
  # 
  # output$box_inferencia_clase <- renderValueBox({
  #   
  #   valueBox(
  #     value = paste0(ifelse(predict(wf, data_preds())$.pred_class=='1',
  #                           'Survived','Did not survive')),
  #     subtitle = "Prediction",
  #     width = 6,
  #     color = "blue")
  #   
  #   
  # })
  
}

shinyApp(ui = ui, server = server)
