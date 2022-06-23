#remove.packages("devtools")
#install.packages("devtools")
# devtools::install_github("karbartolome/shinymatic")
# devtools::install_version("devtools", version = "2.4.3")
# devtools::install_github("karbartolome/shinymatic")
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
.libPaths(Sys.getenv("R_LIBS_USER"))  # add to the path
devtools::install_github("karbartolome/shinymatic")

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
titanic_date <- as.Date('1911-05-31')
datos <- df %>% 
  clean_names() %>% 
  mutate(
    pclass=factor(pclass),
    sex=factor(sex),
    embarked=factor(embarked),
    survived = factor(survived),
    birthdate=titanic_date-age*365+ runif(nrow(.),0,0.99)) %>% 
  select(survived, pclass, fare, sex, birthdate, embarked)

# Modelo ------------------------------------------------------------------

preproc <- recipe(survived~., data=datos) %>% 
  step_mutate(age = as.numeric(titanic_date-birthdate)) %>% 
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

datos <- datos %>% select(-survived)

tab_inferences <- sidebarLayout(
  
  sidebarPanel(
    
    fluidRow(
    column(
      width=12,
      h4('Enter information'),
      autoinputs(.df = datos, .dec_places = 0)
    ))
  ),
  
  
  mainPanel(fluidRow(
    box(
      width = 12,
      title = "Model test",
      "Automatic output with shinymatic::autooutput_df()"
    ),
    h4('Predicted class and probability of survival'),
    
    fluidRow(
      tableOutput(outputId = "data_test"),
      valueBoxOutput("box_inferencia_prob"),
      valueBoxOutput("box_inferencia_clase"),
    )
  ))
  
)


ui <- dashboardPage(
  skin = "black",
  header = dashboardHeader(title = '{shinymatic}'),
  sidebar = dashboardSidebar(sidebarMenu(
    menuItem(
      "Tidymodels inferences",
      icon = icon("th"),
      tabName = "inferences"
    )
  )),
  
  body = dashboardBody(
    h2('Tidymodels test'),
    h4('Using shinymatic functions on a shiny app'),
    tabItems(tabItem(tabName = "inferences",
                     tab_inferences))
    
  )
)


server <- function(input, output) {
  
  output$data_test <- renderTable ({
    autooutput_df(.df=datos, .inputs=input, .dates_as_str=TRUE)
  })
  
  output$box_inferencia_prob <- renderValueBox({
   
   data_test <- autooutput_df(.df=datos, .inputs=input)
    
   valueBox(
     value = round(predict(wf, data_test, type='prob')$.pred_1*100,2),
     subtitle = "Survival probability",
     icon = icon("percent"),
     width = 6,
     color = "blue")
  })
  
  output$box_inferencia_clase <- renderValueBox({
    data_test <- autooutput_df(.df=datos, .inputs=input)
    
    valueBox(
      value = paste0(ifelse(predict(wf, data_test)$.pred_class=='1',
                           'Survived','Did not survive')),
      subtitle = "Prediction",
      width = 6,
      color = "blue")
  
     
  })
  
}

shinyApp(ui = ui, server = server)
