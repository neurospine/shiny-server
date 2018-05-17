library(shiny)
library(shinythemes)

#define UI
shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  
  #Application title
  titlePanel(title = span(h1(strong("DeepLDH - A preoperative prediction tool for lumbar discectomy based on deep learning"), align = "center"), style = "color:white"), windowTitle = "DeepLDH"),
  
  sidebarLayout(
  sidebarPanel("Describe your patient", 
  sliderInput("age", "Select age [years]", min = 16, max = 100, value = 45, step = 1),
  radioButtons("female" , "Select gender" , choices = list("Female" = 1, "Male" = 0) , selected = 1),
  sliderInput("height", "Select height [cm]", min = 100, max = 220, value = 170, step = 1),
  sliderInput("weight", "Select weight [kg]", min = 25, max = 220, value = 70, step = 1),
  radioButtons("prior", "Prior discectomy at index level?", choices = list("Yes" = 1, "No" = 0), selected = 0),
  sliderInput("leg", "Current leg pain severity [NRS]", min = 0, max = 10, value = 5, step = 1),
  sliderInput("back", "Current back pain severity [NRS]", min = 0, max = 10, value = 5, step = 1),
  sliderInput("odi", "Current functional disability [Oswestry Disability Index]", min = 0, max = 100, value = 45, step = 1),
  radioButtons("smoking", "Active smoker?" , choices = list("Yes" = 1, "No" = 0) , selected = 0),
  radioButtons("alcohol", "Regular alcohol consumption?" , choices = list("Yes" = 1, "No" = 0) , selected = 1),
  radioButtons("drugs", "Recreational drug use?", choices = list("Yes" = 1, "No" = 0), selected = 0),
  sliderInput("asascore", "American Society of Anesthesiologists (ASA) score", min = 1, max = 5, value = 1, step = 1),
  selectInput("level", "Select index level", choices = list("L1-L2" = 0, "L2-L3" = 1, "L3-L4" = 2, "L4-L5" = 3, "L5-S1" = 4), selected = 3),
  selectInput("side", "Where is the herniation located?", choices = list("Bilateral" = 0, "Left side" = 1, "Right side" = 2, "Midline" = 3), selected = 1),
  radioButtons("farlateral", "Is the disc herniation classifiable as 'far-lateral' ?" , choices = list("Yes" = 1, "No" = 0) , selected = 0),
  radioButtons("listhesis", "Is relevant spondylolisthesis present?" , choices = list("Yes" = 1, "No" = 0) , selected = 0),
  radioButtons("stenosis", "Is relevant stenosis present?" , choices = list("Yes" = 1, "No" = 0) , selected = 0),
  radioButtons("sequester", "Is the disc herniation sequestered?" , choices = list("Yes" = 1, "No" = 0) , selected = 0),
  radioButtons("bulging", "Is the nerve compressed by a broad bulging disc, rather than a true disc herniation?" , choices = list("Yes" = 1, "No" = 0) , selected = 0)),


  mainPanel(span(h1(strong("Manual")), style="color:darkorange"),
            span(h3("1. Describe your patient by completing the fields to the left."),  style = "color:white"), 
            span(h3("2. Then, make a prediction by clicking the 'Predict' button below."),  style = "color:white"), 
             
            actionButton("predict", h2(strong("Predict")), width = "25%", icon("bolt") , 
                         style="color:black; background-color:orange; border-color:white"),
            br(),
            br(),
            span(h2("Leg Pain Severity (Numeric Rating Scale)", align = "center"), style = "color:darkorange"),
            span(h3("Likelihood", align = "center"), style = "color:deepskyblue"),
            span(h4(textOutput("probleg"), align = "center"),  style = "color:white"),
            span(h3("Prediction", align = "center"), style = "color:deepskyblue"),
            span(h4(textOutput("predleg"), align = "center"), style = "color:white"),
            
      br(),
      br(),
      br(),
            span(h2("Back Pain Severity (Numeric Rating Scale)", align = "center"),  style = "color:darkorange"),
            span(h3("Likelihood", align = "center"), style = "color:deepskyblue"),
            span(h4(textOutput("probback"), align = "center"),  style = "color:white"),
            span(h3("Prediction", align = "center"), style = "color:deepskyblue"),
            span(h4(textOutput("predback"), align = "center"), style = "color:white"),
      br(),
      br(),
      br(),
     
            span(h2("Functional Disability (Oswestry Disability Index)", align = "center"),  style = "color:darkorange"),
            span(h3("Likelihood", align = "center"), style = "color:deepskyblue"),
            span(h4(textOutput("probodi"), align = "center"),  style = "color:white"),
            span(h3("Prediction", align = "center"), style = "color:deepskyblue"),
            span(h4(textOutput("predodi"), align = "center"), style = "color:white"),
      br(),
      br(),
      br(),
      br(),
  span(h4("What does this mean?"), style = "color:deepskyblue"),   
  span(h4("A clinically relevant improvement is defined as a 30% or greater improvement in pain or functional disability scores at 1 year postoperatively. This is according to the disease-specific minimum clinically important difference (MCID) threshold set for these patient-reported outcome measures by Ostelo et al. [1]" , style = "color:white")),
  
  span(h6("[1]    Ostelo, R.W.J.G., Deyo, R.A., Stratford, P., Waddell, G., Croft, P., Von Korff, M., Bouter, L.M., de Vet, H.C., 2008. Interpreting change scores for pain and functional status in low back pain: towards international consensus regarding minimal important change. Spine 33, 90-94."), style = "color:white"))
  )))
 