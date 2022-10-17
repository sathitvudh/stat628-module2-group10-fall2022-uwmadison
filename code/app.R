library(shiny)
library(ggplot2)
library(scales)
library(shinythemes)
library(shinytoastr)

bodyfat_stand = function(bodyfat){
  if (bodyfat < 2) return(NA) #"Extremely below normal range! Please check your input."
  if (2 <= bodyfat & bodyfat < 6) return("Essential fat")
  if (6 <= bodyfat & bodyfat < 13) return("Athletes")
  if (13 <= bodyfat & bodyfat < 17) return("Fitness")
  if (17 <= bodyfat & bodyfat < 24) return("Average")
  if (bodyfat >= 25) return("Obese")
}



ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel(h1("Body Fat Calculator")),
  sidebarPanel(h2("Please enter"),
               numericInput("abodmen",h5("Circumference of abdomen(cm)"), value = 68, min = 68, max = 150),
              
               numericInput("weight",h5("Weight(lbs)"), value = 100, min = 100, max = 400),
               
               
               numericInput("wrist",h5("Circumference of wrist(cm)"), value = 15,min = 15,max = 22),
              
               submitButton()
               
               
  ),
  
  mainPanel(
    htmlOutput(outputId = "bodyfat"),
    h3(textOutput("space")),
    h4(textOutput("info")),
    textOutput("contact1"),textOutput("contact2"),textOutput("contact3"),textOutput("contact4")
  )
)
  
  
  
server <- function(input,output) {
  output$bodyfat = renderUI({
    validate(
      need(input$abodmen, 'Please enter circumference of abdomen(cm)'),
      need(input$weight, 'Please enter weight'),
      need(input$wrist, 'Please enter circumference of wrist(cm)')
    )
    abodmen = input$abodmen
    weight = input$weight 
    wrist = input$wrist
    if (is.na(abodmen)){
      HTML(paste(h1("abdomen's range should be in between 68cm-150cm")))
    }
    fat = -23.93449 + 0.89794*abodmen-0.10557*weight-1.16961*wrist
    if (abodmen<65|abodmen>150){
    HTML(paste(h1("abdomen's range should be in between 68cm-150cm")))
    }else if(weight<100 |weight>400){
      HTML(paste(h1("weight's range should be in between 100lbs-400lbs")))
    }else if (wrist<15 | wrist>22){
      HTML(paste(h1("wrist's range should be in between 15cm-22cm")))
    }else if (fat<2){
      HTML(paste(h1("Your body fat seems abnormal,please reenter your information")))
    }
    else{
      str1 <- paste(h4("Your predicted body fat percentage is:"),h1(round(fat,1),"%"))
      HTML(paste(str1,h4("According to American Council on Exercise Body Fat Categorization, your body fat falls in range:    "), h1(bodyfat_stand(fat), align = "center"), sep = '<br/>'))
    }
  })
  output$space = renderText({"---------------------------------------------------------------------------------------------"})
  output$info = renderText({ "Contact us"})
  output$contact1 = renderText({ "E-mail: shuang452@wisc.edu"})
  output$contact2 = renderText({ "E-mail: sathitvudh@wisc.edu"})
  output$contact3 = renderText({ "E-mail: eltgani@wisc.edu"})
  output$contact4 = renderText({ "E-mail: xwang2587@wisc.edu"})
}

shinyApp(ui, server)

