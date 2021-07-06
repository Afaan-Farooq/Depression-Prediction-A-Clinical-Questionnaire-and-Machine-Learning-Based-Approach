library(shiny)

ui <- fluidPage(
  titlePanel("Depression Test"),
  
  sidebarLayout(
    sidebarPanel(
      h1(id="big-heading","Take the test:"),
      tags$style(HTML("#big-heading{color: green;}")),
  
      radioButtons(inputId="first", label=em("How often are you feeling down,depressed, irritable, sad or hopeless?"),  choices = list("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="second", label=em("How often do you have little interest or pleasure in doing normal things?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="third", label=em("How often are you having trouble falling asleep or sleeping too much?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="fourth", label=em("How often do you have a poor appetite, or you overeat?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="fifth", label=em("How often have you been feeling tired,or having little energy or feel lifeless?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="sixth", label=em("How often have you been feeling bad about yourself or feeling that you are a failure, or that you have let yourself or your family down?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="seventh", label=em("How often have you been moving or speaking so slowly that other people could have noticed? Or the opposite - being so fidgety or restless that you were moving around a lot more than usual and is noticeable?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="eighth", label=em("How often have you been bothered by thoughts that you would be better off dead, or of hurting yourself in some way?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="ninth", label=em("How often do you feel aches or cramps without a clear cause?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="tenth", label=em("Have you been more easily tearful or crying more than usual?"), c("None" = 0,"Little" = 1,"Some" = 2,"Much" = 3,"Most" = 4)),
                radioButtons(inputId="eleventh", label=em("Have you lost weight?"), c("Yes" = 1,"No" = 0),selected = 0),
                radioButtons(inputId="twelfth", label=em("Did you get any kind of social support from your family and relatives?"), c("Yes" = 1,"No" = 0),selected = 0),
                radioButtons(inputId="thirteenth", label=em("Do you need great effort to do simple things/is it difficult for you?"), c("Yes" = 1,"No" = 0),selected = 0),
                radioButtons(inputId="fourteenth", label=em("Are you able to make decisions?"), c("Yes" = 1,"No" = 0),selected = 0),
                radioButtons(inputId="fifteenth", label=em("Is anyone from your family suffering from psychiatric disorder?"), c("Yes" = 1,"No" = 0),selected = 0),
                radioButtons(inputId="sixteenth", label=em("Do you have any kind of addiction(say cigarettes, drugs etc)?"), c("Yes" = 1,"No" = 0),selected = 0),
                submitButton(em("Submit"))),
  mainPanel(
    
                 textOutput(outputId= "abc"),           
                 img(src='https://img-s-msn-com.akamaized.net/tenant/amp/entityid/BBTUAgx.img?h=768&w=1366&m=6&q=60&o=f&l=f&x=1606&y=1143', align= 'center')
           ))
)

#https://i.ytimg.com/vi/KSTKheVpEq4/maxresdefault.jpg

server <- function(input, output){ 
  
  new_depression <- reactive({ data.frame("ASadness"=as.numeric(input$first),
                               "Loconcen"=as.numeric(input$second),
                               "Asleep"=as.numeric(input$third),
                               "Aappet"=as.numeric(input$fourth),
                               "Loenergy"=as.numeric(input$fifth),
                               "Foguilt"=as.numeric(input$sixth),
                               "Asbehav"=as.numeric(input$seventh),
                               "Sthough"=as.numeric(input$eighth),
                               "Ppains"=as.numeric(input$ninth),
                               "Eactivity"=as.numeric(input$tenth),
                               "Wloss"=as.numeric(input$eleventh),
                               "Ssupport"=as.numeric(input$twelfth),
                               "Etdsthin"=as.numeric(input$thirteenth),
                               "Dmaking"=as.numeric(input$fourteenth),
                               "Fhopilln"=as.numeric(input$fifteenth),
                               "Addiction"=as.numeric(input$sixteenth))
  })
  
    library(caret)
    depression_model <- read.csv("depression_nor.csv" , header=TRUE , stringsAsFactors = F)
    
    set.seed(3233)
    intrain <- createDataPartition(y = depression_model$Depression, p= 0.7, list = FALSE)
    training <- depression_model[intrain,]
    testing <- depression_model[-intrain,]
    
    dim(training)
    dim(testing)
    
    training[["Depression"]] = factor(training[["Depression"]])
    
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    set.seed(3233)
    svm_Radial <- train(Depression ~., data = training, method ="svmRadial",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
    svm_Radial
    
    test_pred_Radial <- predict(svm_Radial, newdata = testing)
    tab <- table(Predicted = test_pred_Radial, Actual = testing$Depression )
    tab
    1 - sum(diag(tab))/sum(tab)
    
    
    answers <- reactive({
      predict(svm_Radial, newdata=new_depression())
    })
    
  
  
  output$abc <- renderPrint({
    if (as.numeric(answers()) == 1 ) {
      "Your test results are negative"
    } else {
      "Your test results are positive"
    }
    })

  
}


shinyApp(ui, server)  


