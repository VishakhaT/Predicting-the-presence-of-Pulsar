---
    title: 'STAT656: Final Project'
    subtitle: 'Predicting a Pulsar'
    output:
    html_document: default
---

packs = c('dplyr','ggplot2','AppliedPredictiveModeling', 'caret','RANN','corrplot', 'MASS', 'pROC', 'glmnet', 'readr', 'shiny')
lapply(packs,require,character.only=TRUE)

# Define UI for application that outputs Confusion Matrix
ui <- fluidPage(

    # Application title
    titlePanel("Comparing Classification Methods"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            radioButtons(inputId      = "methodType",
                         label        = "Classification Method:",
                         choiceNames  = c('Logistic','Elastic Net','K-Nearest Neighbors','Support Vector Machine','Flexible Discriminant Aanalysis'),
                         choiceValues = c('logit','enet','knn','svm','fda')),
        ),

        # Show Confusion Matrix Table
        mainPanel(
           verbatimTextOutput(outputId = "conMatrix")
        )
    )
)

# Data Pre-Processing
pulsar  = read.csv("data/pulsar_stars.csv")
Y       = make.names(pulsar$target_class)
X       = dplyr::select(pulsar, -target_class)

set.seed(1)

trainSplit  = createDataPartition(y = Y, p = 0.8, list = FALSE)
Ytrain      = Y[trainSplit]
Xtrain      = X[trainSplit,]
XtrainMat   = as.matrix(Xtrain)
Ytest       = Y[-trainSplit]
Xtest       = X[-trainSplit,]
XtestMat    = as.matrix(Xtest)
Ytest       = relevel(as.factor(Ytest), ref = 'X1')
Ytrain      = relevel(as.factor(Ytrain), ref = 'X1')

# Define server logic required to output Confusion Matrix
server = function(input, output) {


    output$conMatrix = renderPrint({

        if(input$methodType == 'logit'){
            method          = 'glm'
            tuneGrid        = NULL
            trControl       = trainControl(method = 'none')
            metric          = 'Kappa'
        }

        if(input$methodType == 'enet'){
            method          = 'glmnet'
            trControl       = trainControl(method = "cv", number = 5)
            tuneGrid        = expand.grid('alpha'=c(.5, 1),'lambda' = seq(0.0001, .01, length.out = 10))
            metric          = 'Kappa'
        }

        if(input$methodType == 'knn'){
            method          = 'knn'
            tuneGrid        = expand.grid(k = c(1:30))
            trControl       = trainControl(method = 'cv', number = 5)
            metric          = 'Kappa'
        }

        if(input$methodType == 'svm'){
            method          = 'svmLinear'
            tuneGrid        = expand.grid(C = c(.001, .01, .1, 1, 10, 50))
            trControl       = trainControl(method = 'cv', number = 5)
            metric          = 'Kappa'
        }
        
        if(input$methodType == 'fda'){
            method          = 'fda'
            tuneGrid        = expand.grid(degree = 1:4, nprune = c(10:30))
            trControl       = trainControl(method = 'cv', number = 5, classProbs = TRUE)
            metric          = 'Kappa'
        }

        
        outTrain = train(Xtrain, Ytrain, method = method, tuneGrid = tuneGrid, trControl = trControl, metric = metric)
        YhatTest = predict(outTrain, Xtest, type = 'raw')
        
        (confusionMatrix(reference = Ytest, data = YhatTest))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
