library(shiny)

library(keras)
library(openxlsx)
library(reticulate)

library(rsconnect)

library(RJSONIO)
library(crayon)

shinyServer(function(input, output) {
  
  set.seed(123)
  
  observe({
  supervised <- as.numeric(1)
  female <- as.numeric(input$female)
  prior <- as.numeric(input$prior)
  smoking <- as.numeric(input$smoking)
  alcohol <- as.numeric(input$alcohol)
  drugs <- as.numeric(input$drugs)
  farlateral <- as.numeric(input$farlateral)
  stenosis <- as.numeric(input$stenosis)
  sequester <- as.numeric(input$sequester)
  listhesis <- as.numeric(input$listhesis)
  bulging <- as.numeric(input$bulging)
  side <- as.numeric(input$side)
  asascore <- as.numeric(input$asascore)
  level <- as.numeric(input$level)
  odi <- as.numeric(input$odi)
  backpain <- as.numeric(input$back)
  legpain <- as.numeric(input$leg)
  weight <- as.numeric(input$weight)
  height <- as.numeric(input$height)
  age <- as.numeric(input$age)
  bmi <- as.numeric(weight/(height/100*height/100))
 
  newdata <- cbind(supervised, female, prior, smoking, alcohol, drugs, farlateral, stenosis, sequester, listhesis, bulging, age, asascore, bmi, height, weight, level, side, odi, backpain, legpain)
  newdata <- as.data.frame(newdata)
  
  olddata <- read.xlsx("datadfl.xlsx", sheet = 1)
  
  dfl <- as.data.frame(rbind(newdata, olddata))
                       
               dflnorm <-  scale(dfl[c(12,14,15,16,19,20,21)])
                       df  <- as.data.frame(cbind(dflnorm, dfl[c(1:11,13,17,18)])) 
                       dfl <- df[1,]
                       dfl <- as.matrix(dfl)
               
                    
                       
                       
                       #Leeeeegs
                       FLAGS <- tfruns::flags(
                         flag_numeric('dropout1', 0.8, 'drop1'),
                         flag_numeric('dropout2', 0.5, "drop2"),
                         flag_numeric("dropout3", 0, "drop3"),
                         flag_numeric("dropout4", 0, "drop4"),
                         flag_integer('u1', 256, 'Units1'),
                         flag_integer("u2", 128, "units2"),
                         flag_integer("u3", 64, "units3"),
                         flag_integer("u4", 32, "units4"),
                         flag_integer("u5", 4, "units5"),
                         flag_integer("batch", 5, "batchsize"),
                         flag_integer("epoch", 120, "epochs"),
                         flag_numeric("alpha", 0.002, "learningrate")
                       )
                       model_keras <- keras_model_sequential()
                       model_keras %>% 
                         layer_dense(units = FLAGS$u1, activation = "relu", input_shape = c(21)) %>%
                         #layer_batch_normalization() %>%
                         layer_dropout(rate = FLAGS$dropout1) %>%
                         layer_dense(units = FLAGS$u2, activation = "relu") %>%
                         #layer_batch_normalization() %>%
                         layer_dropout(rate = FLAGS$dropout2) %>%
                         layer_dense(units = FLAGS$u3, activation = "relu") %>%
                         #layer_batch_normalization() %>%
                         layer_dropout(rate = FLAGS$dropout3) %>%
                         layer_dense(units = FLAGS$u4, activation = "relu") %>%
                         #layer_batch_normalization() %>%
                         layer_dropout(rate = FLAGS$dropout4) %>%
                         layer_dense(units = FLAGS$u5, activation = "relu") %>%
                         #layer_batch_normalization() %>%
                         layer_dense(units = 2, activation = "softmax")
                       
                       model_keras %>% load_model_weights_hdf5("lp.h5")
                       
          observeEvent(input$predict, {
            probleg <- model_keras %>% predict_proba(dfl)
            predleg <- model_keras %>% predict_classes(dfl) %>% as.vector()
            probleg <- probleg[1,2]
            probleg <- round(as.numeric(probleg) * 100, digits = 1)
            predleg <- if(predleg > 0.5){print("likely")} else {print("unlikely")}
           
           
            output$probleg <- renderPrint(cat("\tThe estimated likelihood of a clinically relevant improvement in leg pain is", probleg, "%.\n"))
            output$predleg <- renderPrint(cat("\tBased on the deep learning model, it is", predleg, "that there will be a clinically relevant improvement in leg pain at 1 year postoperatively.\n"))
            
            
          })   
            
            
            
            
            
            
            #backpain
            FLAGS <- tfruns::flags(
              flag_numeric('dropout1', 0.8, 'drop1'),
              flag_numeric('dropout2', 0.5, "drop2"),
              flag_numeric("dropout3", 0, "drop3"),
              flag_numeric("dropout4", 0, "drop4"),
              flag_integer('u1', 256, 'Units1'),
              flag_integer("u2", 128, "units2"),
              flag_integer("u3", 64, "units3"),
              flag_integer("u4", 32, "units4"),
              flag_integer("u5", 4, "units5"),
              flag_integer("batch", 5, "batchsize"),
              flag_integer("epoch", 120, "epochs"),
              flag_numeric("alpha", 0.002, "learningrate")
            )
            model_keras2 <- keras_model_sequential()
            model_keras2 %>% 
              layer_dense(units = FLAGS$u1, activation = "relu", input_shape = c(21)) %>%
              #layer_batch_normalization() %>%
              layer_dropout(rate = FLAGS$dropout1) %>%
              layer_dense(units = FLAGS$u2, activation = "relu") %>%
              #layer_batch_normalization() %>%
              layer_dropout(rate = FLAGS$dropout2) %>%
              layer_dense(units = FLAGS$u3, activation = "relu") %>%
              #layer_batch_normalization() %>%
              layer_dropout(rate = FLAGS$dropout3) %>%
              layer_dense(units = FLAGS$u4, activation = "relu") %>%
              #layer_batch_normalization() %>%
              layer_dropout(rate = FLAGS$dropout4) %>%
              layer_dense(units = FLAGS$u5, activation = "relu") %>%
              #layer_batch_normalization() %>%
              layer_dense(units = 2, activation = "softmax")
            
            model_keras2 %>% load_model_weights_hdf5("lp.h5")
            
            observeEvent(input$predict, {
              probback <- model_keras2 %>% predict_proba(dfl)
              predback <- model_keras2 %>% predict_classes(dfl) %>% as.vector()
              probback <- probback[1,2]
              probback <- round(as.numeric(probback) * 100, digits = 1)
              predback <- if(predback > 0.5){print("likely")} else {print("unlikely")}
              
              
              output$probback <- renderPrint(cat("\tThe estimated likelihood of a clinically relevant improvement in back pain is", probback, "%.\n"))
              output$predback <- renderPrint(cat("\tBased on the deep learning model, it is", predback, "that there will be a clinically relevant improvement in back pain at 1 year postoperatively.\n"))
              
      
              
              
            }) 
           
              #ODI
              FLAGS <- tfruns::flags(
                flag_numeric('dropout1', 0.8, 'drop1'),
                flag_numeric('dropout2', 0.5, "drop2"),
                flag_numeric("dropout3", 0, "drop3"),
                flag_numeric("dropout4", 0, "drop4"),
                flag_integer('u1', 256, 'Units1'),
                flag_integer("u2", 128, "units2"),
                flag_integer("u3", 64, "units3"),
                flag_integer("u4", 32, "units4"),
                flag_integer("u5", 4, "units5"),
                flag_integer("batch", 5, "batchsize"),
                flag_integer("epoch", 120, "epochs"),
                flag_numeric("alpha", 0.002, "learningrate")
              )
              model_keras <- keras_model_sequential()
              model_keras %>% 
                layer_dense(units = FLAGS$u1, activation = "relu", input_shape = c(21)) %>%
                #layer_batch_normalization() %>%
                layer_dropout(rate = FLAGS$dropout1) %>%
                layer_dense(units = FLAGS$u2, activation = "relu") %>%
                #layer_batch_normalization() %>%
                layer_dropout(rate = FLAGS$dropout2) %>%
                layer_dense(units = FLAGS$u3, activation = "relu") %>%
                #layer_batch_normalization() %>%
                layer_dropout(rate = FLAGS$dropout3) %>%
                layer_dense(units = FLAGS$u4, activation = "relu") %>%
                #layer_batch_normalization() %>%
                layer_dropout(rate = FLAGS$dropout4) %>%
                layer_dense(units = FLAGS$u5, activation = "relu") %>%
                #layer_batch_normalization() %>%
                layer_dense(units = 2, activation = "softmax")
              
              model_keras %>% load_model_weights_hdf5("lp.h5")
              
              observeEvent(input$predict, {
                probodi <- model_keras %>% predict_proba(dfl)
                predodi <- model_keras %>% predict_classes(dfl) %>% as.vector()
                probodi <- probodi[1,2]
                probodi <- round(as.numeric(probodi) * 100, digits = 1)
                predodi <- if(predodi > 0.5){print("likely")} else {print("unlikely")}
                
                
                output$probodi <- renderPrint(cat("\tThe estimated likelihood of a clinically relevant improvement in functional disability is", probodi, "%.\n"))
                output$predodi <- renderPrint(cat("\tBased on the deep learning model, it is", predodi, "that there will be a clinically relevant improvement in functional disability at 1 year postoperatively.\n"))
        
            
    })
  })
})

