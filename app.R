# Define packages list
pkgs <- c(
  "ada",
  "reshape2", 
  "scales", 
  "ggplot2",
  "caret",
  "naivebayes",
  "doParallel",
  "dplyr",
  "DT",
  "e1071",
  "fastshap",      # Added for fastshap
  "gbm",
  "ggplot2",
  "glmnet",
  "klaR",
  "kernlab",
  "Metrics",
  "MLmetrics",
  "plotROC",
  "pROC",
  "randomForest",
  "recipes",
  "reshape2",
  "rpart",
  "rsample",
  "shiny",
  "tidyr",
  "xgboost"
)

# Install packages
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load packages
for (pkg in pkgs) {
  library(pkg, character.only = TRUE)
}

# Define UI interface
ui <- fluidPage(
  titlePanel("SMART-pred"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV file", accept = c(".csv")),
      checkboxInput("show_descriptive_stats", "Descriptive Statistics", value = FALSE),
      conditionalPanel(
        condition = "input.show_descriptive_stats == true",
        uiOutput("numeric_columns_ui"),  
        uiOutput("character_columns_ui"),  
        uiOutput("numeric_stats_ui"),  
        uiOutput("character_stats_ui"),  
        actionButton("calculate_stats", "Calculate")
      ),
      selectInput("task_type", "Select Task Type", choices = c("Classification", "Regression")),
      uiOutput("target_var_ui"),
      checkboxInput("select_all_predictors", "Select all independent variables", value = FALSE),
      conditionalPanel(
        condition = "input.select_all_predictors == false",
        uiOutput("predictor_vars_ui")
      ),
      checkboxInput("Variable_importance", "Variable Importance", value = FALSE),
      checkboxInput("shap_analysis", "SHAP Analysis", value = FALSE),
      uiOutput("model_selection_ui"),
      actionButton("runButton", "Run")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("data_preview")),
        tabPanel("Descriptive Statistics", DTOutput("descriptive_stats")),
        tabPanel("Model Summary", verbatimTextOutput("model_summary")),
        tabPanel("Validation Performance Metrics", tableOutput("val_performance_metrics")),
        tabPanel("Test Performance Metrics", tableOutput("test_performance_metrics")),
        tabPanel("ROC Curve", plotOutput("roc_curves", height = "480px", width = "155%")),
        tabPanel("Variable Importance", plotOutput("varimp", height = "480px", width = "155%")),
        tabPanel("SHAP Analysis", uiOutput("shap_plot"))  # Dynamic UI for multiple SHAP plots
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Store error or model results 
  rv <- reactiveValues(
    results = list(),  
    model_errors = list()  
  )  
  results <- reactiveValues(
    results = list()
  )
  
  # ReactiveValues to save full ROC objects computed in test performance metrics
  test_roc_objects <- reactiveValues(roc = list())  # <--- Added for storing ROC objects
  
  # Data loader
  dataset <- reactive({  
    req(input$datafile)    
    data <- read.csv(input$datafile$datapath, header = TRUE)
    
    updateSelectInput(session, "target_var",   
                      choices = names(data),   
                      selected = names(data)[length(names(data))]) 
    return(data)   
  })  
  
  numeric_cols <- reactive({  
    req(dataset())  
    names(dataset())[sapply(dataset(), is.numeric)]  
  })  
  
  character_cols <- reactive({  
    req(dataset())  
    names(dataset())[sapply(dataset(), function(col) is.character(col) || is.factor(col))]  
  })  
  
  # Select target variable
  output$target_var_ui <- renderUI({
    req(dataset())
    selectInput("target_var", "Select Target Variable", choices = names(dataset()))
  })
  
  # Select Independent variable
  output$predictor_vars_ui <- renderUI({
    req(dataset())
    selectInput("predictor_vars", "Select Independent Variable", choices = names(dataset()), multiple = TRUE)
  })
  
  # Show data preview
  output$data_preview <- renderDT({
    req(dataset())
    datatable(head(dataset(), 50))
  })
  
  # Choose numeric data
  output$numeric_columns_ui <- renderUI({  
    req(numeric_cols())  
    selectInput("numeric_columns", "Select Numeric Variable", choices = numeric_cols(), multiple = TRUE)  
  })  
  
  # Choose character data
  output$character_columns_ui <- renderUI({  
    req(character_cols())  
    selectInput("character_columns", "Select Character Variable", choices = character_cols(), multiple = TRUE)  
  })  
  
  # Choose numeric statistics
  output$numeric_stats_ui <- renderUI({  
    selectInput("numeric_stats", "Select Numeric Statistics", choices = c("mean", "std", "min", "median", "max"), multiple = TRUE)  
  })  
  
  # Choose character statistics 
  output$character_stats_ui <- renderUI({  
    selectInput("character_stats", "Select Character Statistics", choices = c("mode", "unique_count"), multiple = TRUE)  
  })  
  
  # Descriptive statistics interface
  output$descriptive_columns_ui <- renderUI({
    req(dataset())
    selectInput("Descriptive Statistics", "Variables", choices = names(dataset()), multiple = TRUE)
  })
  
  # update data frame 
  observeEvent(input$target_var, {  
    req(dataset())    
    data <- dataset()  
    
    # Select all variables except for the target variable
    predictors <- setdiff(names(data), input$target_var)  
    updateSelectInput(session, "predictor_vars", choices = names(data), selected = predictors)  
  }) 
  
  # Calculate and display descriptive statistics
  observeEvent(input$calculate_stats, {  
    req(dataset())  
    data <- dataset()  
    selected_numeric_cols <- as.character(input$numeric_columns)  
    selected_character_cols <- as.character(input$character_columns)  
    selected_numeric_stats <- input$numeric_stats  
    selected_character_stats <- input$character_stats  
    
    stats_list <- list()  
    
    # Calculate statistics for numeric columns.
    if (!is.null(selected_numeric_cols) && !is.null(selected_numeric_stats)) {  
      numeric_stats <- data %>%  
        dplyr::select(all_of(selected_numeric_cols)) %>%  
        summarise(across(everything(), list(  
          mean = ~ mean(., na.rm = TRUE),  
          std = ~ sd(., na.rm = TRUE),  
          min = ~ min(., na.rm = TRUE),  
          median = ~ median(., na.rm = TRUE),  
          max = ~ max(., na.rm = TRUE)  
        ), .names = "{.col}__{.fn}"))  
      
      numeric_stats_long <- numeric_stats %>%  
        pivot_longer(  
          cols = everything(),  
          names_to = c("Variable", "Statistic"),  
          names_sep = "__",  
          values_to = "Value",  
          values_transform = list(Value = as.character)  
        )  
      
      # Filter selected statistics
      numeric_stats_filtered <- numeric_stats_long %>%  
        filter(Statistic %in% selected_numeric_stats)  
      
      stats_list[[1]] <- numeric_stats_filtered  
    }
    
    # Calculate statistics for character columns
    if (!is.null(selected_character_cols) && !is.null(selected_character_stats)) {  
      character_stats <- data %>%  
        dplyr::select(all_of(selected_character_cols)) %>%  
        summarise(across(everything(), list(  
          mode = ~ names(sort(table(.), decreasing = TRUE))[1],  
          unique_count = ~ length(unique(.))  
        ), .names = "{.col}__{.fn}"))  
      
      character_stats_long <- character_stats %>%  
        pivot_longer(  
          cols = everything(),  
          names_to = c("Variable", "Statistic"),  
          names_sep = "__",  
          values_to = "Value",  
          values_transform = list(Value = as.character)  
        )  
      
      # Filter selected statistics
      character_stats_filtered <- character_stats_long %>%  
        filter(Statistic %in% selected_character_stats)  
      
      stats_list[[2]] <- character_stats_filtered  
    }
    
    if (length(stats_list) > 0) {  
      combined_stats <- bind_rows(stats_list) 
      
      # If needed, convert the value of the numeric statistic back to a numeric type
      numeric_statistics <- c("mean", "std", "min", "median", "max")  
      combined_stats <- combined_stats %>%  
        mutate(Value = ifelse(Statistic %in% numeric_statistics, as.numeric(Value), Value))  
      
      # Convert to wide format for display 
      stats_wide <- combined_stats %>%  
        pivot_wider(  
          names_from = "Statistic",  
          values_from = "Value"  
        )  
      
      # Arrange the columns in the order of the selected statistics 
      all_selected_stats <- c(selected_numeric_stats, selected_character_stats)  
      stats_wide <- stats_wide %>%  
        dplyr::select(Variable, all_of(all_selected_stats))  
      output$descriptive_stats <- renderDT({  
        datatable(stats_wide)  
      })  
    } else {  
      output$descriptive_stats <- renderDT({  
        datatable(data.frame())  
      })  
    }
  })
  
  # Model selection interface
  output$model_selection_ui <- renderUI({
    if (input$task_type == "Classification") {
      checkboxGroupInput("models", "Select Classification Model", 
                         choices = c("Logistic Regression", "Naive Bayes", "KNN", "Decision Tree", "Random Forest", "Gradient Boosting", "AdaBoost", "XGBoost", "SVM", "Neural Network"))
    } else {
      checkboxGroupInput("models", "Select Regression Model", 
                         choices = c("Linear Regression", "Lasso Regression", "Ridge Regression", "Decision Tree", "Random Forest", "Gradient Boosting", "AdaBoost", "XGBoost", "SVM", "Neural Network"))
    }
  })
  
  # Operate analysis
  observeEvent(input$runButton, {
    data <- dataset()
    target <- input$target_var
    predictors <- input$predictor_vars
    models <- input$models
    
    req(target)
    req(length(predictors) > 0)
    req(length(target) == 1)
    predictors <- setdiff(predictors, target)
    all_data <- c(input$target_var, predictors)
    
    # Construct the formula
    formula <- as.formula(paste(target, "~", "."))
    
    # Split dataset
    set.seed(123)
    if (input$task_type == "Classification") {
      split <- initial_split(data[all_data], prop = 0.7, strata = target)
    } else {
      split <- initial_split(data[all_data], prop = 0.7)
    }
    train <- training(split)
    test <- testing(split)
    
    # If it's a classification task, make sure the target variable is a factor type
    if (input$task_type == "Classification") {
      train[[target]] <- as.factor(train[[target]])
      test[[target]] <- as.factor(test[[target]])
    }
    
    # number of comp
    pca_model <- prcomp(train[, sapply(train, is.numeric)], scale. = TRUE)
    explained_variance <- pca_model$sdev^2 / sum(pca_model$sdev^2)
    cumulative_variance <- cumsum(explained_variance)
    num_pcs <- which(cumulative_variance >= 0.95)[1]
    
    # Data preprocess
    blueprint <- recipe(formula, data = train) %>%
      step_impute_mean(all_numeric_predictors()) %>%
      step_impute_mode(all_nominal_predictors()) %>%
      step_nzv(all_predictors()) %>%
      step_corr(all_numeric_predictors(), threshold = 0.9) %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors())  %>%
      step_pca(all_numeric(), -all_outcomes(), num_comp = num_pcs)
    
    # Training control
    if (input$task_type == "Classification") {
      cv <- trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 5,
        summaryFunction = twoClassSummary,
        classProbs = TRUE,
        savePredictions = TRUE,
        verboseIter = TRUE,
        allowParallel = TRUE
      )
      metric <- "ROC"
    } else {
      cv <- trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 5,
        savePredictions = TRUE
      )
      metric <- "RMSE"
    }
    
    rv$results <- list()
    rv$model_errors <- list()
    
    # Enable parallel processing
    registerDoParallel(4)
    
    # Model training
    for (model_name in models) {
      set.seed(123)
      tryCatch({
        if (input$task_type == "Classification") {
          if (model_name == "Logistic Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "glm",
              family = binomial,
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Naive Bayes") {
            model <- train(
              blueprint,
              data = train,
              method = "naive_bayes",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "KNN") {
            model <- train(
              blueprint,
              data = train,
              method = "knn",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Decision Tree") {
            model <- train(
              blueprint,
              data = train,
              method = "rpart",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Random Forest") {
            model <- train(
              blueprint,
              data = train,
              method = "rf",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Gradient Boosting") {
            model <- train(
              blueprint,
              data = train,
              method = "gbm",
              trControl = cv,
              metric = metric,
              verbose = FALSE
            )
          } else if (model_name == "AdaBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "ada",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "XGBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "xgbTree",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "SVM") {
            model <- train(
              blueprint,
              data = train,
              method = "svmRadial",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Neural Network") {
            model <- train(
              blueprint,
              data = train,
              method = "nnet",
              trControl = cv,
              metric = metric,
              trace = FALSE
            )
          }
        } else {
          if (model_name == "Linear Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "lm",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Lasso Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "glmnet",
              trControl = cv,
              tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, by = 0.0002)),
              metric = metric
            )
          } else if (model_name == "Ridge Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "glmnet",
              trControl = cv,
              tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, by = 0.0002)),
              metric = metric
            )
          } else if (model_name == "Decision Tree") {
            model <- train(
              blueprint,
              data = train,
              method = "rpart",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Random Forest") {
            model <- train(
              blueprint,
              data = train,
              method = "rf",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Gradient Boosting") {
            model <- train(
              blueprint,
              data = train,
              method = "gbm",
              trControl = cv,
              metric = metric,
              verbose = FALSE
            )
          } else if (model_name == "AdaBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "ada",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "XGBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "xgbTree",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "SVM") {
            model <- train(
              blueprint,
              data = train,
              method = "svmRadial",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Neural Network") {
            model <- train(
              blueprint,
              data = train,
              method = "nnet",
              trControl = cv,
              metric = metric,
              trace = FALSE
            )
          }
        }
        rv$results[[model_name]] <- model
      }, error = function(e) {
        rv$model_errors[[model_name]] <- e$message
      })
    }
    
    # Show model summary
    output$model_summary <- renderPrint({
      req(rv$results)
      if (length(rv$results) == 0) {
        cat("No successfully trained model.\n")
      } else {
        lapply(rv$results, function(model) {
          cat("Model:", model$method, "\n")
          print(model)
          cat("\n---------------------------------------\n")
        })
      }
      if (length(rv$model_errors) > 0) {
        cat("The following model training encountered an error:\n")
        print(rv$model_errors)
      }
    })
    
    # Calculate and display the performance metrics of the validation set
    output$val_performance_metrics <- renderTable({
      req(rv$results)
      if (length(rv$results) == 0) {
        return(NULL)
      }
      metrics <- lapply(names(rv$results), function(model_name) {
        model <- rv$results[[model_name]]
        if (input$task_type == "Classification") {
          predictions <- model$pred
          
          # Make sure the factor levels for predictions and actual values are consistent
          predictions$pred <- factor(predictions$pred, levels = levels(predictions$obs))
          conf_mat <- confusionMatrix(predictions$pred, predictions$obs)
          accuracy_ci_lower <- conf_mat$overall["AccuracyLower"]
          accuracy_ci_upper <- conf_mat$overall["AccuracyUpper"]
          
          # Calculate the confusion matrix.
          accuracy <- conf_mat$overall['Accuracy']
          ci <- sprintf("(%.3f-%.3f)", accuracy_ci_lower, accuracy_ci_upper)
          sensitivity <- conf_mat$byClass['Sensitivity']
          precision <- conf_mat$byClass['Precision']
          specificity <- conf_mat$byClass['Specificity']
          f1_score <- conf_mat$byClass['F1']
          roc <- max(model$results$ROC, na.rm = TRUE)
          if (!is.null(model$bestTune)) {
            param_str <- paste(names(model$bestTune), model$bestTune, sep = "=", collapse = ", ")
          } else {
            param_str <- "Default parameters"
          }
          data.frame(
            Model = model$method,
            Accuracy = accuracy,
            `Accuracy (95% CI)` = ci,
            Recall = sensitivity,
            Precision = precision,
            Specificity = specificity,
            F1_Score = f1_score,
            ROC = roc,
            Best_Params = param_str,
            stringsAsFactors = FALSE,
            check.names=FALSE)
        } else {
          
          # Calculation of performance metrics for regression tasks
          predictions <- model$pred
          
          # Calculate regression metrics
          mae <- mean(abs(predictions$pred - predictions$obs))
          mse <- mean((predictions$pred - predictions$obs)^2)
          rmse <- sqrt(mse)
          r_squared <- 1 - (sum((predictions$obs - predictions$pred)^2) / sum((predictions$obs - mean(predictions$obs))^2))
          if (!is.null(model$bestTune)) {
            param_str <- paste(names(model$bestTune), model$bestTune, sep = "=", collapse = ", ")
          } else {
            param_str <- "Default parameters"
          }
          data.frame(
            Model = model$method,
            MAE = mae,
            MSE = mse,
            RMSE = rmse,
            R_Squared = r_squared,
            Best_Params = param_str,
            stringsAsFactors = FALSE)
        }
      })
      do.call(rbind, metrics)
    }, rownames = FALSE)
    
    # Calculate and display performance metrics for the test set
    output$test_performance_metrics <- renderTable({
      req(rv$results)
      if (length(rv$results) == 0) {
        return(NULL)
      }
      metrics <- lapply(names(rv$results), function(model_name) {
        model <- rv$results[[model_name]]
        if (input$task_type == "Classification") {
          # Predict probabilities on test set for positive class
          prediction <- predict(model, newdata = test, type = "prob")
          preds <- prediction[, 2]
          
          # Compute ROC object for test set and save for plotting
          roc_obj <- pROC::roc(test[[target]], preds, levels = rev(levels(test[[target]])))
          test_roc_objects$roc[[model_name]] <- roc_obj  # <--- Save full ROC object here
          auc_val <- pROC::auc(roc_obj)
          
          # Predict classes on test set
          pred_classes <- predict(model, newdata = test)
          pred_classes <- factor(pred_classes, levels = levels(test[[target]]))
          conf_mat <- confusionMatrix(pred_classes, test[[target]])
          accuracy_ci_lower <- conf_mat$overall["AccuracyLower"]
          accuracy_ci_upper <- conf_mat$overall["AccuracyUpper"]
          
          accuracy <- conf_mat$overall['Accuracy']
          ci <- sprintf("(%.3f-%.3f)", accuracy_ci_lower, accuracy_ci_upper)
          sensitivity <- conf_mat$byClass['Sensitivity']
          precision <- conf_mat$byClass['Precision']
          specificity <- conf_mat$byClass['Specificity']
          f1_score <- conf_mat$byClass['F1']
          if (!is.null(model$bestTune)) {
            param_str <- paste(names(model$bestTune), model$bestTune, sep = "=", collapse = ", ")
          } else {
            param_str <- "Default parameters"
          }
          data.frame(
            Model = model$method,
            Accuracy = accuracy,
            `Accuracy (95% CI)` = ci,
            Recall = sensitivity,
            Precision = precision,
            Specificity = specificity,
            F1_Score = f1_score,
            ROC = as.numeric(auc_val),
            Best_Params = param_str,
            stringsAsFactors = FALSE,
            check.names=FALSE)
        } else {
          # Calculation of performance metrics for regression tasks
          prediction <- predict(model, newdata = test)
          predictions <- data.frame(pred = prediction, obs = test[[target]])
          
          # Calculate regression metrics
          mae <- mean(abs(predictions$pred - predictions$obs))
          mse <- mean((predictions$pred - predictions$obs)^2)
          rmse <- sqrt(mse)
          r_squared <- 1 - (sum((predictions$obs - predictions$pred)^2) / sum((predictions$obs - mean(predictions$obs))^2))
          if (!is.null(model$bestTune)) {
            param_str <- paste(names(model$bestTune), model$bestTune, sep = "=", collapse = ", ")
          } else {
            param_str <- "Default parameters"
          }
          data.frame(
            Model = model$method,
            MAE = mae,
            MSE = mse,
            RMSE = rmse,
            R_Squared = r_squared,
            Best_Params = param_str,
            stringsAsFactors = FALSE)
        }
      })
      do.call(rbind, metrics)
    }, rownames = FALSE)
    
    # ROC Curve - use saved ROC objects directly for plotting and exact AUC display
    output$roc_curves <- renderPlot({
      req(rv$results)
      if (input$task_type == "Classification") {
        if (length(rv$results) == 0) return(NULL)
        
        colors <- rainbow(length(rv$results))
        legend_text <- character(length(rv$results))
        
        for (i in seq_along(rv$results)) {
          model_name <- names(rv$results)[i]
          
          # Use saved ROC object (no recalculation)
          roc_obj <- test_roc_objects$roc[[model_name]]
          if (is.null(roc_obj)) next  # Skip if missing
          
          if (i == 1) {
            plot(roc_obj,
                 col = colors[i],
                 lwd = 2,
                 asp = NA,
                 xlab = "False Positive Rate (FPR)",
                 ylab = "True Positive Rate (TPR)",
                 cex.axis = 1.5,
                 cex.lab = 1.5,
                 legacy.axes = TRUE)
          } else {
            lines(roc_obj, col = colors[i], lwd = 2)
          }
          
          auc_val <- pROC::auc(roc_obj)
          legend_text[i] <- sprintf("%s (AUC: %.2f)", model_name, auc_val)
        }
        
        legend("bottomright",
               col = colors,
               lty = 1,
               legend = legend_text,
               cex = 0.95)
        
        # Optional print for debug
        print(legend_text)
      }
    })
    
    
    
    # Variable importance
    if(input$Variable_importance){
      data <- dataset()
      target <- input$target_var
      predictors <- input$predictor_vars
      models <- input$models
      
      req(target)
      req(length(predictors) > 0)
      req(length(target) == 1)
      predictors <- setdiff(predictors, target)
      
      # Construct the formula
      formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
      set.seed(123)
      if (input$task_type == "Classification") {
        split <- initial_split(data, prop = 0.7, strata = target)
      } else {
        split <- initial_split(data, prop = 0.7)
      }
      train <- training(split)
      test <- testing(split)
      
      # If it's a classification task, make sure the target variable is a factor type
      if (input$task_type == "Classification") {
        train[[target]] <- as.factor(train[[target]])
        test[[target]] <- as.factor(test[[target]])
      }
      
      # Data preprocessing
      blueprint <- recipe(formula, data = train) %>%
        step_impute_mean(all_numeric_predictors()) %>%
        step_impute_mode(all_nominal_predictors()) %>%
        step_nzv(all_predictors()) %>%
        step_corr(all_numeric_predictors(), threshold = 0.9) %>%
        step_center(all_numeric_predictors()) %>%
        step_scale(all_numeric_predictors())
      
      # Training control
      if (input$task_type == "Classification") {
        cv <- trainControl(
          method = "repeatedcv",
          number = 10,
          repeats = 5,
          summaryFunction = twoClassSummary,
          classProbs = TRUE,
          savePredictions = TRUE,
          verboseIter = TRUE,
          allowParallel = TRUE
        )
        metric <- "ROC"
      } else {
        cv <- trainControl(
          method = "repeatedcv",
          number = 10,
          repeats = 5,
          savePredictions = TRUE
        )
        metric <- "RMSE"
      }
      
      results <- list()
      
      # Enable parallel processing
      registerDoParallel(4)
      
      # Model training
      for (model_name in models) {
        set.seed(123)
        if (input$task_type == "Classification") {
          if (model_name == "Logistic Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "glm",
              family = binomial,
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Naive Bayes") {
            model <- train(
              blueprint,
              data = train,
              method = "naive_bayes",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "KNN") {
            model <- train(
              blueprint,
              data = train,
              method = "knn",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Decision Tree") {
            model <- train(
              blueprint,
              data = train,
              method = "rpart",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Random Forest") {
            model <- train(
              blueprint,
              data = train,
              method = "rf",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Gradient Boosting") {
            model <- train(
              blueprint,
              data = train,
              method = "gbm",
              trControl = cv,
              metric = metric,
              verbose = FALSE
            )
          } else if (model_name == "AdaBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "ada",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "XGBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "xgbTree",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "SVM") {
            model <- train(
              blueprint,
              data = train,
              method = "svmRadial",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Neural Network") {
            model <- train(
              blueprint,
              data = train,
              method = "nnet",
              trControl = cv,
              metric = metric,
              trace = FALSE
            )
          }
        } else {
          if (model_name == "Linear Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "lm",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Lasso Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "glmnet",
              trControl = cv,
              tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, by = 0.0002)),
              metric = metric
            )
          } else if (model_name == "Ridge Regression") {
            model <- train(
              blueprint,
              data = train,
              method = "glmnet",
              trControl = cv,
              tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, by = 0.0002)),
              metric = metric
            )
          } else if (model_name == "Decision Tree") {
            model <- train(
              blueprint,
              data = train,
              method = "rpart",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Random Forest") {
            model <- train(
              blueprint,
              data = train,
              method = "rf",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Gradient Boosting") {
            model <- train(
              blueprint,
              data = train,
              method = "gbm",
              trControl = cv,
              metric = metric,
              verbose = FALSE
            )
          } else if (model_name == "AdaBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "ada",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "XGBoost") {
            model <- train(
              blueprint,
              data = train,
              method = "xgbTree",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "SVM") {
            model <- train(
              blueprint,
              data = train,
              method = "svmRadial",
              trControl = cv,
              metric = metric
            )
          } else if (model_name == "Neural Network") {
            model <- train(
              blueprint,
              data = train,
              method = "nnet",
              trControl = cv,
              metric = metric,
              trace = FALSE
            )
          }
        }
        results[[model_name]] <- model
      }
      
      # Variable importance
      output$varimp <- renderPlot({
        if (length(results) == 0) {
          return(NULL)
        }
        importance_metrics <- lapply(results, function(model) {
          importance <- varImp(model, scale = T)
          importance
        })
        
        # Standardize the column names across all models
        varimp_list <- list()
        for (i in 1:length(importance_metrics)) {
          # Rename "Overall" or "importance" to "importance" for consistency
          varimp_list[[i]] <- importance_metrics[[i]][[1]]
        }
        for (i in 1:length(importance_metrics)) {
          if (all(c("H", "P") %in% names(varimp_list[[i]])) &&
              all(varimp_list[[i]]$H == varimp_list[[i]]$P)) {
            # If H and P are identical, remove P and rename H to Importance
            varimp_list[[i]] <- varimp_list[[i]] %>%
              dplyr::select(-P) %>%
              rename(Importance = H)
          } else if ("Overall" %in% names(varimp_list[[i]])) {
            # If 'Overall' column exists, rename it to Importance
            varimp_list[[i]] <- varimp_list[[i]] %>%
              rename(Importance = Overall)
          }
          varimp_list[[i]]$model <- names(importance_metrics)[i]
        }
        varimp_list <- lapply(varimp_list, function(df){
          df$Variable <- rownames(df)
          df %>%
            slice_max(order_by = Importance, n = 10, with_ties = FALSE)
        })
        varimp_df <- bind_rows(varimp_list)
        write.csv(varimp_df, "varimp_df.csv", row.names = FALSE)
        reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
          new_x <- paste(x, within, sep = sep)
          stats::reorder(new_x, by, FUN = fun)
        }
        scale_x_reordered <- function(..., sep = "___") {
          reg <- paste0(sep, ".+$")
          ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
        }
        model_labels <- setNames(
          paste0("(", letters[1:length(unique(varimp_df$model))], ") ", names(results)),
          names(results)
        )
        
        # Adjust the plot dimensions for a fatter and taller output
        ggplot(varimp_df, aes(x = reorder_within(Variable, -Importance, model), y = Importance, fill = model)) +
          geom_bar(stat = "identity") +
          scale_x_reordered() +
          labs(title = "Top 10 Variable Importance in Each Model",
               x = "Variable",
               y = "Importance") +
          theme_minimal(base_size = 9) +
          theme(text = element_text(face = "bold"),
                axis.text.x = element_text(angle = 13, hjust = 1, size = 9, face = "bold"),
                axis.text.y = element_text(size = 8, face = "bold"),
                strip.text = element_text(size = 9, face = "bold"),
                plot.title = element_text(hjust = 0.5, size = 9, face = "bold"),
                legend.position = "right",
                legend.key.size = unit(0.8,"cm")) +
          facet_wrap(~factor(model,names(results)), scales = "free", ncol = 2, labeller = as_labeller(model_labels))
      })
      
      # SHAP Analysis for multiple models (updated)
      if (input$shap_analysis) {
        output$shap_plot <- renderUI({
          req(results)
          if (length(results) == 0) return(NULL)
          plot_output_list <- lapply(names(results), function(model_name) {
            plotOutput(outputId = paste0("shap_plot_", gsub(" ", "_", model_name)))
          })
          do.call(tagList, plot_output_list)
        })
        
        for (model_name in names(results)) {
          local({
            mn <- model_name  # local copy for closure
            output_id <- paste0("shap_plot_", gsub(" ", "_", mn))
            
            output[[output_id]] <- renderPlot({
              model <- results[[mn]]
              train_x <- train[, predictors, drop = FALSE]
              
              pred_fun <- function(object, newdata) {
                if (input$task_type == "Classification") {
                  pred <- predict(object, newdata, type = "prob")
                  return(pred[, 2])
                } else {
                  pred <- predict(object, newdata)
                  return(pred)
                }
              }
              
              sample_rows <- head(seq_len(nrow(train_x)), 100)
              shap_values <- fastshap::explain(
                object = model,
                X = train_x[sample_rows, , drop = FALSE],
                pred_wrapper = pred_fun,
                nsim = 100
              )
              
              # Compute mean SHAP values with signs preserved
              mean_shap <- colMeans(shap_values)
              
              # Create dataframe
              shap_df <- data.frame(
                Feature = names(mean_shap),
                MeanSHAP = mean_shap
              )
              
              # Order features by absolute mean SHAP value (ascending for plot order)
              shap_df <- shap_df %>%
                mutate(Feature = factor(Feature, levels = shap_df$Feature[order(abs(MeanSHAP))]))
              
              # Color by sign of SHAP value
              shap_df$Color <- ifelse(shap_df$MeanSHAP >= 0, "Positive", "Negative")
              
              ggplot(shap_df, aes(x = Feature, y = MeanSHAP, fill = Color)) +
                geom_col() +
                geom_text(aes(label = ifelse(MeanSHAP >= 0, paste0("+", round(MeanSHAP, 4)), round(MeanSHAP, 4)), 
                              hjust = ifelse(MeanSHAP >= 0, -0.1, 1.1)),
                          position = position_dodge(width = 0.9), size = 3) +
                coord_flip() +
                scale_fill_manual(values = c("Positive" = "#E74C3C", "Negative" = "#3498DB")) +
                labs(
                  title = paste("SHAP Values for", mn),
                  x = "Feature",
                  y = "Mean SHAP value"
                ) +
                theme_minimal(base_size = 14) +
                theme(
                  plot.title = element_text(hjust = 0.5, face = "bold"),
                  legend.title = element_blank(),
                  legend.position = "top"
                )
            })
          })
        }
      } else {
        output$shap_plot <- renderUI({
          plotOutput(NULL)
        })
      }
    }
  })
}

shinyApp(ui = ui, server = server)
