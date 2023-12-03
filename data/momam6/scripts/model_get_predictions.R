library(stringr)
library(purrr)
library(ggplot2)
library(forcats) #for releveling factors
library(reshape2)
library(dplyr)
library(magrittr)
library(readr)


to_predict <- read_csv("momam6_predictions.csv")

add_predictions <- function(method_name,var_name){
  col_nums <- seq(1:nrow(to_predict))
  
  if (!exists("pie_prob")) {
    predictions <- predict(gbmFit,newdata=to_predict,type="prob") %>%
      use_series("pie")
    
    pie_prob <- data.frame(predictions) %>%
      t() %>%
      as.data.frame() %>%
      type_convert() %>%
      `colnames<-`(str_c("round_",col_nums)) %>%
      cbind(method="Stochastic Gradient Boosting") %>%
      as_tibble() %>%
      relocate(method)
  }
  
  #Not all of the models have predictions for their probabilities
  tryCatch({
    #For some dumb reason, some models (adaboost), return ridiculous numbers
    #in place of probabilities. This detects any such anomalies and
    #standardizes them.
    if(any(predict(var_name,newdata=to_predict,type="prob") > 1) || any(predict(var_name,newdata=to_predict,type="prob") < 0)){
      new_prediction <- predict(var_name,newdata=to_predict,type="prob") %>%
        use_series("pie") %>%
        subtract(min(.)) %>%
        divide_by(max(.)-min(.))
    }
    else{
      new_prediction <- predict(var_name,newdata=to_predict,type="prob") %>%
        use_series("pie")
    }
  },
  #Makes the prediction 1 if pie, 0 if spike.
    error=function(e){
      #Error functions use a different environment for some stupid reason,
      #so this needs to be assigned to the gloabl env.
      new_prediction <<- predict(var_name,newdata=to_predict) %>%
      as_tibble() %>%
      mutate(value=case_when(value=="spike"~0,
                             T~1)) %>%
      unlist()}
  )
  
  new_row <- data.frame(new_prediction) %>%
    t() %>%
    as.data.frame() %>% #Don't ask why it needs to be made a data frame again
    type_convert() %>%
    `colnames<-`(str_c("round_",col_nums)) %>%
    cbind(method=method_name) %>%
    as_tibble() %>%
    relocate(method)
  
  rbind(pie_prob,new_row) %>%
    return()
}

init_new_columns <- function(df){
  df %>%
    mutate(correct = 0,
           score = 0) %>%
    return()
}

#Add predictions for each of the trained models.
pie_prob <- add_predictions("Adjacent Categories Probability Model for Ordinal Data",adjcatFit)
pie_prob <- add_predictions("Bagged CART",adabagFit)
pie_prob <- add_predictions("Bagged MARS",bagMARSFit)
pie_prob <- add_predictions("Bayesian Generalized Linear Model",bayesglmFit)
pie_prob <- add_predictions("Boosted Classification Trees",classtreesFit)
pie_prob <- add_predictions("Boosted Generalized Linear Model",glmboostFit)
pie_prob <- add_predictions("Boosted Linear Model",bstlmFit)
pie_prob <- add_predictions("Boosted Logistic Regression",logitboostFit)
pie_prob <- add_predictions("Boosted Tree",bsttreeFit)
pie_prob <- add_predictions("C4.5-like Trees",cliketreeFit)
pie_prob <- add_predictions("C5.0",c5Fit)
pie_prob <- add_predictions("CART",cartFit)
pie_prob <- add_predictions("Conditional Inference Random Forest",infertreeFit)
pie_prob <- add_predictions("Conditional Inference Tree",condinfertreeFit)
pie_prob <- add_predictions("Conditional Inference Tree 2",condinfertree2Fit)
pie_prob <- add_predictions("Continuation Ratio Model for Ordinal Data",ratiomodelFit)
pie_prob <- add_predictions("Cost-Sensitive C5.0",costc5Fit)
pie_prob <- add_predictions("Cost-Sensitive CART",costcartFit)
pie_prob <- add_predictions("Cumulative Probability Model for Ordinal Data",vglmCumulativeFit)
pie_prob <- add_predictions("DeepBoost",deepboostFit)
pie_prob <- add_predictions("Distance Weighted Discrimination with Radial Basis Function Kernel",dwdRadialFit)
pie_prob <- add_predictions("eXtreme Gradient Boosting Tree",xgbTreeFit)
pie_prob <- add_predictions("eXtreme Gradient Boosting Linear",xgbLinearFit)
pie_prob <- add_predictions("Factor-Based Linear Discriminant Analysis",RFldaFit)
pie_prob <- add_predictions("Flexible Discriminant Analysis",fdaFit)
pie_prob <- add_predictions("Generalized Additive Model using LOESS",gamLoessFit)
pie_prob <- add_predictions("Generalized Linear Model",glmFit)
pie_prob <- add_predictions("Generalized Linear Model with Stepwise Feature Selection",glmStepAICFit)
pie_prob <- add_predictions("glmnet",glmnetFit)
pie_prob <- add_predictions("Greedy Prototype Selection",protoclassFit)
pie_prob <- add_predictions("Heteroscedastic Discriminant Analysis",hdaFit)
pie_prob <- add_predictions("High-Dimensional Regularized Discriminant Analysis",hddaFit)
pie_prob <- add_predictions("K-Nearest Neighbors",kknnFit)
pie_prob <- add_predictions("L2 Regularized Linear Support Vector Machines with Class Weights",svmLinearWeights2Fit)
pie_prob <- add_predictions("L2 Regularized Support Vector Machine (dual) with Linear Kernel",svmLinear3Fit)
pie_prob <- add_predictions("Learning Vector Quantization",lvqFit)
pie_prob <- add_predictions("Linear Discriminant Analysis",ldaFit)
pie_prob <- add_predictions("Linear Discriminant Analysis with Stepwise Feature Selection",stepLDAFit)
pie_prob <- add_predictions("Linear Distance Weighted Discrimination",dwdLinearFit)
pie_prob <- add_predictions("Linear Support Vector Machines with Class Weights",svmlinearFit)
pie_prob <- add_predictions("Logistic Model Trees",LMTFit)
pie_prob <- add_predictions("Maximum Uncertainty Linear Discriminant Analysis",mldaFit)
pie_prob <- add_predictions("Model Averaged Neural Network",avNNetFit)
pie_prob <- add_predictions("Monotone Multi-Layer Perceptron Neural Network",monmlpFit)
pie_prob <- add_predictions("Multi-Layer Perceptron",mlpFit)
pie_prob <- add_predictions("Multi-Layer Perceptron with Weight Decay",mlpWeightDecayFit)
pie_prob <- add_predictions("Multi-Layer Perceptron with Weight Decay and multiple layers",mlpWeightDecayMLFit)
pie_prob <- add_predictions("Multi-Step Adaptive MCP-Net",msaenetFit)
pie_prob <- add_predictions("Multilayer Perceptron Network with Dropout",mlpKerasDropoutFit)
pie_prob <- add_predictions("Naive Bayes",naivebayesFit)
pie_prob <- add_predictions("Nearest Shrunken Centroids",nscFit)
pie_prob <- add_predictions("Neural network",nnetFit)
pie_prob <- add_predictions("Neural network with Feature Extraction",pcaNNetFit)
pie_prob <- add_predictions("Non-Informative Model",nullFit)
pie_prob <- add_predictions("Optimal Weighted Nearest Neighbor",ownnFit)
pie_prob <- add_predictions("Parallel Random Forest",parRFFit)
pie_prob <- add_predictions("Partial Least Squares Generalized Linear Model",plsglmFit)
pie_prob <- add_predictions("Penalized Linear Discriminant Analysis",pdaFit)
pie_prob <- add_predictions("Penalized Logistic Regression",plrFit)
pie_prob <- add_predictions("Penalized Multinomial Regression",multinomFit)
pie_prob <- add_predictions("adaboost",adaFit)
pie_prob <- add_predictions("Random Ferns",rFernsFit)
pie_prob <- add_predictions("Random Forest",rangerFit)
pie_prob <- add_predictions("Random Forest by Randomization",extraTreesFit)
pie_prob <- add_predictions("Random Forest Rule-Based Model",rfRulesFit)
pie_prob <- add_predictions("Regularized Logistic Regression",regLogistic)
pie_prob <- add_predictions("Regularized Random Forests",RRF)
pie_prob <- add_predictions("Robust Linear Discriminant Analysis",LindaFit)
pie_prob <- add_predictions("Robust SIMCA",RSimcaFit)
pie_prob <- add_predictions("ROC-Based Classifier",roccFit)
pie_prob <- add_predictions("Rotation Forest",rotationForestFit)
pie_prob <- add_predictions("Rule-Based Classifier",JRipFit)
pie_prob <- add_predictions("Self-Organizing Maps",xyfFit)
pie_prob <- add_predictions("Shrinkage Discriminant Analysis",sdaFit)
pie_prob <- add_predictions("SIMCA",CSimcaFit)
pie_prob <- add_predictions("Single C5.0 Ruleset",C5.0RulesFit)
pie_prob <- add_predictions("Single C5.0 Tree ",C5.0TreeFit)
pie_prob <- add_predictions("Single Rule Classification",OneRFit)
pie_prob <- add_predictions("Sparse Distance Weighted Discrimination",sdwdFit)
pie_prob <- add_predictions("Sparse Linear Discriminant Analysis",sparseLDAFit)
pie_prob <- add_predictions("Sparse Partial Least Squares",splsFit)
pie_prob <- add_predictions("Stabilized Linear Discriminant Analysis",sldaFit)
pie_prob <- add_predictions("Stabilized Nearest Neighbor Classifier",snnFit)
pie_prob <- add_predictions("Stacked AutoEncoder Deep Neural Network",dnnFit)
pie_prob <- add_predictions("Tree Models from Genetic Algorithms",evtreeFit)
pie_prob <- add_predictions("Tree-Based Ensembles",nodeHarvestFit)
pie_prob <- add_predictions("Weighted Subspace Random Forest",wsrfFit)
pie_prob <- add_predictions("Gaussian Process",gaussprLinearFit)
pie_prob <- add_predictions("Gaussian Process with Polynomial Kernal",gaussprPolyFit)
pie_prob <- add_predictions("Gaussian Process with Radial Basis Function Kernal",gaussprRadialFit)
pie_prob <- add_predictions("Oblique Random Forest",ORFlogFit)
pie_prob <- add_predictions("avnnet with random hyperparameter search",avNNetROC)
pie_prob <- add_predictions("Keras Dropout with random hyperparameter search",mlpKerasDropoutROC)
pie_prob <- add_predictions("Conditional Inference Tree 2 with Random Hyperparameter search",ctree2ROC)
pie_prob <- type_convert(pie_prob)
#These make the columns into character vectors for some reason.
pie_prob <- rbind(pie_prob,c("Model Ensemble: Mean",apply(pie_prob[,-1],2,mean))) %>% type_convert()
pie_prob <- rbind(pie_prob,c("Model Ensemble: Median",apply(pie_prob[,-1],2,median))) %>% type_convert()
pie_prob <- rbind(pie_prob,c("Good Model Ensemble: Mean",apply(good_models[,c(2:32)],2,mean))) %>% type_convert()
pie_prob <- rbind(pie_prob,c("Good Model Ensemble: Mean",apply(good_models[,c(2:32)],2,median))) %>% type_convert()


pie_prob <- init_new_columns(pie_prob)


write_csv(pie_prob,"final_model_predictions_momam6.csv")