library(stringr)
library(purrr)
library(ggplot2)
library(forcats) #for releveling factors
library(reshape2)
library(dplyr)
library(magrittr)
library(readr)

#Road to momam6
#to_predict <- read_csv("momam6_modelselect.csv")
#Actual momam6
to_predict <- read_csv("momam6_predictions.csv")

rm(pie_prob)

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
    #in place of probabilities. This detects any such anamolies and
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


#In case I look for this later, this is where I evaluate is a string as a variable name!
update_scores <- function(df,winner,round){
  column <- str_c("round_",round)
  
  df %>%
    mutate(correct = case_when((winner=="pie" & df[column] > .5)~correct+1,
                               (winner=="spike" & df[column] < .5)~correct+1,
                               T~correct),
           score = case_when(winner=="pie"~score+eval(as.name(paste(column))),
                             winner=="spike"~score+(1-eval(as.name(paste(column)))))) %>%
    return()
}


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
#pie_prob <- add_predictions("eXtreme Gradient Boosting",xgbDARTFit)
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
#pie_prob <- add_predictions("Multi-Layer Perceptron with multiple layers",mlpMLFit)
pie_prob <- add_predictions("Multi-Layer Perceptron with Weight Decay and multiple layers",mlpWeightDecayMLFit)
pie_prob <- add_predictions("Multi-Step Adaptive MCP-Net",msaenetFit)
pie_prob <- add_predictions("Multilayer Perceptron Network with Dropout",mlpKerasDropoutFit)
#pie_prob <- add_predictions("Multivariate Adaptive Regression Spline",earthFit)
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

names(pie_prob)


apply(good_models[,c(2:32)],2,mean)


write_csv(pie_prob,"final_model_predictions_momam6.csv")
write_csv(pie_prob,"model_predictions_momam6.csv")

pie_prob <- read_csv("model_predictions.csv")
pie_prob <- read_csv("final_model_predictions_momam6.csv")

good_models <- pie_prob %>%
  filter(road2momam6_acc > .7)


apply(pie_prob[,-1],2,mean)


#Road to Momam 6
boxplot(pie_prob[,seq(2,ncol(pie_prob)-3)],ylab="Pie's Predicited Chance of Winning",
        names=c("Shadow","Mario Party 2","PS1:Rental Rando","YGO:FM",
                "SM64 120 Star"))




#Road to MOMAM 6
pie_prob %>%
  magrittr::extract(seq(2,6)) %>%
  melt() %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = case_when(variable=="round_1"~"Shadow the Hedgehog",
                              variable=="round_2"~"Mario Party 2 All Boards",
                              variable=="round_3"~"Pokemon Stadium Rental Rando",
                              variable=="round_4"~"YGO Forbidden Memories",
                              variable=="round_5"~"Super Mario 64 120 Star",
                              T~variable)) %>%
  mutate(variable = fct_relevel(variable,"Shadow the Hedgehog","Mario Party 2 All Boards","Pokemon Stadium Rental Rando",
                                "YGO Forbidden Memories","Super Mario 64 120 Star")) %>%
  ggplot(aes(x=variable,y=value)) +
  geom_boxplot(aes(fill=variable),
               outlier.alpha = .8,
               outlier.size = .8,
               outlier.stroke = 1.2) +
  xlab("Game") +
  ylab("Pie's Predicted Chance of Winning") +
  ggtitle("Road to MOMAM6 Predictions with Various Models") +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(angle=9,vjust=.75))
  



#MOMAM 6
pie_prob %>%
  #good_models %>%
  magrittr::extract(seq(2,ncol(pie_prob)-3)) %>%
  melt() %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = case_when(variable=="round_1"~"MP3",
                              variable=="round_2"~"Pikmin 3",
                              variable=="round_3"~"Ape Esc",
                              variable=="round_4"~"BfBB",
                              variable=="round_5"~"YGO:DotR",
                              variable=="round_6"~"PokeShip",
                              variable=="round_7"~"PokeTCG",
                              variable=="round_8"~"Rocket",
                              variable=="round_9"~"M:ZM",
                              variable=="round_10"~"MM2MOMAM",
                              variable=="round_11"~"SMB35",
                              variable=="round_12"~"BotW",
                              variable=="round_13"~"SADX",
                              variable=="round_14"~"MK8Sub",
                              variable=="round_15"~"MK:DD",
                              variable=="round_16"~"BeatEm",
                              variable=="round_17"~"MG:TT",
                              variable=="round_18"~"MP3CPU",
                              variable=="round_19"~"SM3DWBF",
                              variable=="round_20"~"Risk 2",
                              variable=="round_21"~"LMGCN",
                              variable=="round_22"~"Elmo",
                              variable=="round_23"~"OoTSub",
                              variable=="round_24"~"CVNES",
                              variable=="round_25"~"WL4",
                              variable=="round_26"~"MvsDK",
                              variable=="round_27"~"SMRPG",
                              variable=="round_28"~"SMAS",
                              variable=="round_29"~"GoldPig",
                              variable=="round_30"~"Undertale",
                              variable=="round_31"~"PS2RR",
                              T~variable)) %>%
  mutate(variable = fct_relevel(variable,"MP3","Pikmin 3","Ape Esc","BfBB",
                                "YGO:DotR","PokeShip","PokeTCG","Rocket","M:ZM",
                                "MM2MOMAM","SMB35","BotW","SADX","MK8Sub",
                                "MK:DD","BeatEm","MG:TT","MP3CPU","SM3DWBF",
                                "Risk 2","LMGCN","Elmo","OoTSub","CVNES",
                                "WL4","MvsDK","SMRPG","SMAS","GoldPig",
                                "Undertale","PS2RR")) %>%
  ggplot(aes(x=variable,y=value)) +
  geom_boxplot(aes(fill=variable),
               outlier.alpha = .8,
               outlier.size = .8,
               outlier.stroke = 1.2) +
  xlab("Game") +
  ylab("Pie's Predicted Chance of Winning") +
  ggtitle("MOMAM6 Predictions with Various Models") +
  scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(angle=69,vjust=.5,hjust=.25))











#Which models have the most faith in pie?
pie_prob %>% mutate(faith_in_pie = (round_1+round_2+round_3+round_4+round_5)/5) %>%
  dplyr::select(method,faith_in_pie) %>%
  arrange(desc(faith_in_pie)) %>%
  View()

boxplot(pie_prob$training_acc_or_roc,horizontal = T)

#View current leaderboard
pie_prob %>%
  arrange(desc(correct),desc(score),desc(training_acc_or_roc)) %>%
  
  View()

pie_prob %>%
  mutate(score = score/5) %>%
  select(method,score) %>%
  arrange(method) %>%
  View()

pie_prob %>%
  count(correct) %>%
  arrange(desc(correct))

View(pie_prob)


pie_prob <- update_scores(pie_prob,"spike",1)
pie_prob <- update_scores(pie_prob,"spike",2)
pie_prob <- update_scores(pie_prob,"pie",3)
pie_prob <- update_scores(pie_prob,"spike",4)
pie_prob <- update_scores(pie_prob,"spike",5)

pie_prob %>%
  mutate_at(vars(matches("round_")), "hi")

pie_prob %>%
  select(starts_with("round")) %>%
  sapply(prob_to_pred)

prob_to_pred <- function(value){
  if(value >= .5){
    return("pie")
  }
  else{
    return("spike")
  }
}

sapply(c(.2,.5,.7),prob_to_pred)

prob_to_pred(pie_prob[1,seq(2,32)])

apply(pie_prob[1,seq(2,32)],1,prob_to_pred)

generate_predictions <- function(df,model_name){
  model <- df %>%
    filter(method == model_name)
  
  values <- df %>%
    select(starts_with("round"))
  
  
  
  
  
}