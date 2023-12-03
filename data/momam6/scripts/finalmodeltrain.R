library(caret)
library(readr)

setwd("C:/users/Mike/Documents/R/momam6predictions")
full_data <- read_csv("momam6_trainingdata.csv")
to_predict <- read_csv("momam6_predictions.csv")

train <- full_data[,2:48]
to_predict <- to_predict[,2:48]


fitControl <- trainControl(method = "adaptive_cv")
adaptControl <- trainControl(method = "adaptive_cv",
                             number = 10, repeats = 10,
                             adaptive = list(min = 5, alpha = 0.05, 
                                             method = "gls", complete = TRUE),
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             search = "random")


#First Place: ROC-Based Classifier
set.seed(3151021)
roccFit <- caret::train(winner ~ ., data=train,
                 method = 'rocc',
                 trControl=fitControl)

predict(roccFit,newdata=to_predict)


#Second Place: L2 Regularized Linear Support Vector Machines with Class Weights
set.seed(3151021)
svmLinearWeights2Fit <- caret::train(winner ~ ., data=train,
                              method = 'svmLinearWeights2',
                              trControl=fitControl)

predict(svmLinearWeights2Fit,newdata=to_predict)

svmLinearWeights2Fit$finalModel

#Third Place: Maximum Uncertainty Linear Discriminant Analysis
fitControl <- trainControl(method = "repeatedcv")
set.seed(3151021)
mldaFit <- caret::train(winner ~ ., data=train,
                 method = 'Mlda',
                 trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(mldaFit,newdata=to_predict)


#Fourth Place: Linear Support Vector Machines with Class Weights
set.seed(3151021)
svmlinearFit <- caret::train(winner ~ ., data=train,
                      method = 'svmLinearWeights',
                      trControl=fitControl)

predict(svmlinearFit,newdata=to_predict)

#Fifth Place: Factor-Based Linear Discriminant Analysis
set.seed(3151021)
RFldaFit <- caret::train(winner ~ ., data=train,
                  method = 'RFlda',
                  trControl=fitControl)

predict(RFldaFit,newdata=to_predict)

#Sixth Place: Boosted Linear Model
set.seed(3151021)
bstlmFit <- caret::train(winner ~ ., data=train,
                  method = 'BstLm',
                  trControl=fitControl)

predict(bstlmFit,newdata=to_predict)

#Seventh Place: L2 Regularized Support Vector Machine (dual) with Linear Kernel
set.seed(3151021)
svmLinear3Fit <- caret::train(winner ~ ., data=train,
                       method = 'svmLinear3',
                       trControl=fitControl)

predict(svmLinear3Fit,newdata=to_predict)

#8th Place: Stabilized Nearest Neighbor Classifier
set.seed(3151021)
snnFit <- caret::train(winner ~ ., data=train,
                method = 'snn',
                trControl=fitControl)

predict(snnFit,newdata=to_predict)

#9th Place: High-Dimensional Regularized Discriminant Analysis
set.seed(3151021)
hddaFit <- caret::train(winner ~ ., data=train,
                 method = 'hdda',
                 trControl=fitControl)

predict(hddaFit,newdata=to_predict,type="prob")


#10th Place: Generalized Linear Model
set.seed(3151021)
glmFit <- caret::train(winner ~ ., data=train,
                method = 'glm',
                trControl=fitControl)

predict(glmFit,newdata=to_predict,type="prob")

#11th Place: Adjacent Categories Probability Model for Ordinal Data
set.seed(3151021)
adjcatFit <- caret::train(winner ~ ., data=train,
                   method="vglmAdjCat",
                   trControl=fitControl)

predict(adjcatFit,newdata=to_predict,type="prob")

#12th Place: Linear Discriminant Analysis
set.seed(3151021)
ldaFit <- caret::train(winner ~ ., data=train,
                method = 'lda',
                trControl=fitControl)

predict(ldaFit,newdata=to_predict,type="prob")

#13th Place: Gaussian Process
fitControl <- trainControl(method = "repeatedcv")
set.seed(3151021)
gaussprLinearFit <- caret::train(winner ~ ., data=train,
                          method = 'gaussprLinear',
                          trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(gaussprLinearFit,newdata=to_predict,type="prob")


#14th Place: Generalized Linear Model with Stepwise Feature Selection
fitControl <- trainControl(method = "repeatedcv")
set.seed(3151021)
glmStepAICFit <- caret::train(winner ~ ., data=train,
                       method = 'glmStepAIC',
                       trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(glmStepAICFit,newdata=to_predict,type="prob")


#15th Place: Cumulative Probability Model for Ordinal Data
set.seed(3151021)
vglmCumulativeFit <- caret::train(winner ~ ., data=train,
                           method = 'vglmCumulative',
                           trControl=fitControl)

predict(vglmCumulativeFit,newdata=to_predict,type="prob")

#16th and 17th Place: Multi-Layer Perceptron with Weight Decay and multiple layers
set.seed(3151021)
mlpWeightDecayMLFit <- caret::train(winner ~ ., data=train,
                             method = 'mlpWeightDecayML',
                             trControl=fitControl)

predict(mlpWeightDecayMLFit,newdata=to_predict,type="prob")

#18th Place: C4.5-like Trees
set.seed(3151021)
cliketreeFit <- caret::train(winner ~ ., data=train,
                      method = 'J48',
                      trControl=fitControl)

predict(cliketreeFit,newdata=to_predict,type="prob")


#19th Place: Robust Linear Discriminant Analysis
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
LindaFit <- caret::train(winner ~ ., data=train,
                  method = 'Linda',
                  trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(LindaFit,newdata=to_predict,type="prob")

#20th Place: Boosted Logistic Regression
set.seed(3151021)
logitboostFit <- caret::train(winner ~ ., data=train,
                       method = 'LogitBoost',
                       trControl=fitControl)

predict(logitboostFit,newdata=to_predict,type="prob")


#21st Place: Bayesian Generalized Linear Model
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
bayesglmFit <- caret::train(winner ~ ., data=train,
                     method = 'bayesglm',
                     trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(bayesglmFit,newdata=to_predict,type="prob")


#22nd Place: Sparse Linear Discriminant Analysis
set.seed(3151021)
sparseLDAFit <- caret::train(winner ~ ., data=train,
                      method = 'sparseLDA',
                      trControl=fitControl)

predict(sparseLDAFit,newdata=to_predict,type="prob")

#23rd Place: Model Averaged Neural Network
set.seed(3151021)
avNNetFit <- caret::train(winner ~ ., data=train,
                   method = 'avNNet',
                   trControl=fitControl)

predict(avNNetFit,newdata=to_predict,type="prob")

#Model 24: Neural network
set.seed(3151021)
nnetFit <- caret::train(winner ~ ., data=train,
                 method = 'nnet',
                 trControl=fitControl)


#25th Place: Penalized Multinomial Regression
set.seed(3151021)
multinomFit <- caret::train(winner ~ ., data=train,
                     method = 'multinom',
                     trControl=fitControl)

predict(multinomFit,newdata=to_predict,type="prob")

#26th Place: Penalized Logistic Regression
set.seed(3151021)
plrFit <- caret::train(winner ~ ., data=train,
                method = 'plr',
                trControl=fitControl)

predict(plrFit,newdata=to_predict,type="prob")

#27th Place: Single C5.0 Tree 
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
C5.0TreeFit <- caret::train(winner ~ ., data=train,
                     method = 'C5.0Tree',
                     trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(C5.0TreeFit,newdata=to_predict,type="prob")

#28th Place: Self-Organizing Maps (lol)
set.seed(3151021)
xyfFit <- caret::train(winner ~ ., data=train,
                method = 'xyf',
                trControl=fitControl,
                tuneGrid = selfmapGrid)

predict(xyfFit,newdata=to_predict,type="prob")

#29th Place: glmnet
set.seed(3151021)
glmnetFit <- caret::train(winner ~ ., data=train,
                   method = 'glmnet',
                   trControl=fitControl)

predict(glmnetFit,newdata=to_predict,type="prob")

#30th Place: Boosted Generalized Linear Model
set.seed(3151021)
glmboostFit <- caret::train(winner ~ ., data=train,
                     method = 'glmboost',
                     trControl=fitControl)

predict(glmboostFit,newdata=to_predict,type="prob")


#31st Place: Regularized Logistic Regression
set.seed(3151021)
regLogistic <- caret::train(winner ~ ., data=train,
                     method = 'regLogistic',
                     trControl=fitControl)

predict(regLogistic,newdata=to_predict,type="prob")

#32nd Place Stabilized Linear Discriminant Analysis
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
sldaFit <- caret::train(winner ~ ., data=train,
                 method = 'slda',
                 trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(sldaFit,newdata=to_predict,type="prob")

#33rd Place: Linear Distance Weighted Discrimination
set.seed(3151021)
dwdLinearFit <- caret::train(winner ~ ., data=train,
                      method = 'dwdLinear',
                      trControl=fitControl)

predict(dwdLinearFit,newdata=to_predict,type="prob")

#34th Place: avnnet with random hyperparameter search
set.seed(3151021)
avNNetROC <- caret::train(winner ~ ., data=train,
                   method="avNNet",
                   trControl=adaptControl,
                   verbose=F,
                   metric="ROC",
                   tuneLength=100)

predict(avNNetROC,newdata=to_predict,type="prob")

#35th Place: Sparse Distance Weighted Discrimination
set.seed(3151021)
sdwdFit <- caret::train(winner ~ ., data=train,
                 method = 'sdwd',
                 trControl=fitControl)

predict(sdwdFit,newdata=to_predict,type="prob")

#36th Place: Penalized Linear Discriminant Analysis
set.seed(3151021)
pdaFit <- caret::train(winner ~ ., data=train,
                method = 'pda2',
                trControl=fitControl)

predict(pdaFit,newdata=to_predict,type="prob")

#37th Place: Gaussian Process with Radial Basis Function Kernal
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
gaussprRadialFit <- caret::train(winner ~ ., data=train,
                          method = 'gaussprRadial',
                          trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(gaussprRadialFit,newdata=to_predict,type="prob")

#38th Place: Nearest Shrunken Centroids
set.seed(3151021)
nscFit <- caret::train(winner ~ ., data=train,
                method = 'pam',
                trControl=fitControl)

predict(nscFit,newdata=to_predict,type="prob")

#39th Place: Naive Bayes
set.seed(3151021)
naivebayesFit <- caret::train(winner ~ ., data=train,
                       method = 'naive_bayes',
                       trControl=fitControl)

predict(naivebayesFit,newdata=to_predict,type="prob")

#40th Place: Continuation Ratio Model for Ordinal Data
set.seed(3151021)
ratiomodelFit <- caret::train(winner ~ ., data=train,
                       method = 'vglmContRatio',
                       trControl=fitControl)

predict(ratiomodelFit,newdata=to_predict,type="prob")

#41st Place: Random Ferns
set.seed(3151021)
rFernsFit <- caret::train(winner ~ ., data=train,
                   method = 'rFerns',
                   trControl=fitControl)

predict(rFernsFit,newdata=to_predict)


#42nd Place: Learning Vector Quantization
set.seed(3151021)
lvqFit <- caret::train(winner ~ ., data=train,
                method = 'lvq',
                trControl=fitControl)

predict(lvqFit,newdata=to_predict)

#43rd Place: Random Forest Rule-Based Model
set.seed(3151021)
rfRulesFit <- caret::train(winner ~ ., data=train,
                    method = 'rfRules',
                    trControl=fitControl)

predict(rfRulesFit,newdata=to_predict)

#44th Place: Random Forest
set.seed(3151021)
rangerFit <- caret::train(winner ~ ., data=train,
                   method = 'ranger',
                   trControl=fitControl)

predict(rangerFit,newdata=to_predict)

#45th Place: Shrinkage Discriminant Analysis
set.seed(3151021)
sdaFit <- caret::train(winner ~ ., data=train,
                method = 'sda',
                trControl=fitControl)

predict(sdaFit,newdata=to_predict,type="prob")

#46th and 47th Place: Multi-Layer Perceptron
set.seed(3151021)
mlpFit <- caret::train(winner ~ ., data=train,
                method = 'mlp',
                trControl=fitControl)

predict(mlpFit,newdata=to_predict,type="prob")

#48th Place: Gaussian Process with Polynomial Kernal
set.seed(3151021)
gaussprPolyFit <- caret::train(winner ~ ., data=train,
                        method = 'gaussprPoly',
                        trControl=fitControl)

predict(gaussprPolyFit,newdata=to_predict,type="prob")

#49th Place: Single C5.0 Ruleset 
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
C5.0RulesFit <- caret::train(winner ~ ., data=train,
                      method = 'C5.0Rules',
                      trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(C5.0RulesFit,newdata=to_predict,type="prob")

#50th Place Oblique Random Forest
set.seed(3151021)
ORFlogFit <- caret::train(winner ~ ., data=train,
                   method = 'ORFlog',
                   trControl=fitControl)

predict(ORFlogFit,newdata=to_predict,type="prob")

#51st Place: Neural network with Feature Extraction
set.seed(3151021)
pcaNNetFit <- caret::train(winner ~ ., data=train,
                    method = 'pcaNNet',
                    trControl=fitControl)

predict(pcaNNetFit,newdata=to_predict,type="prob")

#52nd Place: Distance Weighted Discrimination with Radial Basis Function Kernel
set.seed(3151021)
dwdRadialFit <- caret::train(winner ~ ., data=train,
                      method = 'dwdRadial',
                      trControl=fitControl)

predict(dwdRadialFit,newdata=to_predict,type="prob")

#53rd Place (Model Ensemble)

#54th Place: eXtreme Gradient Boosting Tree
set.seed(3151021)
xgbTreeFit <- caret::train(winner ~ ., data=train,
                    method = 'xgbTree',
                    trControl=fitControl)

predict(xgbTreeFit,newdata=to_predict,type="prob")

#55th Place: Regularized Random Forests
set.seed(3151021)
RRF <- caret::train(winner ~ ., data=train,
             method = 'RRF',
             trControl=fitControl)

predict(RRF,newdata=to_predict,type="prob")

#56th Place: Rotation Forest
set.seed(3151021)
rotationForestFit <- caret::train(winner ~ ., data=train,
                           method = 'rotationForest',
                           trControl=fitControl)

predict(rotationForestFit,newdata=to_predict,type="prob")

#57th Place: Stochastic Gradient Boosting
set.seed(3151021)
gbmFit <- caret::train(winner ~ ., data=train,
                       method="gbm",
                       trControl=fitControl,
                       verbose=F)

predict(gbmFit,newdata=to_predict,type="prob")

#58th Place: Keras Dropout with random hyperparameter search
set.seed(3151021)
mlpKerasDropoutROC <- caret::train(winner ~ ., data=train,
                            method="mlpKerasDropout",
                            trControl=adaptControl,
                            verbose=F,
                            metric="ROC",
                            tuneLength=100)

predict(mlpKerasDropoutROC,newdata=to_predict,type="prob")

#59th Place: Parallel Random Forest
set.seed(3151021)
parRFFit <- caret::train(winner ~ ., data=train,
                         method = 'parRF',
                         trControl=fitControl)

predict(parRFFit,newdata=to_predict,type="prob")

#60th Place: Conditional Inference Random Forest
set.seed(3151021)
infertreeFit <- caret::train(winner ~ ., data=train,
                             method = 'cforest',
                             trControl=fitControl)

predict(infertreeFit,newdata=to_predict,type="prob") %>%
  use_series("pie") %>%
  subtract(min(.)) %>%
  divide_by(max(.)-min(.))

#61st Place: adaboost
set.seed(3151021)
adaFit <- caret::train(winner ~ ., data=train,
                       method="adaboost",
                       trControl=fitControl)

adalol <- predict(adaFit,newdata=to_predict,type="prob")

(adalol[,1]-min(adalol[,1]))/(max(adalol[,1]-min(adalol[1])))



predict(adaFit,newdata=to_predict,type="prob") %>%
  use_series("pie") %>%
  subtract(min(.)) %>%
  divide_by(max(.)-min(.))

#62nd Place: Non-Informative Model
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
nullFit <- caret::train(winner ~ ., data=train,
                        method = 'null',
                        trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(nullFit,newdata=to_predict,type="prob")

#63rd Place: Stacked AutoEncoder Deep Neural Network
set.seed(3151021)
dnnFit <- caret::train(winner ~ ., data=train,
                       method = 'dnn',
                       trControl=fitControl)

predict(dnnFit,newdata=to_predict,type="prob")

#64th Place Weighted Subspace Random Forest
set.seed(3151021)
wsrfFit <- caret::train(winner ~ ., data=train,
                        method = 'wsrf',
                        trControl=fitControl)

predict(wsrfFit,newdata=to_predict,type="prob")

#65th Place: Conditional Inference Tree
set.seed(3151021)
condinfertreeFit <- caret::train(winner ~ ., data=train,
                                 method = 'ctree',
                                 trControl=fitControl)

predict(condinfertreeFit,newdata=to_predict,type="prob")

#66th Place: Conditional Inference Tree 2 with Random Hyperparameter search
set.seed(3151021)
ctree2ROC <- caret::train(winner ~ ., data=train,
                   method="ctree2",
                   trControl=adaptControl,
                   #verbose=F,
                   metric="ROC",
                   tuneLength=100)

predict(ctree2ROC,newdata=to_predict,type="prob")

#67th Place: Boosted Classification Trees
set.seed(3151021)
classtreesFit <- caret::train(winner ~ ., data=train,
                              method = 'ada',
                              trControl=fitControl)

predict(classtreesFit,newdata=to_predict,type="prob")

#68th and 69th Place: eXtreme Gradient Boosting Linear
set.seed(3151021)
xgbLinearFit <- caret::train(winner ~ ., data=train,
                             method = 'xgbLinear',
                             trControl=fitControl)

predict(xgbLinearFit,newdata=to_predict,type="prob")

#70th Place: C5.0
set.seed(3151021)
c5Fit <- caret::train(winner ~ ., data=train,
                      method = 'C5.0',
                      trControl=fitControl)

predict(c5Fit,newdata=to_predict,type="prob")

#71st Place: Tree-Based Ensembles
set.seed(3151021)
nodeHarvestFit <- caret::train(winner ~ ., data=train,
                               method = 'nodeHarvest',
                               trControl=fitControl)

predict(nodeHarvestFit,newdata=to_predict,type="prob")

#72nd Place: Boosted Tree
set.seed(3151021)
bsttreeFit <- caret::train(winner ~ ., data=train,
                           method = 'bstTree',
                           trControl=fitControl)

predict(bsttreeFit,newdata=to_predict)

#73rd Place: Single Rule Classification
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
OneRFit <- caret::train(winner ~ ., data=train,
                        method = 'OneR',
                        trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(OneRFit,newdata=to_predict,type="prob")

#74th Place: DeepBoost
set.seed(3151021)
deepboostFit <- caret::train(winner ~ ., data=train,
                             method = 'deepboost',
                             trControl=fitControl)

predict(deepboostFit,newdata=to_predict)

#75th Place: Greedy Prototype Selection 
set.seed(3151021)
protoclassFit <- caret::train(winner ~ ., data=train,
                              method = 'protoclass',
                              trControl=fitControl)

predict(protoclassFit,newdata=to_predict)

#76th Place: Optimal Weighted Nearest Neighbor
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
ownnFit <- caret::train(winner ~ ., data=train,
                        method = 'ownn',
                        trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(ownnFit,newdata=to_predict)

#77th Place: SIMCA
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
CSimcaFit <- caret::train(winner ~ ., data=train,
                          method = 'CSimca',
                          trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(CSimcaFit,newdata=to_predict)

#78th Place: Monotone Multi-Layer Perceptron Neural Network
set.seed(3151021)
monmlpFit <- caret::train(winner ~ ., data=train,
                          method = 'monmlp',
                          trControl=fitControl)

predict(monmlpFit,newdata=to_predict,type="prob")

#79th Place: Bagged CART
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
adabagFit <- caret::train(winner ~ ., data=train,
                          method="treebag",
                          trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(adabagFit,newdata=to_predict,type="prob")

#82nd Place: Flexible Discriminant Analysis
set.seed(3151021)
fdaFit <- caret::train(winner ~ ., data=train,
                       method = 'fda',
                       trControl=fitControl)

predict(fdaFit,newdata=to_predict,type="prob")

#83rd Place: Linear Discriminant Analysis with Stepwise Feature Selection
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
stepLDAFit <- caret::train(winner ~ ., data=train,
                           method = 'stepLDA',
                           trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(stepLDAFit,newdata=to_predict,type="prob")


#84th Place: Conditional Inference Tree 2
set.seed(3151021)
condinfertree2Fit <- caret::train(winner ~ ., data=train,
                                  method = 'ctree2',
                                  trControl=fitControl)

predict(condinfertree2Fit,newdata=to_predict,type="prob")


#87th Place: Rule-Based Classifier
set.seed(3151021)
JRipFit <- caret::train(winner ~ ., data=train,
                        method = 'JRip',
                        trControl=fitControl)

predict(JRipFit,newdata=to_predict,type="prob")

#88th Place: CART
set.seed(3151021)
cartFit <- caret::train(winner ~ ., data=train,
                        method = 'rpart',
                        trControl=fitControl)

predict(cartFit,newdata=to_predict,type="prob")

#91st Place: Logistic Model Trees
set.seed(3151021)
LMTFit <- caret::train(winner ~ ., data=train,
                       method = 'LMT',
                       trControl=fitControl)

predict(LMTFit,newdata=to_predict,type="prob")

#94th Place: Multi-Step Adaptive MCP-Net
set.seed(3151021)
msaenetFit <- caret::train(winner ~ ., data=train,
                           method = 'msaenet',
                           trControl=fitControl)

predict(msaenetFit,newdata=to_predict,type="prob")

#96th Place: Sparse Partial Least Squares
set.seed(3151021)
splsFit <- caret::train(winner ~ ., data=train,
                        method = 'spls',
                        trControl=fitControl)

predict(splsFit,newdata=to_predict,type="prob")

#97th Place: Generalized Additive Model using LOESS
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
gamLoessFit <- caret::train(winner ~ ., data=train,
                            method = 'gamLoess',
                            trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(gamLoessFit,newdata=to_predict,type="prob")

#98th Place: Random Forest by Randomization
set.seed(3151021)
extraTreesFit <- caret::train(winner ~ ., data=train,
                              method = 'extraTrees',
                              trControl=fitControl)

predict(extraTreesFit,newdata=to_predict,type="prob")

#99th Place: Heteroscedastic Discriminant Analysis
set.seed(3151021)
hdaFit <- caret::train(winner ~ ., data=train,
                       method = 'hda',
                       trControl=fitControl)

predict(hdaFit,newdata=to_predict,type="prob")

#100th Place: Cost-Sensitive CART
set.seed(3151021)
costcartFit <- caret::train(winner ~ ., data=train,
                            method = 'rpartCost',
                            trControl=fitControl)

predict(costcartFit,newdata=to_predict)

#101st Place: K-Nearest Neighbors
set.seed(3151021)
kknnFit <- caret::train(winner ~ ., data=train,
                        method = 'kknn',
                        trControl=fitControl)

predict(kknnFit,newdata=to_predict,type="prob")

#102nd Place: Multilayer Perceptron Network with Dropout (slow) (done)
set.seed(3151021)
mlpKerasDropoutFit <- caret::train(winner ~ ., data=train,
                                   method = 'mlpKerasDropout',
                                   trControl=fitControl)

predict(mlpKerasDropoutFit,newdata=to_predict,type="prob")

#103rd Place: Bagged MARS
set.seed(3151021)
bagMARSFit <- caret::train(winner ~ ., data=train,
                           method = 'bagEarth',
                           trControl=fitControl)

predict(bagMARSFit,newdata=to_predict,type="prob")

#105th Place: Cost-Sensitive C5.0
set.seed(3151021)
costc5Fit <- caret::train(winner ~ ., data=train,
                          method = 'C5.0Cost',
                          trControl=fitControl)

predict(costc5Fit,newdata=to_predict)

#106th Place: Robust SIMCA
set.seed(3151021)
fitControl <- trainControl(method = "repeatedcv")
RSimcaFit <- caret::train(winner ~ ., data=train,
                          method = 'RSimca',
                          trControl=fitControl)
fitControl <- trainControl(method = "adaptive_cv")

predict(RSimcaFit,newdata=to_predict)

#107th Place: Partial Least Squares Generalized Linear Model
set.seed(3151021)
plsglmFit <- caret::train(winner ~ ., data=train,
                          method = 'plsRglm',
                          trControl=fitControl)

predict(plsglmFit,newdata=to_predict,type="prob")


#Unranked: Fuzzy Rules Using Chi's Method
set.seed(3151021)
FRBCS.CHIFit <- caret::train(winner ~ ., data=train,
                          method = 'FRBCS.CHI',
                          trControl=fitControl)

predict(FRBCS.CHIFit,newdata=to_predict)
