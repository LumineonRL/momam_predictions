#Note: 100+ models are trained in this file despite me being fully aware
#that many of them are not appropriate for the data. I decided to train them
#anyway for a few reasons:
#1) It's fun to see how well the models perform even if I know I'll never trust it for predictions.
#2) The fast majority of them take at most a few minutes to train (since there's so little data)
#3) An algorithm that I'm unfamiliar with that ends up performing well will lead me
#to read up and learn more about that algorithm.

library(caret)
library(readr)

setwd("C:/users/Mike/Documents/R/momam6predictions")
full_data <- read_csv("momam6_trainingdata.csv")
to_predict <- read_csv("momam6_predictions.csv")


#Not a whole lot of data is available (only ~150 records), so a larger
#training set with LOOCV will be used.
set.seed(3151021)
trainIndex <- createDataPartition(full_data$winner,p=.8,
                                  list=F,
                                  times=1)


train <- full_data[trainIndex,-1]
test <- full_data[-trainIndex,1]


fitControl <- trainControl(method = "adaptive_cv")
adaptControl <- trainControl(method = "adaptive_cv",
                             number = 10, repeats = 10,
                             adaptive = list(min = 5, alpha = 0.05, 
                                             method = "gls", complete = TRUE),
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             search = "random")

#Model 1 Stochastic Gradient Boosting 
set.seed(3151021)
gbmFit <- caret::train(winner ~ ., data=train,
                method="gbm",
                trControl=fitControl,
                verbose=F)

predict(gbmFit,newdata=to_predict,type="prob")


#Model 2 adaboost
set.seed(3151021)
adaFit <- caret::train(winner ~ ., data=train,
                method="adaboost",
                trControl=fitControl)

predict(adaFit,newdata=to_predict,type="prob")


#Model 4 Adjacent Categories Probability Model for Ordinal Data
set.seed(3151021)
adjcatFit <- caret::train(winner ~ ., data=train,
                  method="vglmAdjCat",
                  trControl=fitControl)

predict(adjcatFit,newdata=to_predict,type="prob")

#Model 6 Bagged CART
set.seed(3151021)
adabagFit <- caret::train(winner ~ ., data=train,
                   method="treebag",
                   trControl=fitControl)

predict(adabagFit,newdata=to_predict,type="prob")


#Model 7 Bagged MARS - slow (done)
set.seed(3151021)
bagMARSFit <- caret::train(winner ~ ., data=train,
                   method = 'bagEarth',
                   trControl=fitControl)

predict(bagMARSFit,newdata=to_predict,type="prob")

#Model 8 Bayesian Generalized Linear Model
set.seed(3151021)
bayesglmFit <- caret::train(winner ~ ., data=train,
                    method = 'bayesglm',
                    trControl=fitControl)

predict(bayesglmFit,newdata=to_predict,type="prob")


#Model 9 Boosted Classification Trees
set.seed(3151021)
classtreesFit <- caret::train(winner ~ ., data=train,
                     method = 'ada',
                     trControl=fitControl)

predict(classtreesFit,newdata=to_predict,type="prob")

#Model 10 Boosted Generalized Linear Model
set.seed(3151021)
glmboostFit <- caret::train(winner ~ ., data=train,
                       method = 'glmboost',
                       trControl=fitControl)

predict(glmboostFit,newdata=to_predict,type="prob")

#Model 11 Boosted Linear Model
set.seed(3151021)
bstlmFit <- caret::train(winner ~ ., data=train,
                     method = 'BstLm',
                     trControl=fitControl)

predict(bstlmFit,newdata=to_predict)

#Model 12 Boosted Logistic Regression
set.seed(3151021)
logitboostFit <- caret::train(winner ~ ., data=train,
                  method = 'LogitBoost',
                  trControl=fitControl)

predict(logitboostFit,newdata=to_predict,type="prob")

#Model 14 Boosted Tree - slow (done)
set.seed(3151021)
bsttreeFit <- caret::train(winner ~ ., data=train,
                    method = 'bstTree',
                    trControl=fitControl)

predict(bsttreeFit,newdata=to_predict)

#Model 15 C4.5-like Trees
set.seed(3151021)
cliketreeFit <- caret::train(winner ~ ., data=train,
                    method = 'J48',
                    trControl=fitControl)

predict(cliketreeFit,newdata=to_predict,type="prob")

#Model 16 C5.0
set.seed(3151021)
c5Fit <- caret::train(winner ~ ., data=train,
                      method = 'C5.0',
                      trControl=fitControl)

predict(c5Fit,newdata=to_predict,type="prob")


#Model 17 CART
set.seed(3151021)
cartFit <- caret::train(winner ~ ., data=train,
               method = 'rpart',
               trControl=fitControl)

predict(cartFit,newdata=to_predict[2:47],type="prob")


#Model 18 Conditional Inference Random Forest
set.seed(3151021)
infertreeFit <- caret::train(winner ~ ., data=train,
                 method = 'cforest',
                 trControl=fitControl)

predict(infertreeFit,newdata=to_predict,type="prob")

#Model 19 Conditional Inference Tree
set.seed(3151021)
condinfertreeFit <- caret::train(winner ~ ., data=train,
                      method = 'ctree',
                      trControl=fitControl)

predict(condinfertreeFit,newdata=to_predict[2:47],type="prob")

#Model 20 Conditional Inference Tree 2
set.seed(3151021)
condinfertree2Fit <- caret::train(winner ~ ., data=train,
                          method = 'ctree2',
                          trControl=fitControl)

predict(condinfertree2Fit,newdata=to_predict[2:47],type="prob")

#Model 21 Continuation Ratio Model for Ordinal Data
set.seed(3151021)
ratiomodelFit <- caret::train(winner ~ ., data=train,
                           method = 'vglmContRatio',
                           trControl=fitControl)

predict(ratiomodelFit,newdata=to_predict,type="prob")

#Model 22 Cost-Sensitive C5.0
set.seed(3151021)
costc5Fit <- caret::train(winner ~ ., data=train,
                       method = 'C5.0Cost',
                       trControl=fitControl)

#No probabilities on this one either
predict(costc5Fit,newdata=to_predict)

#Model 23 Cost-Sensitive CART
set.seed(3151021)
costcartFit <- caret::train(winner ~ ., data=train,
                   method = 'rpartCost',
                   trControl=fitControl)

#No probabilities on this one either
predict(costcartFit,newdata=to_predict)

#Model 24 Cumulative Probability Model for Ordinal Data
set.seed(3151021)
vglmCumulativeFit <- caret::train(winner ~ ., data=train,
                     method = 'vglmCumulative',
                     trControl=fitControl)

predict(vglmCumulativeFit,newdata=to_predict,type="prob")


#Model 25 DeepBoost
set.seed(3151021)
deepboostFit <- caret::train(winner ~ ., data=train,
                           method = 'deepboost',
                           trControl=fitControl)

#No probabilities
predict(deepboostFit,newdata=to_predict)

#Model 26 Distance Weighted Discrimination with Radial Basis Function Kernel
set.seed(3151021)
dwdRadialFit <- caret::train(winner ~ ., data=train,
                      method = 'dwdRadial',
                      trControl=fitControl)

predict(dwdRadialFit,newdata=to_predict,type="prob")

#Model 27 eXtreme Gradient Boosting - slow (done)
set.seed(3151021)
xgbDARTFit <- caret::train(winner ~ ., data=train,
                      method = 'xgbDART',
                      trControl=fitControl)

predict(xgbDARTFit,newdata=to_predict[2:47],type="prob")

#Model 28 eXtreme Gradient Boosting Linear - slow (done)
set.seed(3151021)
xgbLinearFit <- caret::train(winner ~ ., data=train,
                    method = 'xgbLinear',
                    trControl=fitControl)

predict(xgbLinearFit,newdata=to_predict,type="prob")

#Model 29 eXtreme Gradient Boosting Tree - slow (done)
set.seed(3151021)
xgbTreeFit <- caret::train(winner ~ ., data=train,
                      method = 'xgbTree',
                      trControl=fitControl)

predict(xgbTreeFit,newdata=to_predict,type="prob")

#Model 30 Factor-Based Linear Discriminant Analysis
set.seed(3151021)
RFldaFit <- caret::train(winner ~ ., data=train,
                    method = 'RFlda',
                    trControl=fitControl)

predict(RFldaFit,newdata=to_predict)


#Model 31 Flexible Discriminant Analysis
set.seed(3151021)
fdaFit <- caret::train(winner ~ ., data=train,
                  method = 'fda',
                  trControl=fitControl)

predict(fdaFit,newdata=to_predict,type="prob")

#Model 36 Gaussian Process - slow
set.seed(3151021)
gaussprLinearFit <- caret::train(winner ~ ., data=train,
                          method = 'gaussprLinear',
                          trControl=fitControl)

predict(gaussprLinearFit,newdata=to_predict,type="prob")

#Model 37 Gaussian Process with Polynomial Kernal- slow
set.seed(3151021)
gaussprPolyFit <- caret::train(winner ~ ., data=train,
                        method = 'gaussprPoly',
                        trControl=fitControl)

predict(gaussprPolyFit,newdata=to_predict,type="prob")

#Model 38 Gaussian Process with Radial Basis Function Kernal- slow (done)
set.seed(3151021)
gaussprRadialFit <- caret::train(winner ~ ., data=train,
                          method = 'gaussprRadial',
                          trControl=fitControl)

predict(gaussprRadialFit,newdata=to_predict,type="prob")


#Model 39 Generalized Additive Model using LOESS
set.seed(3151021)
gamLoessFit <- caret::train(winner ~ ., data=train,
                          method = 'gamLoess',
                          trControl=fitControl)

predict(gamLoessFit,newdata=to_predict,type="prob")

#Model 40 Generalized Linear Model
set.seed(3151021)
glmFit <- caret::train(winner ~ ., data=train,
                     method = 'glm',
                     trControl=fitControl)

predict(glmFit,newdata=to_predict,type="prob")


#Model 41 Generalized Linear Model with Stepwise Feature Selection
set.seed(3151021)
glmStepAICFit <- caret::train(winner ~ ., data=train,
                method = 'glmStepAIC',
                trControl=fitControl)

predict(glmFit,newdata=to_predict,type="prob")

#Model 42 glmnet
set.seed(3151021)
glmnetFit <- caret::train(winner ~ ., data=train,
                       method = 'glmnet',
                       trControl=fitControl)

predict(glmnetFit,newdata=to_predict,type="prob")

#Model 43 Greedy Prototype Selection 
set.seed(3151021)
protoclassFit <- caret::train(winner ~ ., data=train,
                   method = 'protoclass',
                   trControl=fitControl)

predict(protoclassFit,newdata=to_predict)

#Model 44 Heteroscedastic Discriminant Analysis
set.seed(3151021)
hdaFit <- caret::train(winner ~ ., data=train,
                       method = 'hda',
                       trControl=fitControl)

predict(hdaFit,newdata=to_predict,type="prob")

#Model 45 High-Dimensional Regularized Discriminant Analysis
set.seed(3151021)
hddaFit <- caret::train(winner ~ ., data=train,
                method = 'hdda',
                trControl=fitControl)

predict(hddaFit,newdata=to_predict,type="prob")

#Model 46 High-Dimensional Regularized Discriminant Analysis
set.seed(3151021)
kknnFit <- caret::train(winner ~ ., data=train,
                 method = 'kknn',
                 trControl=fitControl)

predict(kknnFit,newdata=to_predict,type="prob")

#Model 47 L2 Regularized Linear Support Vector Machines with Class Weights
set.seed(3151021)
svmLinearWeights2Fit <- caret::train(winner ~ ., data=train,
                 method = 'svmLinearWeights2',
                 trControl=fitControl)

predict(svmLinearWeights2Fit,newdata=to_predict)

#Model 48 L2 Regularized Support Vector Machine (dual) with Linear Kernel
set.seed(3151021)
svmLinear3Fit <- caret::train(winner ~ ., data=train,
                              method = 'svmLinear3',
                              trControl=fitControl)

predict(kknnFit,newdata=to_predict,type="prob")

#Model 49 Learning Vector Quantization
set.seed(3151021)
lvqFit <- caret::train(winner ~ ., data=train,
                       method = 'lvq',
                       trControl=fitControl)

predict(kknnFit,newdata=to_predict,type="prob")

#Model 50 Linear Discriminant Analysis
set.seed(3151021)
ldaFit <- caret::train(winner ~ ., data=train,
                method = 'lda',
                trControl=fitControl)

predict(kknnFit,newdata=to_predict,type="prob")


#Model 51 Linear Discriminant Analysis with Stepwise Feature Selection
set.seed(3151021)
stepLDAFit <- caret::train(winner ~ ., data=train,
                method = 'stepLDA',
                trControl=fitControl)

predict(stepLDAFit,newdata=to_predict,type="prob")

#Model 52 Linear Distance Weighted Discrimination
set.seed(3151021)
dwdLinearFit <- caret::train(winner ~ ., data=train,
                    method = 'dwdLinear',
                    trControl=fitControl)

predict(dwdLinearFit,newdata=to_predict,type="prob")

#Model 53 Linear Support Vector Machines with Class Weights
set.seed(3151021)
svmlinearFit <- caret::train(winner ~ ., data=train,
                      method = 'svmLinearWeights',
                      trControl=fitControl)

predict(svmlinearFit,newdata=to_predict)

#Model 54 Logistic Model Trees
set.seed(3151021)
LMTFit <- caret::train(winner ~ ., data=train,
                      method = 'LMT',
                      trControl=fitControl)

predict(LMTFit,newdata=to_predict,type="prob")

#Model 55 Maximum Uncertainty Linear Discriminant Analysis
set.seed(3151021)
mldaFit <- caret::train(winner ~ ., data=train,
                      method = 'Mlda',
                      trControl=fitControl)

predict(mldaFit,newdata=to_predict)

#Model 56 Model Averaged Neural Network
set.seed(3151021)
avNNetFit <- caret::train(winner ~ ., data=train,
                method = 'avNNet',
                trControl=fitControl)

predict(avNNetFit,newdata=to_predict,type="prob")

#Model 57 Monotone Multi-Layer Perceptron Neural Network (slow) (d0ne)
set.seed(3151021)
monmlpFit <- caret::train(winner ~ ., data=train,
                   method = 'monmlp',
                   trControl=fitControl)

predict(monmlpFit,newdata=to_predict,type="prob")


#Model 58 Multi-Layer Perceptron
set.seed(3151021)
mlpFit <- caret::train(winner ~ ., data=train,
                   method = 'mlp',
                   trControl=fitControl)

predict(mlpFit,newdata=to_predict,type="prob")


#Model 59 Multi-Layer Perceptron with Weight Decay
set.seed(3151021)
mlpWeightDecayFit <- caret::train(winner ~ ., data=train,
                method = 'mlpWeightDecay',
                trControl=fitControl)

predict(mlpWeightDecayFit,newdata=to_predict,type="prob")


#Model 60 Multi-Layer Perceptron with multiple layers
set.seed(3151021)
mlpMLFit <- caret::train(winner ~ ., data=train,
                           method = 'mlpML',
                           trControl=fitControl)

predict(mlpMLFit,newdata=to_predict,type="prob")


#Model 61 Multi-Layer Perceptron with Weight Decay and multiple layers
set.seed(3151021)
mlpWeightDecayMLFit <- caret::train(winner ~ ., data=train,
                  method = 'mlpWeightDecayML',
                  trControl=fitControl)

predict(mlpWeightDecayMLFit,newdata=to_predict,type="prob")

#Model 62 Multi-Step Adaptive MCP-Net (slow) (done)
set.seed(3151021)
msaenetFit <- caret::train(winner ~ ., data=train,
                             method = 'msaenet',
                             trControl=fitControl)

predict(msaenetFit,newdata=to_predict,type="prob")

#Model 63 Multilayer Perceptron Network with Dropout (slow) (done)
set.seed(3151021)
mlpKerasDropoutFit <- caret::train(winner ~ ., data=train,
                    method = 'mlpKerasDropout',
                    trControl=fitControl)

predict(mlpKerasDropoutFit,newdata=to_predict,type="prob")


#Model 64 Multivariate Adaptive Regression Spline
set.seed(3151021)
earthFit <- caret::train(winner ~ ., data=train,
                            method = 'earth',
                            trControl=fitControl)

predict(earthFit,newdata=to_predict[2:47],type="prob")

#Model 65 Naive Bayes
set.seed(3151021)
naivebayesFit <- caret::train(winner ~ ., data=train,
                  method = 'naive_bayes',
                  trControl=fitControl)

predict(naivebayesFit,newdata=to_predict,type="prob")

#Model 66 Nearest Shrunken Centroids
set.seed(3151021)
nscFit <- caret::train(winner ~ ., data=train,
                       method = 'pam',
                       trControl=fitControl)

predict(nscFit,newdata=to_predict,type="prob")

#Model 67 Neural network
set.seed(3151021)
nnetFit <- caret::train(winner ~ ., data=train,
                method = 'nnet',
                trControl=fitControl)

predict(nnetFit,newdata=to_predict,type="prob")

#Model 68 Neural network with Feature Extraction
set.seed(3151021)
pcaNNetFit <- caret::train(winner ~ ., data=train,
                 method = 'pcaNNet',
                 trControl=fitControl)

predict(pcaNNetFit,newdata=to_predict,type="prob")


#Model 69 Non-Informative Model
set.seed(3151021)
nullFit <- caret::train(winner ~ ., data=train,
                    method = 'null',
                    trControl=fitControl)

predict(nullFit,newdata=to_predict,type="prob")

#Model 70 Oblique Random Forest (slow)
set.seed(3151021)
ORFlogFit <- caret::train(winner ~ ., data=train,
                   method = 'ORFlog',
                   trControl=fitControl)

predict(ORFlogFit,newdata=to_predict,type="prob")


#Model 71 Optimal Weighted Nearest Neighbor
set.seed(3151021)
ownnFit <- caret::train(winner ~ ., data=train,
                   method = 'ownn',
                   trControl=fitControl)

predict(ownnFit,newdata=to_predict)


#Model 72 Parallel Random Forest
set.seed(3151021)
parRFFit <- caret::train(winner ~ ., data=train,
                 method = 'parRF',
                 trControl=fitControl)

predict(parRFFit,newdata=to_predict,type="prob")


#Model 73 Partial Least Squares Generalized Linear Model
set.seed(3151021)
plsglmFit <- caret::train(winner ~ ., data=train,
                  method = 'plsRglm',
                  trControl=fitControl)

predict(plsglmFit,newdata=to_predict,type="prob")


#Model 74 Penalized Linear Discriminant Analysis
set.seed(3151021)
pdaFit <- caret::train(winner ~ ., data=train,
                   method = 'pda2',
                   trControl=fitControl)

predict(pdaFit,newdata=to_predict,type="prob")

#Model 75 Penalized Logistic Regression
set.seed(3151021)
plrFit <- caret::train(winner ~ ., data=train,
                method = 'plr',
                trControl=fitControl)

predict(plrFit,newdata=to_predict,type="prob")


#Model 76 Penalized Multinomial Regression
set.seed(3151021)
multinomFit <- caret::train(winner ~ ., data=train,
                method = 'multinom',
                trControl=fitControl)

predict(multinomFit,newdata=to_predict,type="prob")


#Model 77 Random Ferns
set.seed(3151021)
rFernsFit <- caret::train(winner ~ ., data=train,
                     method = 'rFerns',
                     trControl=fitControl)

predict(rFernsFit,newdata=to_predict)

#Model 78 Random Forest
set.seed(3151021)
rangerFit <- caret::train(winner ~ ., data=train,
                   method = 'ranger',
                   trControl=fitControl)

predict(rangerFit,newdata=to_predict)


#Model 79 Random Forest by Randomization
set.seed(3151021)
extraTreesFit <- caret::train(winner ~ ., data=train,
                    method = 'extraTrees',
                    trControl=fitControl)

predict(extraTreesFit,newdata=to_predict,type="prob")

#Model 80 Random Forest Rule-Based Model
set.seed(3151021)
rfRulesFit <- caret::train(winner ~ ., data=train,
                       method = 'rfRules',
                       trControl=fitControl)

predict(rfRulesFit,newdata=to_predict)

#Model 81 Regularized Logistic Regression
set.seed(3151021)
regLogistic <- caret::train(winner ~ ., data=train,
                       method = 'regLogistic',
                       trControl=fitControl)

predict(regLogistic,newdata=to_predict,type="prob")

#Model 82 Regularized Random Forests
set.seed(3151021)
RRF <- caret::train(winner ~ ., data=train,
                     method = 'RRF',
                     trControl=fitControl)

predict(RRF,newdata=to_predict,type="prob")


#Model 83 Robust Linear Discriminant Analysis
set.seed(3151021)
LindaFit <- caret::train(winner ~ ., data=train,
             method = 'Linda',
             trControl=fitControl)

predict(LindaFit,newdata=to_predict,type="prob")

#Model 84 Robust SIMCA
set.seed(3151021)
RSimcaFit <- caret::train(winner ~ ., data=train,
                  method = 'RSimca',
                  trControl=fitControl)

predict(RSimcaFit,newdata=to_predict)


#Model 85 ROC-Based Classifier
set.seed(3151021)
roccFit <- caret::train(winner ~ ., data=train,
                  method = 'rocc',
                  trControl=fitControl)

predict(roccFit,newdata=to_predict)


#Model 86 Rotation Forest
set.seed(3151021)
rotationForestFit <- caret::train(winner ~ ., data=train,
                 method = 'rotationForest',
                 trControl=fitControl)

predict(rotationForestFit,newdata=to_predict,type="prob")


#Model 87 Rule-Based Classifier
set.seed(3151021)
JRipFit <- caret::train(winner ~ ., data=train,
                           method = 'JRip',
                           trControl=fitControl)

predict(JRipFit,newdata=to_predict[2:47],type="prob")


#Model 88 Self-Organizing Maps
set.seed(3151021)
xyfFit <- caret::train(winner ~ ., data=train,
                 method = 'xyf',
                 trControl=fitControl)

predict(xyfFit,newdata=to_predict,type="prob")


#Model 89 Shrinkage Discriminant Analysis
set.seed(3151021)
sdaFit <- caret::train(winner ~ ., data=train,
                method = 'sda',
                trControl=fitControl)

predict(sdaFit,newdata=to_predict,type="prob")

#Model 90 SIMCA
set.seed(3151021)
CSimcaFit <- caret::train(winner ~ ., data=train,
                method = 'CSimca',
                trControl=fitControl)

predict(CSimcaFit,newdata=to_predict)

#Model 91 Single C5.0 Ruleset 
set.seed(3151021)
C5.0RulesFit <- caret::train(winner ~ ., data=train,
                method = 'C5.0Rules',
                trControl=fitControl)

predict(C5.0RulesFit,newdata=to_predict,type="prob")

#Model 92 Single C5.0 Tree 
set.seed(3151021)
C5.0TreeFit <- caret::train(winner ~ ., data=train,
                      method = 'C5.0Tree',
                      trControl=fitControl)

predict(C5.0TreeFit,newdata=to_predict,type="prob")

#Model 93 Single Rule Classification
set.seed(3151021)
OneRFit <- caret::train(winner ~ ., data=train,
                     method = 'OneR',
                     trControl=fitControl)

predict(OneRFit,newdata=to_predict,type="prob")

#Model 94 Sparse Distance Weighted Discrimination
set.seed(3151021)
sdwdFit <- caret::train(winner ~ ., data=train,
                 method = 'sdwd',
                 trControl=fitControl)

predict(sdwdFit,newdata=to_predict,type="prob")

#Model 95 Sparse Linear Discriminant Analysis
set.seed(3151021)
sparseLDAFit <- caret::train(winner ~ ., data=train,
                 method = 'sparseLDA',
                 trControl=fitControl)

predict(sparseLDAFit,newdata=to_predict,type="prob")


#Model 96 Sparse Partial Least Squares
set.seed(3151021)
splsFit <- caret::train(winner ~ ., data=train,
                      method = 'spls',
                      trControl=fitControl)

predict(splsFit,newdata=to_predict,type="prob")

#Model 97 Stabilized Linear Discriminant Analysis
set.seed(3151021)
sldaFit <- caret::train(winner ~ ., data=train,
                 method = 'slda',
                 trControl=fitControl)

predict(sldaFit,newdata=to_predict,type="prob")


#Model 98 Stabilized Nearest Neighbor Classifier
set.seed(3151021)
snnFit <- caret::train(winner ~ ., data=train,
                 method = 'snn',
                 trControl=fitControl)

predict(snnFit,newdata=to_predict)


#Model 99 Stacked AutoEncoder Deep Neural Network
set.seed(3151021)
dnnFit <- caret::train(winner ~ ., data=train,
                method = 'dnn',
                trControl=fitControl)

predict(dnnFit,newdata=to_predict,type="prob")


#Model 100 Tree Models from Genetic Algorithms
set.seed(3151021)
evtreeFit <- caret::train(winner ~ ., data=train,
                method = 'evtree',
                trControl=fitControl)

predict(evtreeFit,newdata=to_predict[2:47],type="prob")

#Model 101 Tree-Based Ensembles
set.seed(3151021)
nodeHarvestFit <- caret::train(winner ~ ., data=train,
                   method = 'nodeHarvest',
                   trControl=fitControl)

predict(nodeHarvestFit,newdata=to_predict,type="prob")


#Model 102 Weighted Subspace Random Forest
set.seed(3151021)
wsrfFit <- caret::train(winner ~ ., data=train,
                       method = 'wsrf',
                       trControl=fitControl)

predict(wsrfFit,newdata=to_predict,type="prob")
