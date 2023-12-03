descrCor <-  cor(x)

highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
summary(descrCor[upper.tri(descrCor)])

highlyCorr <- findCorrelation(descrCor,cutoff=.75)
highlyCorr

View(descrCor)

findLinearCombos(x)

sort(descrCor[,16])
sort(descrCor[,36])

#is_zelda and is_adventure, as well as is_pc and is_educational have high correlations with each other.

x <- full_data[,3:48]

names(x[,c(16,36)])


preProcValues <- preProcess(x)

trainTransformed <- predict(preProcValues,x)
testTransformed <- predict(preProcValues,to_predict)

View(trainTransformed)
View(testTransformed)

#5 depth, 650 trees, .2 shrinkage, 20 nodes, 67% acc
gbmGrid <- expand.grid(interaction.depth = c(1,5,9),
                       n.trees = c(1:30)*50,
                       shrinkage=c(.1,.2),
                       n.minobsinnode=c(10,20))


#5 depth, 610 tree, 20 nodes, .25 shrinkage, 67.7% accuracy
gbmGrid2 <- expand.grid(interaction.depth = c(4,5,6),
                        n.trees = seq(560,660,by=50),
                        shrinkage = seq(.15,.35,by=.05),
                        n.minobsinnode=seq(15,25,by=5))

gbmGrid3 <- expand.grid(interaction.depth = 5,
                        n.trees = seq(570,650,by=20),
                        shrinkage = seq(.25,.55,by=.05),
                        n.minobsinnode=20)


set.seed(3151021)
gbmParamTune <- train(winner ~ ., data=train,
                method="gbm",
                trControl=fitControl,
                verbose=F,
                tuneGrid=gbmGrid3)
  
predict(gbmParamTune,newdata=to_predict[2:47],type="prob")
#predict(gbmFit,newdata=to_predict[3:48],type="prob")

#gbmFit
gbmParamTune

start_time <- Sys.time()
set.seed(3151021)
gbmROC <- train(winner ~ ., data=train,
                      method="gbm",
                      trControl=adaptControl,
                      verbose=F,
                      metric="ROC",
                      tuneLength=100)
end_time <- Sys.time()
end_time - start_time


gbmROC
predict(gbmROC,newdata=to_predict[2:47],type="prob")


#Now for gradient boosting
set.seed(3151021)
xgbDARTFit <- train(winner ~ ., data=train,
                    method = 'xgbDART',
                    trControl=fitControl)


xgbDARTFit #.617 is the accuracy to beat

start_time <- Sys.time()
set.seed(3151021)
xgbDARTFit <- train(winner ~ ., data=train,
                method="xgbDART",
                trControl=adaptControl,
                verbose=F,
                metric="ROC",
                tuneLength=50)
end_time <- Sys.time()
end_time - start_time

xgbDARTFit
predict(xgbDARTFit,newdata=to_predict[2:47],type="prob")


xgbTreeFit #Accuracy to beat: .59%?

start_time <- Sys.time()
set.seed(3151021)
xgbTreeROC <- train(winner ~ ., data=train,
                    method="xgbTree",
                    trControl=adaptControl,
                    verbose=F,
                    metric="ROC",
                    tuneLength=100)
end_time <- Sys.time()
end_time - start_time

xgbTreeROC
predict(xgbTreeROC,newdata=to_predict[2:47],type="prob")

mlpKerasDropoutFit #accuracy to beat: Like 51%...

start_time <- Sys.time()
set.seed(3151021)
mlpKerasDropoutROC <- train(winner ~ ., data=train,
                    method="mlpKerasDropout",
                    trControl=adaptControl,
                    verbose=F,
                    metric="ROC",
                    tuneLength=100)
end_time <- Sys.time()
end_time - start_time

mlpKerasDropoutROC
predict(mlpKerasDropoutROC,newdata=to_predict[2:47],type="prob")

max(avNNetFit$results$Accuracy) #Accuracy to beat: 61%. Improved to 64.8%

start_time <- Sys.time()
set.seed(3151021)
avNNetROC <- train(winner ~ ., data=train,
                            method="avNNet",
                            trControl=adaptControl,
                            verbose=F,
                            metric="ROC",
                            tuneLength=100)
end_time <- Sys.time()
end_time - start_time

avNNetROC
predict(avNNetROC,newdata=to_predict[2:47],type="prob")

max(avNNetROC$results$ROC)


#THIS SHIT TAKES FOREVER.
max(adaFit$results$Accuracy) #Accuracy to beat: 60.6%. Improved to 66.3%

start_time <- Sys.time()
set.seed(3151021)
adaROC <- train(winner ~ ., data=train,
                   method="ada",
                   trControl=adaptControl,
                   #verbose=F,
                   metric="ROC",
                   tuneLength=100)
end_time <- Sys.time()
end_time - start_time

adaROC
predict(adaROC,newdata=to_predict[2:47],type="prob")

max(ctree2ROC$results$ROC)

#This shit's fast af
max(condinfertree2Fit$results$Accuracy) #Accuracy to beat: 63.0%. Improved to 68.8%

start_time <- Sys.time()
set.seed(3151021)
ctree2ROC <- train(winner ~ ., data=train,
                   method="ctree2",
                   trControl=adaptControl,
                   #verbose=F,
                   metric="ROC",
                   tuneLength=100)
end_time <- Sys.time()
end_time - start_time

ctree2ROC
predict(ctree2ROC,newdata=to_predict[2:47],type="prob")

max(ctree2ROC$results$ROC)


max(plsglmFit$results$Accuracy) #Accuracy to beat: 63.6%: Fell to... 44.7%

start_time <- Sys.time()
set.seed(3151021)
plsRglmROC <- train(winner ~ ., data=train,
                   method="plsRglm",
                   trControl=adaptControl,
                   #verbose=F,
                   metric="ROC",
                   tuneLength=100)
end_time <- Sys.time()
end_time - start_time

plsRglmROC
predict(plsRglmROC,newdata=to_predict[2:47],type="prob")

max(plsRglmROC$results$ROC)


max(evtreeFit$results$Accuracy) #Accuracy to beat: 63.4%: Improved to 66.1%

start_time <- Sys.time()
set.seed(3151021)
evtreeROC <- train(winner ~ ., data=train,
                    method="evtree",
                    trControl=adaptControl,
                    #verbose=F,
                    metric="ROC",
                    tuneLength=100)
end_time <- Sys.time()
end_time - start_time

evtreeROC
predict(evtreeROC,newdata=to_predict[2:47],type="prob")

max(evtreeROC$results$ROC)


max(stepLDAFit$results$Accuracy) #Accuracy to beat: 63.3%

start_time <- Sys.time()
set.seed(3151021)
stepLDAROC <- train(winner ~ ., data=train,
                   method="stepLDA",
                   trControl=adaptControl,
                   #verbose=F,
                   metric="ROC",
                   tuneLength=100)
end_time <- Sys.time()
end_time - start_time

stepLDAROC
predict(stepLDAROC,newdata=to_predict[2:47],type="prob")

max(stepLDAROC$results$ROC)



#Determine which models performed the best on the training data
models <- list(adabagFit,adaFit,adjcatFit,avNNetFit,
               bagMARSFit,bayesglmFit,bstlmFit,bsttreeFit,
               C5.0RulesFit,C5.0TreeFit,c5Fit,cartFit,classtreesFit,
               cliketreeFit,condinfertree2Fit,condinfertreeFit,
               costc5Fit,costcartFit,CSimcaFit,deepboostFit,dnnFit,dwdLinearFit,
               dwdRadialFit,earthFit,evtreeFit,extraTreesFit,fdaFit,gamLoessFit,
               gaussprLinearFit,gaussprPolyFit,gaussprRadialFit,glmboostFit,
               glmFit,glmnetFit,glmStepAICFit,hdaFit,hddaFit,infertreeFit,
               JRipFit,kknnFit,ldaFit,LindaFit,LMTFit,logitboostFit,lvqFit,mldaFit,
               mlpFit,mlpKerasDropoutFit,mlpMLFit,mlpWeightDecayFit,
               mlpWeightDecayMLFit,monmlpFit,msaenetFit,multinomFit,naivebayesFit,
               nnetFit,nodeHarvestFit,nscFit,nullFit,OneRFit,ORFlogFit,ownnFit,
               parRFFit,pcaNNetFit,pdaFit,plrFit,plsglmFit,protoclassFit,
               rangerFit,ratiomodelFit,regLogistic,rFernsFit,RFldaFit,rfRulesFit,
               roccFit,rotationForestFit,RRF,RSimcaFit,sdaFit,sdwdFit,sldaFit,
               snnFit,sparseLDAFit,splsFit,stepLDAFit,splsFit,stepLDAFit,
               svmLinear3Fit,svmlinearFit,svmLinearFit,svmLinearWeights2Fit,
               vglmCumulativeFit,wsrfFit,xgbDARTFit,xgbLinearFit,
               xgbTreeFit,xyfFit)

max_accuracy <- models %>%
  map(., magrittr::extract("results")) %>%
  map(., magrittr::extract("Accuracy")) %>%
  map(max) %>%
  unlist()

model_name <- models %>%
  map(.,magrittr::extract("method")) %>%
  unlist()

accuracy_df <- tibble(model=model_name,
                          max_acc=max_accuracy)

accuracy_df %>%
  arrange(desc(max_acc)) %>%
  View()



#No hyper-parameter optimization ranking!
#plsRglm  0.63632642
#ctree2   0.63563673
#evtree   0.63356664
#stepLDA  0.63250815
#bstTree  0.6302520



max(xgbDARTFit$results$Accuracy)

svmPolyFit
predict(svmPolyFit,newdata=to_predict[2:47],type="prob")
