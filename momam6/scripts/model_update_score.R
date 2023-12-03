#The purpose of this file is to 1) Visualize predictions across various models
#and 2) Track how well each model is performing by updating the scores after
#each round.

library(stringr)
library(purrr)
library(ggplot2)
library(forcats) #for releveling factors
library(reshape2)
library(dplyr)
library(magrittr)
library(readr)


pie_prob <- read_csv("final_model_predictions_momam6.csv")


visualize <- function(df=pie_prob){
  df %>%
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
                                variable=="round_21"~"LuigiMan",
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
                                  "Risk 2","LuigiMan","Elmo","OoTSub","CVNES",
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
          axis.text.x = element_text(angle=70,vjust=.5,hjust=.25))
}

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

#Displays the distribution of how many correct responses each model has.
leaderboard_summary <- function(df=pie_prob){
  df %>%
    count(correct) %>%
    arrange(desc(correct))
}

#Displays which models are currently performing the best.
#Results are ordered by:
#1) The number of correct responses.
#2) Their confidence in each correct answer.
#3) Their test set performance.
model_leaderboard <- function(df=pie_prob){
  df %>%
    select(method,round_31,correct,score,road2momam6_acc) %>%
    arrange(desc(correct),desc(score),desc(road2momam6_acc)) %>%
    View()
}

#Accidentally save a result that didn't happen? This resets the correct and score
#columns so they can be readded properly.
leaderboard_reset_score <- function(df=pie_prob){
  df %>%
    mutate(correct = 0,
           score = 0) %>%
    return()
}


#Add results for each event. This should be updated daily.
pie_prob <- update_scores(pie_prob,"pie",1)
pie_prob <- update_scores(pie_prob,"spike",2)
pie_prob <- update_scores(pie_prob,"spike",3)
pie_prob <- update_scores(pie_prob,"pie",4)
pie_prob <- update_scores(pie_prob,"spike",5)
pie_prob <- update_scores(pie_prob,"pie",6)
pie_prob <- update_scores(pie_prob,"spike",7)
pie_prob <- update_scores(pie_prob,"pie",8)
pie_prob <- update_scores(pie_prob,"pie",9)
pie_prob <- update_scores(pie_prob,"pie",10)
pie_prob <- update_scores(pie_prob,"pie",11)
pie_prob <- update_scores(pie_prob,"spike",12)
pie_prob <- update_scores(pie_prob,"spike",13)
pie_prob <- update_scores(pie_prob,"pie",14)
pie_prob <- update_scores(pie_prob,"pie",15)
pie_prob <- update_scores(pie_prob,"spike",16)
pie_prob <- update_scores(pie_prob,"pie",17)
pie_prob <- update_scores(pie_prob,"spike",18)
pie_prob <- update_scores(pie_prob,"pie",19)
pie_prob <- update_scores(pie_prob,"pie",20)
pie_prob <- update_scores(pie_prob,"spike",21)
pie_prob <- update_scores(pie_prob,"spike",22)
pie_prob <- update_scores(pie_prob,"pie",23)
pie_prob <- update_scores(pie_prob,"pie",24)
pie_prob <- update_scores(pie_prob,"pie",25)
pie_prob <- update_scores(pie_prob,"spike",26)
pie_prob <- update_scores(pie_prob,"pie",27)
pie_prob <- update_scores(pie_prob,"pie",28)
pie_prob <- update_scores(pie_prob,"pie",29)
pie_prob <- update_scores(pie_prob,"spike",30)
pie_prob <- update_scores(pie_prob,"pie",31)

#Summary of the current results:
model_leaderboard()
visualize()
hist(pie_prob$correct,breaks = seq(-1,31))
write_csv(pie_prob,"final_model_predictions_momam6.csv")


pie_prob %>%
  mutate(accuracy = correct/31,
    roc = score/31) %>%
  select(method,correct,score,road2momam6_acc,accuracy,roc) %>%
  arrange(desc(roc)) %>%
    View()
