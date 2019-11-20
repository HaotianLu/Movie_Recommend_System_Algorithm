##################################################
### Getting Predictions from Prediction Matrix ###
##################################################

### Author: Yakun Wang
### Project 4


get.pred <- function(obs,est_rating){
  pred <- est_rating[as.character(obs[1]), as.character(obs[2])]
  return(pred)
}
