library(e1071)


get_csv_and_use_it_for_testing<- function(name){
  data_set <- read.csv(sprintf("./%s.csv",name))
  y <- take_lables_from_data_set(data_set)
  X <- take_set_from_data_set(data_set)
  
  spliting <- sample(1:nrow(data_set))
  X_ <- split.data.frame(as.data.frame(X[spliting,]),rep(1:5))
  y_ <- split.data.frame(as.data.frame(y[spliting]),rep(1:5))
  
  ERR <- 0
  MAD <- 0
  MSE <- 0

  for(i in 1:5){
    X_df <- data.frame(X_[[i]])
    subX <- data.frame(X[-as.integer(rownames(X_df)),],stringsAsFactors = FALSE)
    suby <- y[-as.integer(rownames(y_[[i]]))]
    
    subX$etykieta <- factor(suby, levels = 1:max(suby)) 
    subX <- subX[,c(ncol(subX),1:(ncol(subX)-1))]
    
    classifier <- e1071::svm(formula = formula(subX),data = subX)
    res_agg <-  predict(classifier, newdata = X_df)
    res_agg <- as.numeric(res_agg)

    ERR <- ERR+sum(ifelse((res_agg - y_[[i]])!=0,1,0))/length(res_agg)
    MAD <- MAD+sum(abs(res_agg - y_[[i]]))/length(res_agg)
    MSE <- MSE+sum(abs(res_agg - y_[[i]])^2)/length(res_agg)
  }
  
  ERR <- ERR/5
  MAD <- MAD/5
  MSE <- MSE/5
  
  print(ERR)
  print(MAD)
  print(MSE)
  return(c(name,ERR,MAD,MSE))
}

data_sets <- c("abalone","abalone_ord","affairs","ailerons","auto_ord","auto_riskness","bostonhousing","bostonhousing_ord","californiahousing","cement_strength","fireman_example","glass","kinematics","machine_ord","skill","stock_ord","winequality-red","winequality-white","wisconsin_breast_ord")


bench_results <- data.frame(name=character(),ERR=numeric(),MAD=numeric(),MSE=numeric(),stringsAsFactors = FALSE)
for(name in data_sets){
  print(name)
  bench_results[nrow(bench_results)+1,] <- get_csv_and_use_it_for_testing(name)
}