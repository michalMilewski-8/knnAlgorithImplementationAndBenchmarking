source("./knn.R")
source("./helpers.R")

aggregate_functions <- c(moda,srednia_a,srednia_wazona,minkara1.5,minkara3.0,mediana)
aggregate_functions_names <- c("moda","srednia_a","srednia_wazona","minkara1.5","minkara3.0","mediana")
data_sets <- c("abalone","abalone_ord","affairs","ailerons","auto_ord","auto_riskness","bostonhousing","bostonhousing_ord","californiahousing","cement_strength","fireman_example","glass","kinematics","machine_ord","skill","stock_ord","winequality-red","winequality-white","wisconsin_breast_ord")

get_csv_and_use_it_for_testing<- function(name, aggregate_funs, function_name){
  data_set <- read_csv_from_name(name)
  y <- take_lables_from_data_set(data_set)
  X <- take_set_from_data_set(data_set)
  
  spliting <- sample(1:nrow(data_set))
  X_ <- split.data.frame(as.data.frame(X[spliting,]),rep(1:5))
  y_ <- split.data.frame(as.data.frame(y[spliting]),rep(1:5))
  
  ERR <- 0
  MAD <- 0
  MSE <- 0
  
  benchmark_result <- data.frame(agregacja = character(),set=integer(),p=integer(),k=integer(),ERR=numeric(),MAD=numeric(),MSE=numeric(),stringsAsFactors = FALSE)
  
  for(i in 1:5){
    X_df <- X_[[i]]
    subX <- X[-as.integer(rownames(X_df)),]
    suby <- y[-as.integer(rownames(y_[[i]]))]
    
    for(p in c(1,2,Inf)){
      res <- knn(subX,suby,as.matrix(X_df),19,p)
      for(k in seq(1,19,by=2) ){
        for(funcc in 1:length(aggregate_funs)){
          res_agg <- aggregate_funs[[funcc]](res[,1:k])
          ERR <- sum(ifelse((res_agg - y_[[i]])!=0,1,0))/length(res_agg)
          MAD <- sum(abs(res_agg - y_[[i]]))/length(res_agg)
          MSE <- sum(abs(res_agg - y_[[i]])^2)/length(res_agg)
          benchmark_result[nrow(benchmark_result) + 1,] <-c(function_name[funcc],i,p,k,as.numeric(ERR),as.numeric(MAD),as.numeric(MSE))
        }
      }
    }
  }
  
  result <- aggregate.data.frame(benchmark_result[,c("ERR","MAD","MSE")],by=benchmark_result[,c("agregacja","p","k")],FUN = function(x) {mean(as.numeric(x))})
  write.csv(result,sprintf("knn_benchmarks/%s_benchmark.csv",name))
}
  
for(name in data_sets){
  print(name)
  get_csv_and_use_it_for_testing(name,aggregate_functions,aggregate_functions_names)
}

