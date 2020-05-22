#install.packages('doBy')
#install.packages('matrixStats')
library('doBy')
library('matrixStats')

knn <- function(X, y, Z, k, p = 2) {
  X <- as.matrix(X)
  y <- as.vector(y)
  Z <- as.matrix(Z)
  if(is.finite(p))
  {
    nearest_neigbours <- apply(Z,1,function(vec2) {
      ztmp <- matrix(vec2,nrow = nrow(X),ncol=length(vec2),byrow = TRUE)
      return(doBy::which.minn(rowSums(abs(X-ztmp)^p)^(1/p),k))
    })
  } 
  else
  {
    nearest_neigbours <- apply(Z,1,function(vec2) {
      ztmp <- matrix(vec2,nrow = nrow(X),ncol=length(vec2),byrow = TRUE)
      return(doBy::which.minn(rowMaxs(abs(X-ztmp)),k))
    })
  }
 return(apply(as.matrix(nearest_neigbours),1,function(x) y[x]))
}

# nie używane, okazało się że można lepiej zwektoryzować
distance_from_vector <- function(X,vec,p,k) {
  return(doBy::which.minn(apply(X,1, function(x) minkowski_metrics(x,vec,p)),k))
}

# nie używane, okazało się że można lepiej zwektoryzować
distance_from_vector_inf <- function(X,vec,p,k) {
  return(doBy::which.minn(apply(X,1, function(x) minkowski_metrics_inf(x,vec,p)),k))
}

moda <- function(W){
  s<- apply(as.matrix(W),1,FUN = function(v) {
    uv <- unique(v)
    tab <- tabulate(match(v,uv))
    res <- uv[tab==max(tab,na.rm = TRUE)]
    return(res[sample(1:length(res),1)])
  })
}

# nie używane, okazało się że można lepiej zwektoryzować
minkowski_metrics <- function(vec1, vec2, p){
  return(sum(abs(vec1-vec2)^p,na.rm = TRUE)^(1/p))
}

# nie używane, okazało się że można lepiej zwektoryzować
minkowski_metrics_inf <- function(vec1, vec2, p){
  return(max(abs(vec1 - vec2),na.rm = TRUE))
}

srednia_a <- function(W) {
  return(apply(as.matrix(W),1,FUN = function(vec){
    sr <- sum(vec,na.rm = TRUE)/length(vec)
    if((sr %% 1)==0.5)
      return(round(sr + sample(c(-1, 1), 1) * .01))
    else
      return(round(sr))
  }))
}

mediana <- function(W) {
  return(apply(as.matrix(W),1, FUN = function(vec) {
    ord <- vec[order(vec)]
    if(length(vec)%%2 == 1){
      return(ord[(length(vec)-1)/2+1])
    }
    else{
      sr <- (ord[length(vec)/2] + ord[length(vec)/2+1])/2
      if((sr %% 1)==0.5)
        return(round(sr + sample(c(-1, 1), 1) * .01))
      else
        return(round(sr))
    }
  }))
}

srednia_wazona <- function(W){
  W <- as.matrix(W)
  row_len <- ncol(W);
  weigths_sum <- sum(1:row_len)
  row_weights <- row_len:1
  return(apply(as.matrix(W),1,FUN = function(vec){
    sr <- sum(vec*row_weights)/weigths_sum
    if((sr %% 1)==0.5)
      return(round(sr + sample(c(-1, 1), 1) * .01))
    else
      return(round(sr))
  }))
}



minkara1.5 <- function(W) {
  return(apply(as.matrix(W),1,FUN = function(vec) minkara(vec,1.5)))
}

minkara3.0 <- function(W) {
  return(apply(as.matrix(W),1,FUN = function(vec) minkara(vec,3.0)))
}

minkara <- function(vec, kara){
  max_val_in_vec <- max(vec)
  min_u <- 1
  min_val <- sum(abs(vec-min_u)^kara)
  for (val in 2:max_val_in_vec){
    tmp<-sum(abs(vec-val)^kara)
    if (tmp < min_val) {
      min_u <- val
      min_val <- tmp
    }
  }
  return(min_u)
}

