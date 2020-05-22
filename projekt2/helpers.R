read_csv_from_name <- function(name){
  return(read.csv(sprintf("./%s.csv",name)))
}

take_lables_from_data_set <- function(data_set){
  return(as.vector(data_set[,1]))
}

take_set_from_data_set <- function(data_set){
  return(as.matrix(data_set[,2:ncol(data_set)],rownames.force = NA))
}