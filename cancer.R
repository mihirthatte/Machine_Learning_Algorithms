#Initialize number of centroid -
k = 2


set.seed(1)
data = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",header=FALSE)

na.omit(data)
data = data[-grep("\\?",data[,7]),]
data[,7] <- as.integer(as.character(data[,7]))


min = min(data[,1])
max = max(data[,1])
for(r in 1:nrow(data)){
  data[r,1] = (data[r,1] - min)/(max - min)
  if(data[r,1]==0)
    data[r,1] = 1
  else if (data[r,1]==1)
    data[r,1] = 10
  else
    data[r,1] = data[r,1] *100
  if(data[r,1] > 10)
    data[r,1] = 10
  
  
}

for(r in 1:nrow(data)){
 data[r,1] = floor(data[r,1]) 
}

row = nrow(data)
col = ncol(data)




new_data = data
#head(new_data)

#Function to calculate nearest centroid amongst 1 to k
get_centroid <- function(centroids,row){
  index_vector <- c(rep(0,k))
  i =1
  for(c in 1:ncol(centroids)){
    index_vector[i] = sqrt(sum((row - centroids[,c])^2))
    i=i+1
  }
  index = 1
  min = index_vector[1]
  for(i in 2:length(index_vector)){
    if(index_vector[i] < min){
      index = i
    }
  }
  return(index)
}



centroids = matrix(c(rep(0,ncol(new_data))))

for(j in 1:ncol(new_data)){
  centroids[j,1] =  runif(1,min(new_data[,j]),max(new_data[,j]))
}



for(i in 2:k){
  v = matrix(c(rep(0,ncol(new_data))))
  for(j in 1:ncol(new_data)){
    v[j,1] =  runif(1,min(new_data[,j]),max(new_data[,j]))
  }
  centroids <- cbind(centroids, v[,1]) 
}

#centroids one column defines a point in N-dimension
new_data = cbind(new_data,rep(0,nrow(new_data)),deparse.level = 0)
iteration = 1
repeat{
  v=vector(mode="numeric",length =0)
  for(row in 1: nrow(new_data)){
    centroid_index = get_centroid(centroids,new_data[row,1:ncol(new_data)-1])
    v <- c(v,centroid_index)
  }
  
  new_data = new_data[,-ncol(new_data)]
  new_data = cbind(new_data,v,deparse.level = 0) # centroid got assigned to each data point
  
  old_centroids <- centroids
  #aggregate(new_data[,c], by=list(new_data[,ncol(new_data)]), FUN=mean)
  
  #Calculate Average - 
  for(k_i in 1:k){
    
    new_c = matrix(rep(0,ncol(new_data)-1),nrow=1, ncol=ncol(new_data)-1)
    for(x in 1:nrow(new_data)){
      if (new_data[x,ncol(new_data)] == k_i){
        colnames(new_c) <- colnames(new_data[x,1:ncol(new_data)-1]) #assigning same col name before 
        new_c = rbind(new_c, new_data[x,1:ncol(new_data)-1])
        
      }
    }
    for(x in 1:ncol(new_c)){
      if(length(new_c[,x]) == 1){
        centroids[x,k_i] = 0
      }
      else{
        centroids[x,k_i] = sum(new_c[,x])/(length(new_c[,x])-1)
      }
    }
    
  }
  diff_centroids = old_centroids - centroids
  
  norm_vec <- function(x) {
    return(sqrt(sum(x^2)))
  }
  convergence = 0
  for(i in 1:ncol(diff_centroids)){
    convergence = convergence+ norm_vec(diff_centroids[,i])
  }
  cat("iteration - ",iteration,"\n")
  print(convergence/k)
  iteration=iteration+1
  if(convergence/k <10)
    break
  
}
print(new_data)

summary(new_data[,12])

new_labels = new_data[,ncol(new_data)]
org_labels = data[,ncol(data)]
