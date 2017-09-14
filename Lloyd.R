data = matrix(c(1,1,0,5,6,4))
data = cbind(data,c(4,3,4,1,2,0))
head(data)
row = nrow(data)
col = ncol(data)
#new_data = subset(data,select = c(4:col-1))
new_data = data
head(new_data)

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


#Initialize number of centroid -
k = 2

centroids = matrix(runif(ncol(new_data),min(new_data),max(new_data))) #randomly initialize centoirds using runif

for(i in 2:k){
  centroids <- cbind(centroids, runif(ncol(new_data),-1,1)) 
}
#centroids one column defines a point in N-dimension

v=vector(mode="numeric",length =0)
for(row in 1: nrow(new_data)){
  centroid_index = get_centroid(centroids,new_data[row,])
  v <- c(v,centroid_index)
}
print(nrow(new_data))
print(length(v))
new_data = cbind(new_data,v,deparse.level = 0) # centroid got assigned to each data point
print(new_data)
print(centroids)
#Initialization Complete
#print(new_data[,3])

#aggregate(new_data[,c], by=list(new_data[,ncol(new_data)]), FUN=mean)

#Calculate Average - 

for(k_i in 1:k){
  for(x in 1:nrow(new_data)){
    if (new_data[x,ncol(new_data)] == k_i){
      new_data[x,1]
    }
  }
}











