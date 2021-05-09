library(data.table) 
library(dplyr)
library(rstudioapi)

# function to generate matrix length
vector_length <- function(l){
  vector_a <<- vector(mode = "numeric", length = l)
}


# function to calculate majority
majority <- function(array_value){
  vector_a <- unlist(strsplit(array_value, " "))
  
  # Find max value in a vector
  max_freq <- max(table(vector_a))
  # max_freq
  # [1] 5
  
  # if value with max freq. less than a length/2 -> return -1
  if(max_freq <= length(vector_a)/2){
    a = -1
  } else {
    a = max_freq
  }
  
  
  # Logical vector to identify which element has max freq
  table(vector_a) == max(table(vector_a))
  
  # Print column with max freq. or -1 if can't find
  if(a == -1){
    return(paste0("Output: ",a))
  }else{
    return(paste0("Output: ", names(which(table(vector_a) == max(table(vector_a))))))
  }  
  
}

# get user input function
input_func <- function(){

# prompt to get array length
leng <- as.integer(readline(prompt = "Enter array length:"))

vector_length(leng)

vector_a

# prompt to get array element
array_value <- readline(prompt = "Enter array value:")

majority(array_value)
}

input_func()
