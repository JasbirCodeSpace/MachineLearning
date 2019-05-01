setwd('C:\\Users\\JASBIR-PC\\Desktop\\1223\\1223\\1223\\Programs')
#1(a)
find_max<- function(v){
  max = v[1]
  for (i in 2:length(v)) {
    if(v[i]>max){
      max = v[i]
    }
  }
  return(max)
}
vector = c(read.csv("data.csv")[,5])
MAX = find_max(vector)
MAX

#1(b)
sum_even=function(v){
  sum=0
  for(i in 1:length(v)){
    if(v[i]%%2==0){
      sum=sum+v[i]
    }
  }
  return(sum)
}

SUM = sum_even(vector)
SUM

#1(c)
search_vector = function(v){
  num = as.numeric(readline(prompt = "Enter the number to search :: "))
  if(is.numeric(num)){
    for(i in 1:length(v)){
      if(v[i] == num){
        return(i)
      }
    }
  }else{
    return(-1)
  }
}

check = search_vector(vector)
if(check==-1){
  print("NOT FOUND")
}else{
  cat("FOUND AT LOCATION ::",check)
}

#1(d)
factorial_num = function(num){
  
  if(num==0){
      return(1)
    }else{
    return(num*factorial_num(num-1))
    }
}


num = as.integer(readline(prompt = "Enter a positive integer number :: "))
if(is.integer(num) && num>=0){
result = factorial_num(num)
cat("Factorial of ",num ," is = ",result)
}else{
  cat("INVALID INPUT")
}

#1(d)

Mean <- function(v){
  total=0
  for(i in 1:length(v)){
    total = total + v[i]
  }
  
  return(total/length(v))
}

Std_Dev<-function(v){
  error =0
  Mean = Mean(v)
  for(i in 1:length(v)){
    error = error+(v[i]-Mean)^2
  }
  
  return(sqrt(error/(length(v)-1)))
}

M = Mean(vector)
std_Dev = Std_Dev(vector)

#1(e)

Check_Prime = function(num){
  if(num<=1){
    return(FALSE)
  }
  for(i in 2:ceiling(num/2)){
    if(num%%i ==0){
      return(FALSE)
    }
  }
  
  return(TRUE)
}
num = as.integer(readline(prompt = "Enter a positive integer number :: "))
check = Check_Prime(num)
if(check){
  cat(num," is a Prime Number")
}else{
  cat(num," is not a Prime Number")
}

#1(f)

digit_sum <- function(num){
  if(num==0){
    return(num)
  }else{
  return(num%%10+digit_sum(floor(num/10)))
  }
}
num = as.integer(readline(prompt = "Enter a positive integer number :: "))
if(!is.na(num) && num>=0){
  result =  digit_sum(num)
  cat("Sum of digits of ",num," is = ",result)
}else{
  cat(num," is an Invalid Input for the given function")
}
