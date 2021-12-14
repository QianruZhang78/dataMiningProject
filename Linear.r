data = read.csv("owid-covid-data.csv",stringsAsFactors=FALSE, sep=",", nrows = 10000)[-c(2)]
data <- as.matrix(as.numeric(data)) #Input data + convert to numeric values
data[is.na(data)] <- 0 #Remove any rows with NA values

x=data[,51:55] # Delimit symptom parameters

plot(x,y) #Plot data for reference
X = as.matrix(x) # Add symptoms to matrix
y=as.vector(data[,9]) #Add target parameter to vector

a = solve(t(X) %*% X, t(X) %*% y) #solve
print(a)
cat(a, '\n')    	       	      	       	  
yhat = X %*% a
error = y - yhat
sse = sum(error*error) #Get sum squared error 
cat(sse, '\n')



