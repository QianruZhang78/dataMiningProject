x = read.csv('/Users/yashasuru/Desktop/corona_tested_individuals_ver_0083.english.csv')
dimnames(x)
print(x)
positive = (x[,"corona_result"] == "positive" & !(is.na(x[,"corona_result"])))   
negative = (x[,"corona_result"] == "negative" & !(is.na(x[,"corona_result"])))
x = x[negative | positive,]
t = table(x[,c("cough", "gender","corona_result")])

print(t)

Q = prop.table(t, margin = c(3)) #Makes Conditional Prob Table
print(Q)
prior = c(sum(t[,,"negative"])/sum(t[,,]), sum(t[,,"positive"])/sum(t[,,])) #Gets the prior probs.


#Bayes Classifier(Simple Classifier since only 4 outcomes)
classes = c("negative", "positive")
c_head=array(0, dim=c(4,3))
i = 1
for (a in 1:2){ #1 represents No cough and 2 represents Cough symptom since 0 cannot be used in classifier
  for (e in c("male", "female")){
    c_head[i,1]=paste(a)
    c_head[i,2]=paste(e)
    c_head[i,3] = classes[which.max(c(Q[a,e,"negative"]*prior[1], Q[a,e,"positive"]*prior[2]))]
    i = i + 1
  }
}
print(c_head)
print((sum(t[,,"negative"])/2742596)*100)#Because this dataset is not the true sample size n

      