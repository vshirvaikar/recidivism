reader = file("northcarolina1.txt", open = "r")
input = readLines(reader)
close(reader)
data = matrix(NA, nrow = 0, ncol = 28)
for(line in input){
  line = strtoi(strsplit(line, split = "")[[1]])
  if(line[28] != 3){
    data = rbind(data, line[1:28])
  }
}
reader = file("northcarolina2.txt", open = "r")
input = readLines(reader)
close(reader)
for(line in input){
  line = strtoi(strsplit(line, split = "")[[1]])
  if(line[28] != 3){
    data = rbind(data, line[1:28])
  }
}
data[is.na(data)] = 0

race = data[,1] # non-black = 1 - mean 51%
alcohol = data[,2] # alcohol problem = 1 - mean 29%
drugs = data[,3] # hard drug user = 1 - mean 23%
parole = data[,4] # parole = 1 - mean 76%
married = data[,5] # married = 1 - mean 25%
felony = data[,6] # felony = 1 - mean 37%
workrelease = data[,7] # work release = 1 - mean 44%
property = data[,8] # property crime = 1 - mean 36%
personal = data[,9] # personal crime = 1 - mean 9%
gender = data[,10] # male = 1 - mean 94%
priors = 10*data[,11] + data[,12] # mean 1.36
school = 10*data[,13] + data[,14] # mean 9.65
violations = 10*data[,15] + data[,16] # mean 1.36
age = (100*data[,17] + 10*data[,18] + data[,19])/12 # mean 28.52
prisontime = (100*data[,20] + 10*data[,21] + data[,22])/12 # mean 1.614
followup = (10*data[,23] + data[,24])/12 # mean 5.16
recid = data[,25] # mean 37%
returntime = (10*data[,26] + data[,27])/12 # mean 1.77, only for recid = 1

data.cov = data.frame(cbind(race, age, gender, alcohol, drugs, school, 
                            married, priors, felony, property, personal))
data.all = data.frame(cbind(recid, prisontime, data.cov))
