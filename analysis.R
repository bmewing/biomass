library(readr)
library(purrr)
library(data.table)

d = read_csv("data/example.csv")

l = split(d,d$fish)

identifyShifts = function(example){
  example = example[order(example$year),] #sort data by year
  example$state = "Stable"
  toCheck = which(example$biomass <= 1 & example$biomass >= 0.5)
  toCheck = toCheck[toCheck != 1] #if the first row is <=1 and >=0.5 it can't be flagged as anything other than 'Stable', so ignore it
  while(length(toCheck) > 0){
    i = toCheck[1]
    ns = which(`-`(toCheck[-1],i) == 1:(length(toCheck)-1))
    tr = 1:(ifelse(length(ns)>0,max(ns),0)+1)
    if(any(toCheck[tr] == nrow(example)) | length(tr) == 0){ #if the table ends before the transition period finshes, there's not choice but to label it 'Stable', so end early
      toCheck = c()
      next
    }
    if(example$biomass[i-1] > 1){
      if(example$biomass[length(tr)+i] < 0.5) example$state[(i-1):(i+length(tr)-1)] = "Decline"
    } else if(example$biomass[i-1] < 0.5){
      if(example$biomass[length(tr)+i] > 1.0) example$state[(i-1):(i+length(tr)-1)] = "Recover"
    }
    toCheck = toCheck[(1:length(tr))*-1]
  }
  runs = rle(example$state)
  example$runID = unlist(map2(seq_along(runs$lengths),runs$lengths,rep))
  return(as.data.table(example))
}

summarizeShifts = function(fish){
  return(fish[,list("avgF"=mean(`F`),"runLength" = .N,"start" = min(`year`),"end" = max(`year`)),by=list(fish,state,runID)])
}

shifts = lapply(l,identifyShifts)
summaries = lapply(shifts,summarizeShifts)

write_csv(rbindlist(summaries)[,c("fish","state","avgF","runLength","start","end")],"data/summarized.csv")
