#Use FunctionalDiversity_Overall_Distribution.R to create SiteTrait list without family averaged values
#Call this object "Keeper"

# Extract, then add family and class to the trait table
m.Class <- list()
m.Family <- list()
for(i in 1:length(Keeper)){
  m.Class[[i]] <- masterlist$Class[match(rownames(Keeper[[i]]), masterlist$Unique_Name)]
  m.Family[[i]] <- masterlist$Family[match(rownames(Keeper[[i]]), masterlist$Unique_Name)]
  
}

Keeper2 <- list()
for(i in 1:length(Keeper)){
Keeper2[[i]] <- cbind(Keeper[[i]], m.Class[[i]], m.Family[[i]])
}

#Summarize

WriteThisList <- Keeper2
combinedWriteThisList <- rbind(Keeper2[[1]], Keeper2[[2]], Keeper2[[3]], Keeper2[[4]], Keeper2[[5]], 
                               Keeper2[[6]], Keeper2[[7]], Keeper2[[8]], Keeper2[[9]], Keeper2[[10]], 
                               Keeper2[[11]], Keeper2[[12]], Keeper2[[13]], Keeper2[[14]])

write.table(combinedWriteThisList, file="combinedWriteThisList.csv", sep=",")