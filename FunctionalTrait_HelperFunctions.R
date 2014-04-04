# Helper functions for assigning average Family values to missing trait data values

f.family.AC <- function(data){ # determine mode for each Family for Activity Cycle 
  hold <- table(data$Family, data$ActivityCycle)
  ActivityCycle_m <- vector(mode="numeric", length=dim(hold)[1])
  
  for(i in 1:dim(hold)[1]){
    ActivityCycle_m[i] <- ifelse(hold[i,1] > hold[i,2] & hold[i,1] > hold[i,3], 1, 
                                 ifelse(hold[i,2] > hold[i,1] & hold[i,2] > hold[i,3], 2, 
                                        ifelse(hold[i,3] > hold[i,1] & hold[i,3] > hold[i,2], 3, NA)))
  }
  ActivityCycle_m
}



f.family.HB <- function(data){ # determine mode for each Family for Habitat Breadth
  hold <- table(data$Family, data$HabitatBreadth)
  HabitatBreadth_m <- vector(mode="numeric", length=dim(hold)[1])
  
  for(i in 1:dim(hold)[1]){
    HabitatBreadth_m[i] <- ifelse(hold[i,1] > hold[i,2] & hold[i,1] > hold[i,3] & hold[i,1] > hold[i,4], 1, 
                                  ifelse(hold[i,2] > hold[i,1] & hold[i,2] > hold[i,3] & hold[i,2] > hold[i,4], 2, 
                                         ifelse(hold[i,3] > hold[i,1] & hold[i,3] > hold[i,2] & hold[i,3] > hold[i,4], 3, 
                                                ifelse(hold[i,4] > hold[i,1] & hold[i,4] > hold[i,2] & hold[i,4] > hold[i,3], 4, NA))))
  }
  HabitatBreadth_m
}


f.family.DB <- function(data){ # determine mode for each Family for Diet Breadth
  hold <- table(data$Family, data$DietBreadth)
  DietBreadth_m <- vector(mode="numeric", length=dim(hold)[1])
  
  for(i in 1:dim(hold)[1]){
    DietBreadth_m[i] <- ifelse(hold[i,1] > hold[i,2] & hold[i,1] > hold[i,3] & hold[i,1] > hold[i,4] & hold[i,1] > hold[i,5] & hold[i,1] > hold[i,6] & hold[i,1] > hold[i,7], 1, 
                               ifelse(hold[i,2] > hold[i,1] & hold[i,2] > hold[i,3] & hold[i,2] > hold[i,4] & hold[i,2] > hold[i,5] & hold[i,2] > hold[i,6] & hold[i,2] > hold[i,7], 2, 
                                      ifelse(hold[i,3] > hold[i,1] & hold[i,3] > hold[i,2] & hold[i,3] > hold[i,4] & hold[i,3] > hold[i,5] & hold[i,3] > hold[i,6] & hold[i,3] > hold[i,7], 3, 
                                             ifelse(hold[i,4] > hold[i,1] & hold[i,4] > hold[i,2] & hold[i,4] > hold[i,3] & hold[i,4] > hold[i,5] & hold[i,4] > hold[i,6] & hold[i,4] > hold[i,7], 4, 
                                                    ifelse(hold[i,5] > hold[i,1] & hold[i,5] > hold[i,2] & hold[i,5] > hold[i,3] & hold[i,5] > hold[i,4] & hold[i,5] > hold[i,6] & hold[i,5] > hold[i,7], 5, 
                                                           ifelse(hold[i,6] > hold[i,1] & hold[i,6] > hold[i,2] & hold[i,6] > hold[i,3] & hold[i,6] > hold[i,4] & hold[i,6] > hold[i,5] & hold[i,6] > hold[i,7], 6, 
                                                                  ifelse(hold[i,7] > hold[i,1] & hold[i,7] > hold[i,2] & hold[i,7] > hold[i,3] & hold[i,7] > hold[i,4] & hold[i,7] > hold[i,5] & hold[i,7] > hold[i,6], 7, NA)))))))
  }
  DietBreadth_m
}


f.family.G <- function(data){ # determine mode for each Family for Guild
  hold <- table(data$Family, data$Guild)
  Guild_m <- vector(mode="numeric", length=dim(hold)[1])
  
  for(i in 1:dim(hold)[1]){
    Guild_m[i] <- ifelse(hold[i,2] > hold[i,3] & hold[i,2] > hold[i,4] & hold[i,2] > hold[i,5], 1, 
                         ifelse(hold[i,3] > hold[i,2] & hold[i,3] > hold[i,4] & hold[i,3] > hold[i,5], 2, 
                                ifelse(hold[i,4] > hold[i,2] & hold[i,4] > hold[i,3] & hold[i,4] > hold[i,5], 3, 
                                       ifelse(hold[i,5] > hold[i,2] & hold[i,5] > hold[i,3] & hold[i,5] > hold[i,4], 3, NA))))
  }
  Guild_m
}


f.family.avg <- function(data){ # calculate medians for each Family for continuous traits and combine with modes for each categorical trait
  Mass_m <- tapply(data$Mass, droplevels(data$Family), median, na.rm=TRUE)
  LitterSize_m <-tapply(data$LitterSize, droplevels(data$Family), median, na.rm=TRUE)
  GR_Area_m <- tapply(data$GR_Area, droplevels(data$Family), median, na.rm=TRUE)
  #BodyLength_m <- tapply(data$BodyLength, droplevels(data$Family), median, na.rm=TRUE)
  #HB_m <- tapply(data$HabitatBreadth, droplevels(data$Family), median, na.rm=TRUE)
  #DB_m <- tapply(data$DietBreadth, droplevels(data$Family), median, na.rm=TRUE)
  medians <- cbind(Mass_m, LitterSize_m, GR_Area_m)
  medians
  
  #HabitatBreadth_m <- f.family.HB(data)
  #DietBreadth_m <- f.family.HB(data)
  ActivityCycle_m <- f.family.AC(data)
  Guild_m <- f.family.G(data)
  modes <- cbind(ActivityCycle_m, Guild_m)
  rownames(modes) <- levels(data$Family)
  modes <- modes[match(rownames(medians), rownames(modes)),]
  fam_avg <- cbind(medians, modes)
  fam_avg
}

f.family.avg.bird <- function(data){ # calculate medians for each Family for continuous traits and combine with modes for each categorical trait
  Mass_m <- tapply(data$Mass, data$Family, median, na.rm=TRUE)
  Guild_m <- f.family.G(data)
  fam_avg <- cbind(Mass_m, Guild_m)
  fam_avg
}