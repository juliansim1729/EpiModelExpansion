trad_classgen <- function(studentGrades, avgClassSize, scheduleLength = 9, 
                          gradeHomophily = 1, lunchPeriods = c(4, 5, 6)) {
  
  # instantiate empty df with student attributes
  df <- data.frame(grade = studentGrades)
  
  studAttrLength <- ncol(df)
  
  # and then requested amount of periods
  n <- 1:scheduleLength
  df[paste0('pd', n)] <- do.call(cbind, lapply(n, function(x) 0))
  
  # set up lunch schedule
  nStudents <- length(studentGrades)
  nLunches <- length(lunchPeriods)lunches <- sample(c(rep(lunchPeriods, nStudents/nLunches), 
                      sample(lunchPeriods, nStudents %% nLunches)))
  
  for (i in 1:length(lunches)) {
    df[i, lunches[i] + studAttrLength] <- "lunch"
  }
  
  # TODO: class vs classrom
  # TODO: cohorts
  # populate with classrooms
  classID <- 0 
  for (grades in unique(studentGrades)) {
    for (period in 1:scheduleLength) {
      gStuds <- sample(which(studentGrades == grades))
      nStuds <- length(gStuds)
      
      classes <- round(nStuds/avgClassSize)
      remStuds <- sample(c(rep(1, nStuds %% classes), rep(0, classes - (nStuds %% classes))))
      if (!(period %in% lunchPeriods)) {
        toProcess <- 0
        for (class in 1:classes) {
          for (i in 1:(floor(nStuds/classes) + remStuds[class])) {
            df[gStuds[toProcess + i], period + studAttrLength] <- 
              paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))
          }
          df[toProcess:toProcess + floor(nStuds/classes) + remStuds[class] - 1, 
             period + studAttrLength] <- 
            paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))

          toProcess <- toProcess + floor(nStuds/classes) + remStuds[class]
          
          classID <- classID + 1
        }
      }
    }
  }
  # populate lunch periods
  for (grades in unique(studentGrades)) {
  
    for (period in lunchPeriods) {
      # need to calculate based on each period 
      gStuds <- sample(which(studentGrades == grades & 
                               df[, period + studAttrLength ] != "lunch"))
      nStuds <- length(gStuds)
    
      classes <- round(nStuds/avgClassSize)
      
      if (classes == 0) {
        classes <- 1
      }
      
      remStuds <- sample(c(rep(1, nStuds %% classes), rep(0, classes - (nStuds %% classes))))
      
      toProcess <- 0
      for (class in 1:classes) {
        
        for (i in 1:(floor(nStuds/classes) + remStuds[class])) {
          if (df[gStuds[toProcess + i], period + studAttrLength] != 'lunch') {
            df[gStuds[toProcess + i], period + studAttrLength] <- 
              paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))
          }
        }
        if (df[toProcess:toProcess + floor(nStuds/classes) + remStuds[class] - 1, 
               period + studAttrLength] != 'lunch') {
          df[toProcess:toProcess + floor(nStuds/classes) + remStuds[class] - 1, 
             period + studAttrLength] <- 
            paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))
        }
        
        toProcess <- toProcess + floor(nStuds/classes) + remStuds[class]
        
        classID <- classID + 1
      }
    }
  }
  lunchTimes <- which(df == 'lunch', arr.ind = TRUE)
  lunchTimes <- lunchTimes[order(lunchTimes[,1], decreasing = FALSE), ]
  
  df['lunch'] <- lunchTimes[,2] - studAttrLength
  df['lunch'] <- lunchTimes[,2] - studAttrLength
  
  
  return(df)
}
# newdf <- trad_classgen(sampGrades, 20)

block_classgen <- function(studentGrades, avgClassSize, scheduleLength = 9, 
               gradeHomophily = 1, lunchPeriods = c(5, 6, 7)) {
  # note that periods 1, 2 correspond to block 1, periods 3, 4 correspond to
  # block 2, etc...
  # instantiate empty df with student attributes
  df <- data.frame(grade = studentGrades)
  
  studAttrLength <- ncol(df)
  
  # and then requested amount of periods
  n <- 1:scheduleLength
  df[paste0('pd', n)] <- do.call(cbind, lapply(n, function(x) 0))
  
  # set up lunch schedule
  nStudents <- length(studentGrades)
  nLunches <- length(lunchPeriods)
  
  lunches <- sample(c(rep(lunchPeriods, nStudents/nLunches), 
                      sample(lunchPeriods, nStudents %% nLunches)))
  
  for (i in 1:length(lunches)) {
    df[i, lunches[i] + studAttrLength] <- "lunch"
  }
  
  classID <- 0 
  for (grades in unique(studentGrades)) {
    for (period in 1:scheduleLength) {
      gStuds <- sample(which(studentGrades == grades))
      nStuds <- length(gStuds)
      
      classes <- round(nStuds/avgClassSize)
      remStuds <- sample(c(rep(1, nStuds %% classes), rep(0, classes - (nStuds %% classes))))
      if (!(period %in% lunchPeriods)) {
        toProcess <- 0
        for (class in 1:classes) {
          for (i in 1:(floor(nStuds/classes) + remStuds[class])) {
            df[gStuds[toProcess + i], period + studAttrLength] <- 
              paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))
          }
          df[toProcess:toProcess + floor(nStuds/classes) + remStuds[class] - 1, 
             period + studAttrLength] <- 
            paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))
          
          toProcess <- toProcess + floor(nStuds/classes) + remStuds[class]
          
          classID <- classID + 1
        }
      }
    }
  }
  
  # populate lunch periods
  for (grades in unique(studentGrades)) {
    
    for (period in lunchPeriods) {
      # need to calculate based on each period 
      gStuds <- sample(which(studentGrades == grades & 
                               df[, period + studAttrLength ] != "lunch"))
      nStuds <- length(gStuds)
      
      classes <- round(nStuds/avgClassSize)
      
      if (classes == 0) {
        classes <- 1
      }
      
      remStuds <- sample(c(rep(1, nStuds %% classes), rep(0, classes - (nStuds %% classes))))
      
      toProcess <- 0
      for (class in 1:classes) {
        
        for (i in 1:(floor(nStuds/classes) + remStuds[class])) {
          if (df[gStuds[toProcess + i], period + studAttrLength] != 'lunch') {
            df[gStuds[toProcess + i], period + studAttrLength] <- 
              paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))
          }
        }
        if (df[toProcess:toProcess + floor(nStuds/classes) + remStuds[class] - 1, 
               period + studAttrLength] != 'lunch') {
          df[toProcess:toProcess + floor(nStuds/classes) + remStuds[class] - 1, 
             period + studAttrLength] <- 
            paste0(sprintf("%02d", grades), "_", sprintf("%03d", classID))
        }
        
        toProcess <- toProcess + floor(nStuds/classes) + remStuds[class]
        
        classID <- classID + 1
      }
    }
  }
  
  # TODO: generalize
  # convert to block
  block_df <- df
  lunchTimes <- which(df == 'lunch', arr.ind = TRUE)
  lunchTimes <- lunchTimes[order(lunchTimes[,1], decreasing = FALSE), ]
  
  block_df['lunch'] <- lunchTimes[,2] - studAttrLength
  block_df['lunch'] <- lunchTimes[,2] - studAttrLength
  
  # first lunch takes pd6 (2nd) classes, second lunch takes pd7 (3rd) classes,
  # third lunch takes pd5 (1st) classes
  for (period in lunchPeriods) {
    block_df[which(block_df['lunch'] == period),]['pd5'] <- 
      block_df[which(block_df['lunch'] == period),][,studAttrLength + min(lunchPeriods) + ((period - min(lunchPeriods) + 1) %% length(lunchPeriods))]
  }
  
  bdf <- block_df[,c('grade', 'pd1', 'pd3', 'pd5', 'pd8', 'lunch')]
  colnames(bdf) <- c('grade', 'block1', 'block2', 'block3', 'block4', 'lunch')
  
  
  
  return(bdf)
}