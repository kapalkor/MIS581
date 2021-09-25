
# In your folder of choice with 
# File RQ3.csv (with col headers StoryPts, Priority, and LeadTime)
# run at command line with:
# "C:\Program Files\R\R-4.1.1\bin\Rscript.exe" MIS581.R <file.csv>

# Some global values.
  message ("... starting ...")
  rm(list=ls())  
  graphics.off()  
  options(scipen=999)  
  args = commandArgs(trailingOnly=TRUE)

# Import dataset and rename.
  message ("... ingesting given data ", args[1], " ... ")
  myDF <- read.csv(args[1], header = TRUE)  
  names(myDF)[1] <- 'Y'
  names(myDF)[2] <- 'X1'
  names(myDF)[3] <- 'X2'

# Show var names, types, and counts.
  message ("... reviewing renamed data ...")
  message (" ")
stat <- data.frame(
           Class  = sapply( myDF, class ),
           Length = sapply( myDF, length ),
           UniqCnt= sapply( myDF, function(x)length(unique(x)) ),
           Missing= sapply( myDF, 
                            function(x)sum(length(which(is.na(x))))) )  
stat[order(stat$Class),]  
rm(stat) # tidy

# Run linear model.  
  message (" ")
  message ("... running model ...")
  myFit <- lm(Y ~ X1 + X2, myDF)
  summary(myFit)

message ("ending ...")


