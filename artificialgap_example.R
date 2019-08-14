# load function
source("art_gap_for_public.R")

# load dataset
df <- read.csv("exampleWETBB.csv") 

# run function
gap <- art_gap(df$FCH4,gaprate=0.10,samplesize = 4,gaplength = 100,output.na =T)
plot(gap$orig)
points(gap$X1,col="red")
plot(gap$orig)
points(gap$X2,col="red")
plot(gap$orig)
points(gap$X3,col="red")
