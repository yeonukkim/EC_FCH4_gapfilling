############################################################
# Generating artificial gaps
############################################################
library(dplyr)
art_gap <- function(orig, gaprate = 0.10, samplesize = 5, gaplength = 32, output.na = T){
	
	# orig: variable in vector
	# gaprate: percentage of gap to generate (default: 10%)
	# samplesize: sample size (default: 5) 
	# gaplength: length of gap in hour (default: 32 hours)
	# output.na: whether sampled artificial gap identified as "NA" or "artgap" (default=T)
	
	
	df <- data.frame(replicate(samplesize,orig))
	df <- data.frame(orig,df)
	
	# gaplength in half hourly scale
	l <- gaplength * 2
	
	# artificial gap generator
	for (i in c(2:(samplesize+1) )){
		measured <- which(!is.na(df$orig))
		targetlength <- length(measured)*(1-gaprate)
		
		repeat{
			s <- sample(measured, size = 1, replace = F)
			s1 <- c(s:(s+l))
			
			use <- df[,i]
			use[s1] <- "artgap"
			
			# the artificial gap should not overlap each other
			if( !(df[s,i]=="artgap") & (!(df[s+l,i]=="artgap") | is.na(df[s+l,i])) ){
				
				# the artificial gap should within the size of dataset
			  if((s+l) <= nrow(df)){
			  	df[,i] <- use
					measured <- measured[!(measured %in% s1)]
			  }
			}
			if(length(measured) <= targetlength) {
				break
			}
		}
		
		# output format
		if(output.na){
			df[which(df[,i]=="artgap"),i] <- NA
		}
	}
	
	return(df)
}
