###################################
#time.handlers
###################################

#functions linked with pems.utils time handling 
###############################################
#some may assume you stick to the 
#time.stamp and local.time conventions...

#functions include
###############################################
#regularize
#getLocalTimeFromTimeStamp (not exported)
#repairTimeStamp
#


###################################
#regularize
###################################

#kr vr 0.1.2 17/11/2018

#originial written for 3datx/ce-cert work 
#tested msc Adrian 
#refined as part of work for Daisy's phd

#converts irregular time-series into regular time-series 
#can also hole-file 

regularize <- function (data, Hz=1, ...) 
{
  data <- rebuildPEMS(data, "old")
  test <- comment(isPEMS(data))
  if (!test %in% c("pems", "data.frame")) 
    stop("can't touch this")
  temp <- if (test == "pems") 
    pemsData(data)
  else data
  if (!"local.time" %in% names(temp)) 
    stop("missing required reference 'local.time'")
  x <- temp$local.time
  new.x <- (0:ceiling(x[length(x)])) 
  new.x <- (1:(length(new.x)*Hz)-1)/Hz
  new.df <- temp[1, ]
  new.df[length(new.x), 1] <- NA
  for (i in names(temp)) {
    if (is.numeric(temp[, i])) 
      new.df[, i] <- approx(x, temp[, i], new.x)$y
    if (is.factor(temp[, i]) || is.character(temp[, i])) {
      new.df[, i][which(new.x %in% x)] <- temp[, i]
      for (j in 2:length(new.df[, i])) 
        new.df[j, i] <- new.df[j - 1, i]
    }
    if (any(c("POSIXct", "POSIXt") %in% class(temp[, i]))) {
      new.df[, i] <- temp[1, i] + new.x
    }
  }
  if (test == "pems") {
    class(data) = "not.pems"
    data$data <- new.df
    class(data) <- "pems"
  }
  else data <- new.df
  data <- rebuildPEMS(data)
  return(data)
}



#####################################
#getLocalTimeFromTimeStamp
##################################### 

# kr (from various) vr 0.2.0 18/11/2018

getLocalTimeFromTimeStamp <- function(data, time.stamp=NULL, 
                                      ...){
  #input handling like binVSP?
  #if null, check if we have time.stamp?
  if(is.null(time.stamp)) time.stamp <- "time.stamp"
  temp <- as.numeric(data[time.stamp])
  #warning if any NAs, etc?
  temp <- temp-min(temp, na.rm=TRUE)
  pems.element(temp, units="s")
}

#####################################
#repairLocalTime
#####################################

# kr (from various) vr 0.2.0 18/11/2018

repairLocalTime <- function(data, local.time, ref, ...,
                            reset.count = TRUE,
                            fun.name="repairLocalTime"){
  #for local.time with NAs in it 
  #(structure from binVSP_NCSU.14)
  this.call <- match.call()
  settings <- calcChecks(fun.name=fun.name, ..., data = data)
  #get inputs
  local.time <- getPEMSElement(!!enquo(local.time), data, 
                        ref.name="local.time")
  ref <- getPEMSElement(!!enquo(ref), data, 
                        ref.name="ref")
  #repair
  #this assumes ref is displaced local.time 
  #like local.time.1 from 
  #d <- cAlign(~conc.nox, pems.1[100:200,], pems.1[96:220,])
  #d$local.time <- repairLocalTime(d, local.time, local.time.1)
  if(any(is.na(local.time))){
    prd <- predict(lm(local.time~ref), newdata=data.frame(ref=ref))
    temp <- rep(NA, length(local.time))
    temp[as.numeric(names(prd))] <- prd
    local.time[is.na(local.time)] <- temp[is.na(local.time)]
    #this currently returns original + prediction where original were NAs...
  }
  if(reset.count){
    local.time <- local.time - min(local.time, na.rm=TRUE)
  }
  if(any(is.na(local.time))) 
    warning("time.stamp repair not complete success...")
  #output
  pemsOutput(local.time, output = settings$output, data = data,  
             fun.name = fun.name, this.call = this.call)
}
