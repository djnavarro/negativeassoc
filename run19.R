
# set up
require(rjags,quietly = TRUE)
require(ggplot2)
require(lsr)

# simulation parameters
opt <- list(
  burnin = 10000,
  its = 10000,
  nchains = 10,
  ntrials = 12,
  thin = 10,
  relevance_type = "weak",
  saveSamples = FALSE,
  saveResults = TRUE
)

# for a particular "relevance type" return a matrix indicating which features
# are deemed relevant in each condition
fixedRelevance <- function(relevance_type) {
  
  if(relevance_type == "texture") { # the most salient (texture) feature is relevant
    rel <- rbind(
      "single" = c("TT" = 0, "SZ" = 0, "BG" = 1, "CH" = 0),
      "near"   = c("TT" = 0, "SZ" = 0, "BG" = 1, "CH" = 0),
      "far"    = c("TT" = 1, "SZ" = 0, "BG" = 0, "CH" = 0)
    )
  } else if(relevance_type == "alltexture") { # anything texture related is relevant
    rel <- rbind(
      "single" = c("TT" = 0, "SZ" = 0, "BG" = 1, "CH" = 0),
      "near"   = c("TT" = 0, "SZ" = 0, "BG" = 1, "CH" = 0),
      "far"    = c("TT" = 1, "SZ" = 0, "BG" = 1, "CH" = 1)
    )  
  } else if(relevance_type == "allexists") { # every detectable feature is releant
    rel <- rbind(
      "single" = c("TT" = 0, "SZ" = 1, "BG" = 1, "CH" = 0),
      "near"   = c("TT" = 0, "SZ" = 1, "BG" = 1, "CH" = 0),
      "far"    = c("TT" = 1, "SZ" = 1, "BG" = 1, "CH" = 1)
    )  
  } else if(relevance_type == "sillystrong") {  # everything in the world is relevant!
    rel <- matrix(
      1,3,4,dimnames = list(c("single","near","far"), 
                            c("TT","SZ","BG","CH"))
    )
  } else if(relevance_type == "weak") {  # everything irrelevant
    rel <- matrix(
      0,3,4,dimnames =  list(c("single","near","far"), 
                             c("TT","SZ","BG","CH"))
    )
  } else {
    stop("not a recognised fixed relevance type")
  }  
  return(rel)
}

# for a given condition, return a matrix containing the corresponding "hint" information
hintInformation <- function(condition) {
  
  if(condition == "single") {
    hints <- rbind(
      "exists"       = c("TT" = 0, "SZ" = 1, "BG" = 1, "CH" = 0),
      "varies_train" = c("TT" = 0, "SZ" = 0, "BG" = 0, "CH" = 0),
      "varies_test"  = c("TT" = 0, "SZ" = 1, "BG" = 1, "CH" = 0)
    )
  } else if(condition == "near") {
    hints <- rbind(
      "exists"       = c("TT" = 0, "SZ" = 1, "BG" = 1, "CH" = 0),
      "varies_train" = c("TT" = 0, "SZ" = 0, "BG" = 1, "CH" = 0),
      "varies_test"  = c("TT" = 0, "SZ" = 1, "BG" = 1, "CH" = 0)
    )
  } else if(condition == "far") {
    hints <- rbind(
      "exists"       = c("TT" = 1, "SZ" = 1, "BG" = 1, "CH" = 1),
      "varies_train" = c("TT" = 1, "SZ" = 0, "BG" = 0, "CH" = 0),
      "varies_test"  = c("TT" = 0, "SZ" = 1, "BG" = 1, "CH" = 0)
    )
  } else {
    stop("not a valid condition")
  }
  
  return(hints)
  
}

# create data structure that JAGS will use
makeJAGSData <- function(opt,condition){
  
  jagsData <- list(
    cs1 = rep.int(1,opt$ntrials), # dummy variable for the cs plus
    cs0 = rep.int(1,opt$ntrials), # dummy variable for the cs minus 
    edge = array(1,dim=c(11,11,2,2)), # 11 test stimuli
    ntrials = opt$ntrials,
    grain = 11
  )
  
  # if relevance is to be inferred, the JAGS model is given the "hints" info
  if(opt$relevance_type == "learned") {
    jagsData <- 
      c(jagsData, 
        list(
          TT_exists = opt$hints[[condition]]["exists","TT"], # hints from what "exists"
          SZ_exists = opt$hints[[condition]]["exists","SZ"],
          BG_exists = opt$hints[[condition]]["exists","BG"],
          CH_exists = opt$hints[[condition]]["exists","CH"],
          TT_trainvary = opt$hints[[condition]]["varies_train","TT"], # hints from what "varies at train"
          SZ_trainvary = opt$hints[[condition]]["varies_train","SZ"],
          BG_trainvary = opt$hints[[condition]]["varies_train","BG"],
          CH_trainvary = opt$hints[[condition]]["varies_train","CH"],  
          TT_testvary = opt$hints[[condition]]["varies_test","TT"], # hints from what "varies at test"
          SZ_testvary = opt$hints[[condition]]["varies_test","SZ"],
          BG_testvary = opt$hints[[condition]]["varies_test","BG"],
          CH_testvary = opt$hints[[condition]]["varies_test","CH"]
        )
      )
    
    # otherwise it is fed the information directly
  } else {
    jagsData$input_rel <- fixedRelevance(opt$relevance_type)[condition,]
  }
  
  return(jagsData) 
  
}

makeJAGSModelString <- function(condition, opt) {
  
  # load the skeleton of the model
  jagsmodelstring <- paste0(
    readLines(
      paste0("./jags/condition_",condition,".bug")
    ), 
    collapse="\n"
  )
  
  # insert code specifying the prior
  jagsmodelstring  <- gsub(
    "PRIORHERE",
    paste0(
      readLines("./jags/model_prior.bug"), 
      collapse="\n"
    ),
    jagsmodelstring,
    fixed=TRUE
  )
  
  # insert code specifying the likelihood
  jagsmodelstring <- gsub(
    "LIKELIHOODHERE",
    paste0(
      readLines("./jags/model_likelihood.bug"),
      collapse="\n"
    ),
    jagsmodelstring,
    fixed=TRUE
  )
  
  # insert code specifying the relevance model
  if (opt$relevance_type == "learned") {
    relevance_file <- "./jags/relevance_learned.bug"
  } else {
    relevance_file <- "./jags/relevance_fixed.bug"
  }
  jagsmodelstring <- gsub(
    "RELEVANCEHERE",
    paste0(
      readLines(relevance_file),
      collapse="\n"
    ),
    jagsmodelstring,
    fixed=TRUE
  )
  
  return(jagsmodelstring)
  
}

# construct JAGS model
makeJAGSModel <- function(jagsmodelstring, opt, jagsData) {
  
  jagsModel <- jags.model(
    file = textConnection(jagsmodelstring),
    n.adapt = opt$burnin,
    n.chains = opt$nchains,
    data = jagsData)
  
  return(jagsModel)
  
}

extendOptions <- function(opt) {
  
  # record the file name
  opt$filename <- paste0("jagssamples_", opt$relevance_type, ".Rdata")
  
  # check if this save file is already there!
  if((opt$saveSamples | opt$saveResults) & file.exists(opt$filename)) {
    stop(paste0("Data file '", opt$filename,"' already exists"))
  }
  
  # names for the training conditions and the stimulus features
  opt$condition <- c("single","near","far")
  opt$features <- c("colour","checker","shape")
  
  # what information is "accidentally/intentionally" conveyed
  # to the learner besides the mere fact of covariation?
  opt$hints <- list()
  opt$hints$single <- hintInformation("single")
  opt$hints$near <- hintInformation("near")
  opt$hints$far <- hintInformation("far")
  
  return(opt)
  
}

drawSamples <- function(jagsModel, opt) {
  jags.samples(
    model = jagsModel, 
    variable.names = c(
      "colourvalue","shapevalue","checkervalue","association",
      "lambda","smoothness",
      "TT_relevant","SZ_relevant","BG_relevant","CH_relevant"
    ), 
    n.iter = opt$its,
    thin = opt$thin
  )
}

saveData <- function(opt) {
  if(opt$saveResults == TRUE) {
    if(opt$saveSamples == TRUE) { # long form includes all samples
      save(samples,maps,jags,gen,relevance,opt,file=opt$filename)
    } else { # short form doesn't
      save(maps,jags,gen,relevance,opt,file=opt$filename)
    }
  }
}


plotGeneralisationCurves <- function(gen,opt){
  
  layout(matrix(1:3,1,3))
  col <- c("black", "blue", "red")
  for(i in 1:3) {
    plot.new()
    plot.window(xlim = c(0,11), ylim = c(0,1))
    title(main = opt$stimulusFeatures[i])
    box()
    axis(1)
    axis(2)
    lines(gen$single[[i]], ylim=c(0,1), lwd=2, type="l", col=col[1])
    lines(gen$near[[i]], ylim=c(0,1), lwd=2, type="l", col=col[2])
    lines(gen$far[[i]], ylim=c(0,1), lwd=2, type="l", col=col[3])
    abline(h=.5, lty=2)
  }
  layout(1)
  
  dev.print(
    device = pdf, 
    file = paste0("generalisation_", opt$relevance_type, ".pdf")
  )
  
}



### run the simulation & plot maps as we go ###

layout(matrix(1:6,2,3,FALSE))
gen <- list()
samples <- list()
relevance <- list()
jags <- list()
maps <- list()
opt <- extendOptions(opt)

for(condition in c("single","near","far")) {
  
  # jags setup
  jags[[condition]]$modelstring <- makeJAGSModelString(condition, opt)
  jags[[condition]]$data <- makeJAGSData(opt, condition)
  jags[[condition]]$model <- makeJAGSModel(
    jags[[condition]]$modelstring, opt, jags[[condition]]$data
  )
  
  # sample
  samples[[condition]] <- drawSamples(jags[[condition]]$model, opt)
  
  # store the generalisation gradients
  gen[[condition]] <- list()
  gen[[condition]]$colour <- summary(samples[[condition]]$colourvalue,mean)[[1]]
  gen[[condition]]$checker <- summary(samples[[condition]]$checkervalue,mean)[[1]]
  gen[[condition]]$shape <- summary(samples[[condition]]$shapevalue,mean)[[1]]
  
  # store the relevance values
  relevance[[condition]] <- c(
    "texture" = summary(samples[[condition]]$TT_relevant,mean)[[1]],
    "bluegreen" = summary(samples[[condition]]$BG_relevant,mean)[[1]],
    "checker" = summary(samples[[condition]]$CH_relevant,mean)[[1]],  
    "size" = summary(samples[[condition]]$SZ_relevant,mean)[[1]]
  )
  print(relevance[[condition]])
  
  # plot the associative maps
  value <- summary(samples[[condition]]$association,mean)[[1]]
  image(t(1-value[,,1]), zlim = c(0,1), main=condition, 
        ylab="shape", xlab="colour", col=cm.colors(15))
  image(t(1-value[,,2]), zlim = c(0,1), main=condition,
        ylab="shape", xlab="check", col=cm.colors(15))
  
  # store the average associative map for saving
  maps[[condition]] <- value
  
}
layout(1)

saveData(opt)
plotGeneralisationCurves(gen,opt)






