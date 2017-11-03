require(ggplot2)
require(lsr)

# load the data when relevance is conveyed
load("./jagssamples_texture.Rdata")
G <- as.data.frame(gen)
names(G) <- paste0("Response_",names(G))
G$Stimulus <- 1:11
G$Sampling <- "Helpful"

load("./jagssamples_weak.Rdata")
G2 <- as.data.frame(gen)
names(G2) <- paste0("Response_",names(G2))
G2$Stimulus <- 1:11
G2$Sampling <- "Weak"
G <- rbind(G,G2)

gen <- wideToLong(G)
gen$Condition <- gsub("\\..*","",as.character(gen$within))
gen$Dimension <- gsub(".*\\.","",as.character(gen$within))
gen$within <- NULL
gen <- gen[,c(1,2,4,5,3)]

rm(G,G2,relevance,jags,maps,opt)

gen$Dimension <- gsub("shape","Shape",gen$Dimension,fixed=TRUE)
gen$Dimension <- gsub("colour","Colour",gen$Dimension,fixed=TRUE)

gen$Condition <- gsub("far","Distant Negative", gen$Condition)
gen$Condition <- gsub("near","Close Negative", gen$Condition)
gen$Condition <- gsub("single","Single Positive", gen$Condition)

gen$Condition <- as.factor(gen$Condition)
gen$Condition <- permuteLevels(gen$Condition, c(3,1,2))

gen <- gen[gen$Dimension !="checker",]

# pic
pic <- ggplot(data=gen, 
              mapping = aes(
                x=Stimulus,
                y=Response,
                color=Condition, 
                shape=Condition
              )
)
pic <- pic + facet_grid(Dimension ~ Sampling)
pic <- pic + geom_line() + geom_point(size=2,stroke=1.25)
pic <- pic + theme_bw() + theme(panel.grid = element_blank())

pic <- pic + scale_colour_manual(
  values = c(
    "Single Positive" = "grey50",
    "Close Negative" = "navy",
    "Distant Negative" = "maroon "
  ))

pic <- pic + scale_shape_manual(
  values = c(
    "Single Positive" = 8,
    "Close Negative" = 15,
    "Distant Negative" = 19
  ))

pic <- pic + geom_vline(xintercept=6, lty=3)
pic <- pic + geom_vline(aes(xintercept=xi),
                        data = data.frame(
                          xi = 4,
                          "Dimension"="Colour"), lty=3)
pic <- pic + ylim(0,1)
plot(pic)
