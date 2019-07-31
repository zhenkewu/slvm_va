

#####------------------------------------------------------------------------------------
### This code loads in the PHMRC VA data and fits it to the RLCM developed in the rewind package 
#####------------------------------------------------------------------------------------

## load the libraries needed 
rm(list=ls())
library(rewind)
library(data.table)
library(mcclust.ext)

## set working directory to the location of the dataset 
setwd("~/Dropbox/Irena/verbal_autopsy_data/data_freeze/")

## load in VA data - the data values should all take one of these formats:
#1 (for yes), 0 (for no), or NA for missing values 
vaData <- data.table(read.csv("datafreeze_07292019_binary.csv"))

## For now, remove the demographic variables and only use the symptoms 
dropList <- c("site", "newid", "g1_06y", "g1_06m", "g1_06d")
vaMatrix <- vaData[, !colnames(vaData) %in% dropList, with=FALSE]
## only keep symptoms that have most of their data 
binaryMatrix <- vaMatrix[, colSums(is.na(vaMatrix)) <= 0.05*nrow(vaMatrix),with=FALSE]

## remove the rows that have at least 1 NA 
binarySubset <- na.omit(binaryMatrix)

sampleData  <- binarySubset[sample(nrow(binarySubset),600),]

## Keep the "gold standard" diagnoses for evaluation after the model is done 
truth_values <- sampleData[,c(1:2)]
sampleData$gs_text34 <- NULL
sampleData$Cause <- NULL

## convert the data frame into a matrix 
sampleData <- as.matrix(sampleData)

## Define paramaters of the model: 
L <- ncol(sampleData)
m_max <- 2 # start with a small number and increase if model suggests 

model_options <- list(
  n   = nrow(sampleData),
  t_max=34, ## the entire dataset has 34 defined GS causes of death 
  m_max = m_max,
  b  = 1, # Dirichlet hyperparameter; in the functions above,
  # we used "b" - also can be called "gamma"!.
  #Q  = simu$Q,
  a_theta = replicate(L, c(9,1)),
  a_psi   = replicate(L, c(1,9)),
  #theta = options_sim0$theta,
  #psi   = options_sim0$psi,
  alpha   = 0.1,
  frac = 0.2,
  #p_both      = rep(0.5,3),#,c(0.5,0.5^2,0.5^3,0.5^4,0.5^5)
  #p0 = rep(0.5,m_max0), # <--- this seems to make a difference in convergence.
  log_pk = "function(k) {log(0.1) + (k-1)*log(0.9)}"# Geometric(0.1).
  #Prior for the number of components.
)

# pre-compute the log of coefficients in MFM:
model_options$log_v<-mfm_coefficients(eval(parse(text=model_options$log_pk)),
                                       model_options$b,
                                       model_options$n,
                                       model_options$t_max+1)

# mcmc options:
mcmc_options <- list(
  n_total = 100,
  n_keep  = 5,
  n_split = 5,
  print_mod = 10,
  constrained = TRUE, # <-- need to write a manual about when these options are okay.
  block_update_H = TRUE,
  block_update_Q = !TRUE,
  ALL_IN_ONE_START =!TRUE,  # <--- TRUE for putting all subjects in one cluster,
  # FALSE by starting from a hierechical clustering
  # (complete linkage) and cut
  # to produce floor(t_max/4). Consider this as a warm start.
  MORE_SPLIT = TRUE,
  print_Q    = TRUE
)

# run posterior algorithm for data:
va_out <- sampler(sampleData, model_options,mcmc_options)

## save the output 
out0 <- va_out
va_out0 <- postprocess_H_Q(out0)


Z_SAMP_FOR_PLOT <- va_out0$z_sci_samp 

psm    <- comp.psm(t(Z_SAMP_FOR_PLOT))
# point estimate using all methods:
bmf.VI <- minVI(psm,t(Z_SAMP_FOR_PLOT),method="all",include.greedy=TRUE)
summary(bmf.VI)


# summarize Q (first need to transpose it)
#
# Approach 1: compute the error |QQ' - E[QQ'|Y]|_Frobneious
# Approach 2: compute the marginal co-activation probability
#             P(\sum_m Q_ml >= 1, \sum_m Q_ml' >= 1) -
#             This is invariant to the relabeling of latent states, or cluster labels.
nsamp_Q <- dim(out$Q_merge_samp)[3]
EQQ     <- matrix(0,nrow=dim(out$Q_merge_samp)[2],
                  ncol=dim(out$Q_merge_samp)[2]) # L by L.
EQQ_binary <- EQQ
# Approach 1:
for (iter in 1:nsamp_Q){
  A   <- t(out$Q_merge_samp[,,iter])
  EQQ <- (A%*%t(A)+EQQ*(iter-1))/iter # invariant to the row reordering of Q.
  EQQ_binary <- 0+(A%*%t(A)>0)+EQQ_binary
}

# Approach 2:
EQQ_percent <- EQQ_binary/nsamp_Q
image(EQQ_percent,col=hmcols,main="co-activation patterns across L dimensions")

# Approach 1:
Q_Em <- rep(NA,nsamp_Q)
for (iter in 1:nsamp_Q){
  A <- t(out$Q_merge_samp[,,iter])
  Q_Em[iter] <- norm(A%*%t(A) - EQQ,"F")
}
plot(Q_Em,type="o",main="||QQ'-E[QQ'|Y]||")

# choose the indices minimizing the errors:
ind_of_Q       <- which(Q_Em==min(Q_Em))

#
# visualize truth:
#
pdf(file.path("inst/example_figure/","bmf_truth.pdf"),width=12,height=6)
plot_truth(simu,options_sim0)
dev.off()


