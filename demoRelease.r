#demoRelease.r
#demonstrates how to use C2 standard model features in a pattern classification framework

#optional clear workspace
rm(list = ls())

DEBUG <- FALSE
if (DEBUG) {
  debugSource('readAllImages.r')
  debugSource('extractRandC1Patches.r')
  debugSource('init_gabor.r')
  debugSource('extractC2forcell.r')
}
source('readAllImages.r')
source('extractRandC1Patches.r')
source('init_gabor.r')
source('extractC2forcell.r')

useSVM <- FALSE
READPATCHESFROMFILE <- FALSE  #use patches that were already computed
                              #(e.g., from natural images)

patchSizes <- c(4, 8, 12, 16) #other sizes might be better, maybe not
                              #all sizes are required

numPatchSizes <- length(patchSizes)

#specify directories for training and testing images
train_pos <- 'test_base/pos1'
train_neg <- 'test_base/neg1'
test_pos  <- 'test_base/pos2'
test_neg  <- 'test_base/neg2'

cI <- readAllImages(train_pos, train_neg, test_pos, test_neg) #cI is a cell containing
                                                              #all training and testing images
if (length(cI$train_pos) == 0 | length(cI$train_neg) == 0) {
  stop('No training images were loaded -- did you remember to change the path names?')
}

#below the c1 prototypes are extracted from the images/ read from file
if (!READPATCHESFROMFILE) {
  tic <- Sys.time()
  numPatchesPerSize <- 1  #more will give better results, but will
                            #take more time to compute
  cPatches <- extractRandC1Patches(cI$train_pos, numPatchSizes, numPatchesPerSize, patchSizes) #fix: extracting from positive only
  
  totaltimespectextractingPatches <- Sys.time() - tic
  t_str <- as.numeric(totaltimespectextractingPatches, units = "secs")
  print(paste('extracting patches takes ', t_str, ' seconds'))
} else {
  stop('NOT IMPLEMENTED')
  #TODO
  #print('reading patches');
  #cPatches = load('PatchesFromNaturalImages250per4sizes','cPatches');
  #cPatches = cPatches.cPatches;
}

#----Settings for Testing --------#
rot <- c(90, -45, 0, 45)
c1ScaleSS <- seq(1, 18, 2)
RF_siz    <- seq(7, 39, 2)
c1SpaceSS <- seq(8, 22, 2)
minFS     <- 7
maxFS     <- 39
div       <- seq(4, 3.2, -0.05)
Div       <- div
#--- END Settings for Testing --------#

print('Initializing gabor filters -- full set...')
#creates the gabor filters use to extract the S1 layer
init_gabor_ret_val  <- init_gabor(rot, RF_siz, Div)
fSiz                <- init_gabor_ret_val[[1]]
filters             <- init_gabor_ret_val[[2]]
c1OL                <- init_gabor_ret_val[[3]]
numSimpleFilters    <- init_gabor_ret_val[[4]]
print('done')

#The actual C2 features are computed below for each one of the training/testing directories
totaltimespectextractingC2 <- 0
C2res <- list()
tic <- Sys.time()
for (i in 1:4) {
  C2res[[i]] = extractC2forcell(filters,
                                fSiz,
                                c1SpaceSS,
                                c1ScaleSS,
                                c1OL,
                                cPatches,
                                cI[[i]],
                                numPatchSizes)
  extractC2forcellTime <- Sys.time() - tic
  t_str <- as.numeric(extractC2forcellTime, units = "secs")
  totaltimespectextractingC2 <- t_str + totaltimespectextractingC2
  print(paste('extracting C2 takes ', t_str, ' seconds'))
}

#Simple classification code
XTrain <- cbind(C2res[[1]], C2res[[2]]) #training examples as columns
XTest  <-  cbind(C2res[[3]], C2res[[4]]) #the labels of the training set
ytrain <- matrix(c(rep(1, dim(C2res[[1]])[2]), rep(-1, dim(C2res[[2]])[2])), ncol = 1)
ytest  <- matrix(c(rep(1, dim(C2res[[3]])[2]), rep(-1, dim(C2res[[4]])[2])), ncol = 1)

ry <- c()
if (useSVM) {
  require('e1071')
  train <- t(XTrain)
  test <- t(XTest)
  cl <- factor(ytrain)
  model <- svm(train, cl)
  pred <- predict(model, test)
  ry <- as.integer(as.vector(pred))
} else { #use a Nearest Neighbor classifier
  require('class')
  train <- t(XTrain)
  test <- t(XTest)
  cl <- factor(ytrain)
  knn_ret <- knn(train, test, cl)
  ry <- as.integer(as.vector(knn_ret))
}

successrate <- mean(as.integer(ytest == ry)) #a simple classification score
print(successrate)