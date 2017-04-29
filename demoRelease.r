#demoRelease.r
#demonstrates how to use C2 standard model features in a pattern classification framework

#optional clear workspace
rm(list = ls())

#source files
source('readAllImages.r')
source('extractRandC1Patches.r')
source('init_gabor.r')

#TODO
#addpath osu-svm/ %put your own path to osusvm here

useSVM <- FALSE #if you do not have osusvm installed you can turn this
                #to 0, so that the classifier would be a NN classifier
                #note: NN is not a great classifier for these features

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
  numPatchesPerSize <- 250  #more will give better results, but will
                            #take more time to compute
  cPatches <- extractRandC1Patches(cI$train_pos, numPatchSizes, numPatchesPerSize, patchSizes) #fix: extracting from positive only
  
  totaltimespectextractingPatches <- Sys.time() - tic
  t_str <- as.numeric(totaltimespectextractingPatches, units = "secs")
  print(paste('extractiong patches takes ', t_str, ' seconds'))
} else {
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

#TODO
#%The actual C2 features are computed below for each one of the training/testing directories
#tic
#for i = 1:4,
#C2res{i} = extractC2forcell(filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches,cI{i},numPatchSizes);
#toc
#end
#totaltimespectextractingC2 = toc;
#
#%Simple classification code
#XTrain = [C2res{1} C2res{2}]; %training examples as columns 
#XTest =  [C2res{3},C2res{4}]; %the labels of the training set
#ytrain = [ones(size(C2res{1},2),1);-ones(size(C2res{2},2),1)];%testing examples as columns
#ytest = [ones(size(C2res{3},2),1);-ones(size(C2res{4},2),1)]; %the true labels of the test set
#if useSVM
#Model = CLSosusvm(XTrain,ytrain);  %training
#[ry,rw] = CLSosusvmC(XTest,Model); %predicting new labels
#else %use a Nearest Neighbor classifier
#Model = CLSnn(XTrain, ytrain); %training
#[ry,rw] = CLSnnC(XTest,Model); %predicting new labels
#end  
#successrate = mean(ytest==ry) %a simple classification score
