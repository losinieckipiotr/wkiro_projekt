extractRandC1Patches <- function(cItrainingOnly, numPatchSizes, numPatchesPerSize, patchSizes) {
  #extracts random prototypes as part of the training of the C2 classification 
  #system. 
  #Note: we extract only from BAND 2. Extracting from all bands might help
  #cPatches the returned prototypes
  #cItrainingOnly the training images
  #numPatchesPerSize is the number of sizes in which the prototypes come
  #numPatchesPerSize is the number of prototypes extracted for each size
  #patchSizes is the vector of the patche sizes
  
  require('matlab')
  #source('C1.r')
  debugSource('C1.r')
  cPatches <- list()
  
  #TODO some default values
  #if nargin<2
  #numPatchSizes = 4;
  #numPatchesPerSize = 250;
  #patchSizes = 4:4:16;
  #end
  
  nImages <- length(cItrainingOnly)
  
  #----Settings for Training the random patches--------#
  rot <- c(90, -45, 0, 45)
  c1ScaleSS <- c(1, 3)
  RF_siz    <- c(11, 13)
  c1SpaceSS <- c(10)
  minFS     <- 11
  maxFS     <- 13
  div       <- seq(4, 3.2, -0.05)
  Div       <- div[3:4]
  #--- END Settings for Training the random patches--------#
  
  print('Initializing gabor filters -- full set...')
  init_gabor_ret_val  <- init_gabor(rot, RF_siz, Div)
  fSiz                <- init_gabor_ret_val[[1]]
  filters             <- init_gabor_ret_val[[2]]
  c1OL                <- init_gabor_ret_val[[3]]
  numSimpleFilters    <- init_gabor_ret_val[[4]]
  print('done')
  
  cPatches <- list()
  bsize <- c(0, 0)
  
  pind <- zeros(numPatchSizes,1)
  for (j in 1:numPatchSizes) {
    cPatches[j] <- list(zeros(patchSizes[j]^2*4,numPatchesPerSize))
  }
  
  for (i in 1:numPatchesPerSize) {
    ii <- floor(runif(1)*nImages) + 1
    print('.')
    stim <- cItrainingOnly[[ii]];
    img_siz <- size(stim);
    
    #TODO check C1 function
    #################NOT TESTED#############
    c1_reval <- C1(stim, filters, fSiz, c1SpaceSS, c1ScaleSS, c1OL)
    c1source <- c1_reval[[1]]
    #s1source <- c1_reval[[2]] #it seems that it is unused
    b = c1source[[1]]; #new C1 interface;
    bsize[1] <- size(b,1)
    bsize[2] <- size(b,2)
    for (j in 1:numPatchSizes) {
      xy <- floor(runif(2)*(bsize-patchSizes[j]))+1
      tmp <- b[seq(from=xy[1],length.out=patchSizes(j)),seq(from=xy[2],length.out=patchSizes(j)),];
      pind[j] <- pind[j] + 1
      cPatches[[j]][,pind[j]] <- reshape(tmp, size(tmp))
    }
    ########################################
  }
  
  return(cPatches)
}
