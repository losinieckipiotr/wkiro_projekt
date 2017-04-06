C1 <-
  function(stim,
           filters,
           fSiz,
           c1SpaceSS,
           c1ScaleSS,
           c1OL,
           INCLUDEBORDERS) {
    USECONV2 <- 1
    numScaleBands <- length(c1ScaleSS) + 1
    numScales <- tail(c1ScaleSS, n = 1) - 1
    
    numSimpleFilters = floor(length(fSiz) / numScales)
    for (iband in 1:numScaleBands) {
      ScalesInThisBand[[iBand]] <-
        c1ScalesSS(iBand):(c1SCaless(iband + 1) - 1)
    }
    
    
    ##Rebuild all filters (of all sizes)
    ##
    
    
    nFilts = length(fSiz)
    for (i in 1:nFilts) {
      sqfilter[[i]] <- matrix(filters(1:(fSiz(i) ^ 2), i), nrow = fSiz(i))
      if (USECONV2) {
        sqfilter[[i]] = tail(sqfilter[[i]], 2)
      }
    }
    
    ## Calculate all filter responses (s1)
    ##
    
    squim = stim ^ 2
    iUFilterIndex = 0
    
    uFiltSizes = unique(fSiz)
    
    for (i in 1:length(uFiltSizes)) {
      s1Norm[[uFiltSizes(i)]] = (sumfilter(squim, (uFiltSizes(i) - 1) / 2)) ^ 0.5
      s1Norm[[uFiltSizes(i)]] = s1Norm[[uFiltSizes(i)]] +!s1Norm[[uFiltSizes(i)]]
    }
    
    iUFilterIndex <- 0
    
    for (iBand in 1:numScaleBands) {
      for (iScale in 1:length(ScalesInThisBand[[iBand]])) {
        for (iFilt in 1:numSimpleFilters){
          iUFilterIndex = iUFilterIndex + 1
          if (!USECONV2) {
            s1[[iBand]][[iScale]][[iFilt]] <-
              abs(imfilter(stim, sqfilter[[iUFilterIndex]], 'symmetric', 'same', 'corr'))
          }
          if (USECONV2) {
            s1[[iBand]][[iScale]][[iFilt]] <-
              abs(conv2(stim, sqfilter[[iUFilterIndex]], 'same'))
          }
          if (!INCLUDEBORDERS) {
            s1[[iBand]][[iScale]][[iFilt]] <-
              removeborders(s1[[iBand]][[iScale]][[iFilt]], fSiz(iUFilterIndex))
          }
          s1[[iBand]][[iScale]][[iFilt]] <-
            as.double(s1[[iBand]][[iScale]][[iFilt]]) / s1Norm[[fSiz(iUFilterIndex)]]
          
        }
      }
    }
    
    ## Calculate local pooling (c1)
    ##
    
    for (iBand in  1:numScaleBands) {
      for (iFilt in 1:numSimpleFilters) {
        c1[[iBand]][, , iFilt] <-
          matrix(0, dim(s1[[iBand]][[1]][[iFilt]]))
        for (iScale in 1:ncol(ScalesInThisBand)[[iBand]]){
          c1[[iBand]][, , iFilt] = max(c1[[iBand]][, , iFilt], s1[[iBand]][[iScale]][[iFilt]])
        }
      }
    }
    
    
    ## 2) pool over local neighborhood
    ##
    for (iBand in 1:numScaleBands){
      poolRange <- (c1SpaceSS(iBand))
      for (iFilt in 1:numSimpleFilters){
        c1[[iBand]][, , iFilt] <-
          maxfilter(c1[[iBand]][, , iFilt]) ##, [0 0 poolRange - 1 poolRange - 1])
        
      }
    }
    
    ##(3) subsample
    ##
    
    for (iBand in 1:numScaleBands){
      sSS <- ceil(c1SpaceSS(iBand) / c1OL)
      rm(T)
      for (iFilt in 1:numSimpleFilters){
        T[, , iFilt] <- c1[[]](1:sSS:end, 1:sSS:end, iFilt)
      }
      c1[[iBand]] <- T
    }
  }
    

#removeborders <- function(sin, siz){
#    sin <- unpadimage(sin, [(siz + 1) / 2, (siz + 1) / 2, (siz - 1) / 2, (siz -1) / 2])
#    sin <- padarray(sin, [(siz + 1) / 2, (siz + 1) / 2], 0, 'pre')
#    sout <- padarray(sin, [(siz - 1) / 2, (siz - 1) / 2], 0, 'post')
#     }
    
  