C1 <- function(stim,
               filters,
               fSiz,
               c1SpaceSS,
               c1ScaleSS,
               c1OL,
               INCLUDEBORDERS = 0) {
  #TODO
  #docs

  require('matlab')
  require('EBImage')
  #source('unpadimage.r')
  #source('sumfilter.r')
  source('conv2same.r')
  debugSource('unpadimage.r')
  debugSource('sumfilter.r')
  #debugSource('conv2same.r')

  USECONV2 <- 0
  numScaleBands <- length(c1ScaleSS) - 1  #convention: last element in c1ScaleSS is max index + 1 
  numScales <- tail(c1ScaleSS, n = 1) - 1
  # last index in scaleSS contains scale index where next band would start, i.e., 1 after highest scale!!
  numSimpleFilters <- floor(length(fSiz) / numScales)
  ScalesInThisBand <- list()
  for (iBand in 1:numScaleBands) {
    ScalesInThisBand[[iBand]] <- c1ScaleSS[iBand]:(c1ScaleSS[iBand + 1] - 1)
  }
    
  ##Rebuild all filters (of all sizes)
  ##
  nFilts <- length(fSiz)
  sqfilter <- list()
  for (i in 1:nFilts) {
    temp_m <- matrix(filters[1:(fSiz[i]^2), i], nrow = fSiz[i], ncol = fSiz[i])
    if (USECONV2) {
      temp_m <- temp_m[fSiz[i]:1, fSiz[i]:1] #flip in order to use conv2 instead of imfilter (%bug_fix 6/28/2007);
    }
    sqfilter[[i]] <- temp_m
  }

  ## Calculate all filter responses (s1)
  ##
  sqim <- stim ^ 2
  iUFilterIndex <- 0
  # precalculate the normalizations for the usable filter sizes
  uFiltSizes <- sort(unique(fSiz))
  s1Norm <- list()
  remove_zeroes <- function(x) { if (x == 0) { x <- 1 } else { x <- x } }
  tic <- Sys.time()
  for (i in 1:length(uFiltSizes)) {
    temp_m <- (sumfilter(sqim,(uFiltSizes[i] - 1) / 2)) ^ 0.5
    #avoid divide by zero
    s1Norm[[uFiltSizes[i]]] <- apply(temp_m, 1:2, remove_zeroes)
  }
  totaltimespectextractingPatches <- Sys.time() - tic
  t_str <- as.numeric(totaltimespectextractingPatches, units = "secs")
  print(paste('calc sumfilters takes ', t_str, ' seconds'))
  
  s1 <- list()
  for (iBand in 1:numScaleBands) {
    s1[[iBand]] = list()
    for (iScale in 1:length(ScalesInThisBand[[iBand]])) {
      s1[[iBand]][[iScale]] = list()
      for (iFilt in 1:numSimpleFilters) {
        s1[[iBand]][[iScale]][[iFilt]] = list()
        iUFilterIndex <- iUFilterIndex + 1
        if (!USECONV2) {
          i_stim <- t(stim)
          i_stim <- Image(data = i_stim, dim(i_stim), colormode = 0)
          f_img <- abs(filter2(i_stim, t(sqfilter[[iUFilterIndex]]), boundary = 'replicate'))
          #TODO remove display
          display(f_img)
          f_img <- t(as.array(f_img))
          
          if(!INCLUDEBORDERS) {
            f_img <- removeborders(f_img, fSiz[iUFilterIndex])
          }
          #TODO remove display
          #temp = Image(data = t(f_img), dim(i_stim), colormode = 0)
          #display(temp)
          s1[[iBand]][[iScale]][[iFilt]] <- as.double(f_img) / s1Norm[[fSiz[iUFilterIndex]]]
          
        } else { #not 100% compatible but 20% faster at least
          stop('computation will be very slow')
          #s1[[iBand]][[iScale]][[iFilt]] <- abs(conv2same(stim, sqfilter[[iUFilterIndex]]))
          #if (!INCLUDEBORDERS) {
          #  s1[[iBand]][[iScale]][[iFilt]] <- removeborders(s1[[iBand]][[iScale]][[iFilt]], fSiz(iUFilterIndex))
          #}
          #s1[[iBand]][[iScale]][[iFilt]] <- as.double(s1[[iBand]][[iScale]][[iFilt]]) / s1Norm[[fSiz[iUFilterIndex]]]
        }
      }
    }
  }
  #TO DO the rest
    
  ## Calculate local pooling (c1)
  ##
  c1 <- list()
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

    return(list(c1, s1))
  }

removeborders <- function(sin, siz) {
  sin <- unpadimage(sin, c((siz + 1) / 2, (siz + 1) / 2, (siz - 1) / 2, (siz -1) / 2))
  sin <- padarray(sin, c((siz + 1) / 2, (siz + 1) / 2), 0, 'pre')
  sout <- padarray(sin, c((siz - 1) / 2, (siz - 1) / 2), 0, 'post')
}
