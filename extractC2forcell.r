extractC2forcell <- function(filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches,cImages,numPatchSizes){
  
  require('matlab')
  
  numPatchSizes = min(numPatchSizes,length(cPatches))
  print("Done extrC2_1")
  for (i in 1:numPatchSizes){
    siz = size(cPatches[[i]])[1]
    numpatch = size(cPatches[[i]])[2]
    siz = sqrt(siz/4)
    
    for(j in 1:numpatch){
      tmp = matrix(cPatches[[i]][1:size(cPatches)[1],j], c(siz, siz, 4))
      tmp = rec(tmp)
      cPatches[[i]][1:size(cPatches)[1],j] = tmp[1:size(tmp)[1]]
    }
  }
  
  mC2 = NULL;
  
  for(i in 1:length(cImages)){
    stim = cImages[[i]]
    img_siz = size(stim)
    c1 = NULL
    iC2 = NULL
    for(j in 1:numPatchSizes){
      if(is.null(c1)){
        c2_output = C2(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches[[j]])
        tmpC2 = c2_output[1]
        tmp = c2_output[2]
        c1 = c2_output[3]
      }else{
        tmpC2 = C2(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches[[j]],c1) 
      }
      iC2 = rbind(iC2, tmpC2)
    }
    mC2 = c(mC2, iC2)
  }
  return(mC2)
}