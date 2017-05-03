C2 <- function(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,s2Target,c1){
  nargin = nargin <- length(as.list(match.call())) -1
  if(nargin<8){
    C1_output = C1(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL)
    c1 = C1_output[1]
    s1 = C1_output[2]
  }
  nbands = length(c1)
  c1BandImage = c1
  nfilts = size(c1[[1]])[3]
  n_rbf_centers = size(s2Target)[2]
  L = size(s2Target)[1] / nfilts;
  PatchSize = c(L^.5,L^.5,nfilts)
  
  s2 = cell(n_rbf_centers,1)
  
  for(iCenter in 1:n_rbf_centers){
    Patch = reshape(s2Target(1:size(s2Target)[1],iCenter),PatchSize)
    s2[[iCenter]] = cell(nbands, 1)
    for(iBand in 1:nbands){
      s2[[iCenter]][[iBand]] = WindowedPatchDistance(c1BandImage[[iBand]],Patch)
    }
  }
  c2 = rep(Inf, n_rbf_centers)
  for(iCenter in 1:n_rbf_centers){
    for(iBand in 1:nbands){
      c2[iCenter] = min(c2[iCenter],min(min(s2[[iCenter]][[iBand]])))
    }
  }
  output = list(c2,s2,c1,s1)
  return(output)
}