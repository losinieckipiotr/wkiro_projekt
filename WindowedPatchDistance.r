WindowedPatchDistance <- function(Im,Patch){
  dIm = size(Im)[3]
  dPatch = size(Im)[3]
  if(dIm != dPatch){
    print('The patch and image must be of the same number of layers')
  }
  
  s = size(Patch)[1]
  s[3] = dIm
  Psqr = sum(sum(sum(Patch.^2)))
  Imsq = Im^2
  Imsq = sum(Imsq,3)
  sum_support = c(ceil(s[2]/2)-1,ceil(s[1]/2)-1,floor(s[2]/2),floor(s[1]/2))
  Imsq = sumfilter(Imsq,sum_support)
  PI = zeros(size(Imsq)[1])
  
  for(i in 1:dIm){
    PI = PI + conv2(Im(1:size(Im)[1],1:size(Im)[2],i),Patch(1:size(Patch)[1],1:size(Patch)[2],i), 'same')
  }
  
  D = Imsq - 2 * PI + Psqr + 10^-10
  return(D)
}