

library(jpeg)
ima <- readJPEG(file.choose())
dim(ima)

r <- ima[,,1]
g <- ima[,,2]
b <- ima[,,3]

library(psych)

fr <- fa(r,nfactors = 100, scores =  "Bartlett" )
fg <- fa(g, nfactors = 100, scores =  "Bartlett" )
fb <- fa(b, nfactors = 100, scores =  "Bartlett" )

fli <- list(fr, fg, fb)

img_fa <- sapply(fli, function(i){
  yi <- i$scores %*% t(i$loadings)
}, simplify = "array")




writeJPEG(img_fa, "factbart.jpg")









#################
library(psych)
fr <- fa(r,nfactors = 400, scores =  "Anderson" )
fg <- fa(g, nfactors = 400, scores =  "Anderson" )
fb <- fa(b, nfactors = 400, scores =  "Anderson" )

fli <- list(fr, fg, fb)

img_fa <- sapply(fli, function(i){
  yi <- i$scores[,1:200] %*% t(i$loadings[,1:200])
}, simplify = "array")


writeJPEG(img_fa, "factand.jpg")

#################   Thurston




Image_factor <- function(Image, factors ){
  r <- Image[,,1]
  g <- Image[,,2]
  b <- Image[,,3]
  pcr <- fa(r,nfactors = factors, scores =  "Anderson")
  pcg <- fa(g,nfactors = factors, scores =  "Anderson")
  pcb <- fa(b,nfactors = factors, scores =  "Anderson")
  pr_list <- list(pcr, pcg, pcb)
  pc_im <- sapply(
    
    pr_list,  function(i){
      img <-  i$scores %*% t(i$loadings)
    },
    simplify = "array"
  )
  return( writeJPEG(pc_im,paste0(Image,"fac_comp_",factors,".jpg")))
}  