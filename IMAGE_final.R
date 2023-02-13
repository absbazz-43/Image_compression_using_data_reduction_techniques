library(jpeg)
tiger <- readJPEG(file.choose())
r <- tiger[,,1]
g <- tiger[,,2]
b <- tiger[,,3]
tiger_r <- prcomp(r, center = FALSE)
tiger_g <- prcomp(g, center = FALSE)
tiger_b <- prcomp(b, center = FALSE)

rgb <- list(tiger_r, tiger_g, tiger_b)
pca.img <- sapply(rgb, function(j) {
  compressed.img <- j$x[,1:90] %*% t(j$rotation[,1:90])
}, simplify = 'array')

writeJPEG(pca.img, paste0('Compressed_image_with_',10, '_components.jpg'))

################## using singular value decomposion

library(raster)
im <- raster(file.choose())
im
image(im)
img.flip<-flip(im, direction = "y")
rasterimg<-t(as.matrix(img.flip))
dim(rasterimg)
svdimg<-svd(rasterimg)

U<-svdimg$u
d<-svdimg$d
V<-svdimg$v
U1 <- as.matrix(U[, 1:20])
#d1 <- as.matrix(d[1:118])
V1 <- as.matrix(V[, 1:20])

comp = U1 %*% diag(d[1:20]) %*% t(V1)
image(comp)

writeJPEG(comp, "comp.jpg")


Svd_raster <- function(values){
  U<-svdimg$u
  d<-svdimg$d
  V<-svdimg$v
  U1 <- as.matrix(U[,1:values])
  V1 <- as.matrix(V[,1:values])
  commp <- U1 %*% diag(d[1:values]) %*% t(V1)
  return(image(commp))
}

Svd_raster(50)

###################  SVD Other way 

r_s <- svd(r)
g_s <- svd(g)
b_s <- svd(b)

rgb_s <- list(r_s, g_s, b_s)

svd_image <- sapply(rgb_s, function(i){
  img <- i$u[,1:110] %*% diag(i$d[1:110]) %*% t(i$v[,1:110])
}, simplify = "array")


svd_func <- function(num){
  svd_image <- sapply(rgb_s, function(i){
    img <- i$u[,1:num] %*% diag(i$d[1:num]) %*% t(i$v[,1:num])
  }, simplify = "array")
  return(svd_image)
}

writeJPEG(svd_func(30),"gh.jpg")

writeJPEG(svd_image, "svdj.jpg")

###################  factor analysis 

###############


r_f <- fa(r, nfactors = 10)
g_f <- fa(g, nfactors=10)
b_f<- fa(b, nfactors = 10)

rgb_f <- list(r_f, g_f, b_f)
#image_factors$scores[, 1:num_factors] %*% t(image_factors$loadings[, 1:num_factors])
num_factors = 8
fac_image <- sapply(rgb_f, function(i){

  img <- i$scores[, 1:num_factors] %*% t(i$loadings[, 1:num_factors])
}, simplify = "array")


writeJPEG(fac_image, "fac.jpg")



################################## function of factor analysis final

library(psych)
fac_image <- function(factors){
  
  r_f <- fa(r, nfactors = factors)
  g_f <- fa(g, nfactors=factors)
  b_f<- fa(b, nfactors = factors)
  
  rgb_f <- list(r_f, g_f, b_f)
  #image_factors$scores[, 1:num_factors] %*% t(image_factors$loadings[, 1:num_factors])
  fac_image <- sapply(rgb_f, function(i){
    
    img <- i$scores[, 1:factors] %*% t(i$loadings[, 1:factors])
  }, simplify = "array")
  
  return(fac_image)
}

writeJPEG(fac_image(100), paste0("image",100,".jpg", collapse =""))

r_f <- fa(r, nfactors = 10)




getwd()

#####################################################
gh =pca(r,nfactors = 10)
gh$loadings
factor.plot(gh)
factor.scores(r)

screeplot(r_f)
factanal()

paste("image_","_man", collapse = " ")


image_factors <- fa(rasterimg, nfactors = 100)
image_factors$factors
# Select the number of factors to keep
num_factors <- 70

# Compress the image by keeping only the top `num_factors` factors
compressed_image_matrix <- image_factors$scores[, 1:num_factors] %*% t(image_factors$loadings[, 1:num_factors])

# Reshape the compressed image matrix back into an image
compressed_image <- matrix(compressed_image_matrix, nrow = dim(rasterimg)[1], ncol = dim(rasterimg)[2], byrow = TRUE)

# Save the compressed image to a file
writeJPEG(compressed_image, "compressed_image.jpg")


image(compressed_image)

#### FASTICA 


#####################  FAST ICA


# Load the necessary libraries
library(jpeg)
library(fastICA)

# Load the image
image <- readJPEG("img3.jpg")

# Reshape the image into a matrix of pixel values
image_matrix <- as.matrix(tiger)
dim(image_matrix)
# Perform ICA on the image matrix
image_components <- fastICA(rasterimg, n.comp = 10)
fastICA(mtcars, n.comp = 3)
# Select the number of components to keep
num_components <- 8

# Compress the image by keeping only the top `num_components` components
compressed_image_matrix <- image_components$A[, 1:num_components] %*% image_components$S[1:num_components, ]

# Reshape the compressed image matrix back into an image
compressed_image <- matrix(compressed_image_matrix, nrow = dim(rasterimg)[1], ncol = dim(rasterimg)[2], byrow = TRUE)

# Save the compressed image to a file
writeJPEG(compressed_image, "compressed_image.jpg")

fast_image <- function(factors){
  
  r_f <- fastICA(r, n.comp  = factors)
  g_f <- fastICA(g, n.comp  = factors)
  b_f<- fastICA(b,  n.comp  = factors)
  
  rgb_f <- list(r_f, g_f, b_f)
  #image_factors$scores[, 1:num_factors] %*% t(image_factors$loadings[, 1:num_factors])
  fac_image <- sapply(rgb_f, function(i){
    
    img <- i$A[, 1:factors] %*% t(i$S[1:factors,])
  }, simplify = "array")
  
  return(fac_image)
}

writeJPEG(fast_image(225), paste0("image",100,".jpg", collapse =""))


dim()


getwd()
























########## trial
dim(rasterimg)

image_components <- fastICA(rasterimg, n.comp = 20)
# Select the number of components to keep
num_components <- 15

# Compress the image by keeping only the top `num_components` components
compressed_image_matrix <- image_components$A[, 1:num_components] %*% image_components$S[1:num_components, ]

image(compressed_image_matrix)
# Reshape the compressed image matrix back into an image
compressed_image <- matrix(compressed_image_matrix, nrow = dim(rasterimg)[1], ncol = dim(rasterimg)[2], byrow = TRUE)

# Save the compressed image to a file
#writeJPEG(compressed_image, "compressed_image.jpg")
image(compressed_image)


dim(tiger)
img.flip<-flip(tiger, direction = "y")
rasterimg<-t(as.matrix(img.flip))
dim(rasterimg)


compressed.img <- tiger_r$x[,1:90] %*% t(tiger_r$rotation[,1:90])
image(compressed.img)
dim(compressed.img)


b_f<- fastICA(b,  n.comp  = 100)
df =b_f$A[, 1:80] %*% t(b_f$A[,1:80])
image(df)


fg= b_f$A[, 1:80] %*% t(b_f$A[,1:80])




dim(b_f$S)
dim(b_f$X)
dim(b_f$K)

dim(b_f$W)
dim(b_f$A)






















################################# factors analysis 





