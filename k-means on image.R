library(ggplot2)
library(operators)
library(jpeg)
library(grid)
require(gridExtra)

setwd("C:/Users/gwist/Desktop/3rdYear/Math336/Math336Project")

# image.path is where you set the image name so that the algorithm
# knows which image in the working directory to work on.

# Img is where we read the .jpg/.jpeg file, turning it into an array that
# we can work with.

image.path<-paste(getwd(),"/vangogh.jpeg",sep="")
img <- readJPEG(image.path)

#centroids3d is an empty data frame fed into the algorithm

centroids3d <- data.frame(
  x = c(),
  y = c(),
  z = c()
)

kmc3d <- function(img, k, cent) {

  # If statement so that we don't reassign the img variable during recursion.
  # Within the if statement, we seperate our RGB values and pixel coordinates.
  if (length(colnames(img)) != 9) {
    imgDm <- dim(img)
    img <- data.frame(
      x = rep(1:imgDm[2], each = imgDm[1]),
      y = rep(imgDm[1]:1, imgDm[2]),
      R = as.vector(img[,,1]),
      G = as.vector(img[,,2]),
      B = as.vector(img[,,3])
    )
  }

  colnames = colnames(img)

  # Another if statement to avoid reassignment.
  # Within the if statement we create some dummy columns in the img data frame
  # for the algorithm to work with in the future.
  if (colnames[length(colnames)] != "Bn") {
    img[, "Cluster"] <- 0
    img[, "Rn"] <- 0
    img[, "Gn"] <- 0
    img[, "Bn"] <- 0
  }

  # if the centroid input is an empty data frame, do this:
  # this assigns random centroids.
  if (length(cent) == 0) {
    for (i in 1:k) {
      newcent = data.frame(x = runif(1, 0, 1), y = runif(1, 0, 1), z = runif(1, 0, 1))
      cent <- rbind(cent, newcent)
    }
  }

  # This count is used throughout the algorithm to keep track of pixels that
  # have not changed cluster between iterations.
  sameclustcount <<- 0
  countedi <- c()

  # The following for loop goes through each pixel and finds the distance
  # between its RGB value and the centroids.
  # It then finds the shortest one and then assigns it to that centroid's
  # cluster.
  # If the assigned cluster is equal to the cluster it was already in, then
  # sameclustcount increases by one, and the respective pixel is added to the
  # countedi vector.
  # If our sameclustcount equals the amount of pixels, then the for loop breaks.
  for (i in 1:length(rownames(img))) {
    dist <- c()

    for (j in 1:length(rownames(cent))) {
      dist <- append(dist, as.numeric(dist(rbind(c(img[i,'R'],img[i,'G'],img[i,'B']), c(cent[j,1],cent[j,2],cent[j,3])))))
    }

    minind = match(min(dist), dist)

    if (img[i,"Cluster"] == minind & i %!in% countedi) {
      append(countedi, i)
      sameclustcount <<- sameclustcount + 1
    }

    img[i,"Cluster"] <- minind

    img[i,c("Rn", "Gn", "Bn")] <- cent[img[i,"Cluster"],]

    if (sameclustcount == length(rownames(img))) {
      break
    }
  }

  # factorising the Cluster column in th img data frame.
  img$Cluster <- as.factor(img$Cluster)

  # new blank centroid data frame for the following iteration.
  cent2 <- data.frame(
    x = c(),
    y = c(),
    z = c()
  )

  # The following for loop goes through each cluster and finds the mean of
  # its members, and then adds this value to the new cent2 data frame.
  for (i in 1:k) {
    clusti <- img$Cluster == i
    clustidf <- img[clusti,]
    newcent2 = data.frame(x = mean(clustidf[,'R']), y = mean(clustidf[,'G']), z = mean(clustidf[,'B']))
    cent2 <- rbind(cent2, newcent2)
  }

  # For debugging purposes. Also helps keep track of the algorithm.
  print(cent2)

  # The following if statement occurs at the end of the algorithm when our
  # sameclustcount equals the amount of pixels.
  # Within the if statement we generate an image that is as a comparison
  # between the original and the algorithm's output.
  if (sameclustcount == length(rownames(img))) {
    print("Clustering complete!")
    print("Creating image, please wait...")

    plot1<-ggplot(data = img, aes(x = x, y = y)) + geom_point(colour = rgb(img[c("R", "G", "B")])) + labs(title = "Original Image")
    plot2<-ggplot(data = img, aes(x = x, y = y)) + geom_point(colour = rgb(img[c("Rn", "Gn", "Bn")])) + labs(title = paste("k-Means Clustering of", k, "Colours"))
    print("Done!")
    sameclustcount <<- 0
    return(grid.arrange(plot1, plot2, ncol=2))
  }

  # This else statement begins the next iteration, along with an update
  # on progress.
  else {
    print(paste(format(round((sameclustcount/length(rownames(img)))*100, 2), nsmall = 2), "percent complete..."))
    kmc3d(img, k, cent2)
  }
}

kmc3d(img, 3, centroids3d)
