library(imager)
library(sp)
library(raster)

img_path <- "~/OneDrive/OneDrive - Fondation Biodiversité/MySpace/shiny_ColorChecker/data/"

fls <- list.files(paste0(img_path, "test2"), pattern = "window\\.JPG$", full.names = TRUE)

z <- 4

img <- load.image(fls[z])

ColorCheckerRGB <- read.csv2(paste0(img_path, "test2/spectro_mires.csv"), stringsAsFactors = FALSE)
ids <- c(1, 7, 13, 19, 2, 8, 14, 20, 3, 9, 15, 21, 4, 10, 16, 22, 5, 11, 17, 23, 6, 12, 18, 23)
ColorCheckerRGB[ , "id"] <- ids
ColorCheckerRGB <- ColorCheckerRGB[order(ColorCheckerRGB[ , "id"]), c("id", "R", "G", "B")]
colnames(ColorCheckerRGB) <- c("id", "trueR", "trueG", "trueB")

for (i in 2:4) {
  ColorCheckerRGB[ , i] <- as.numeric(as.character(ColorCheckerRGB[ , i])) * 255
}

par(mfrow = c(1, 1))
par(xaxs = "i", yaxs = "i", mar = rep(1, 4), family = "serif", bg = "black")
plot(img, axes = FALSE, ann = FALSE, bty = "n")

fac <- 0.1

xy <- locator(4)
xyDF <- data.frame(x = xy$x, y = xy$y)

start <- Sys.time()

xleft   <- min(xyDF[ , "x"])
xright  <- max(xyDF[ , "x"])
ybottom <- max(xyDF[ , "y"])
ytop    <- min(xyDF[ , "y"])

nCols <- 4
cols  <- numeric(nCols)
for (i in 1:nCols) {
  cols[i] <- xleft + (i * (xright - xleft) / nCols) - ((xright - xleft) / nCols) / 2
}

nRows <- 6
rows  <- numeric(nRows)
for (i in 1:nRows) {
  rows[i] <- ybottom + (i * (ytop - ybottom) / nRows) - ((ytop - ybottom) / nRows) / 2
}

patterns <- expand.grid(y = rows, x = cols)[ , 2:1]

rect(xleft, ybottom, xright, ytop, border = "green")
text(patterns[ , 1:2], as.character(1:24), cex = .5)

patterns <- data.frame(
  id   = 1:nrow(patterns),
  xmin = patterns[ , "x"] - (((xright - xleft) / nCols) / 2) * fac,
  xmax = patterns[ , "x"] + (((xright - xleft) / nCols) / 2) * fac,
  ymin = patterns[ , "y"] - (((ytop - ybottom) / nRows) / 2) * fac,
  ymax = patterns[ , "y"] + (((ytop - ybottom) / nRows) / 2) * fac,
  xctr = patterns[ , "x"],
  yctr = patterns[ , "y"]
)

for (i in 1:nrow(patterns)){
  rect(patterns[i, "xmin"],patterns[i, "ymin"], patterns[i, "xmax"], patterns[i, "ymax"], border = "red")
}


matrixR <- as.matrix(R(img)) * 255
matrixG <- as.matrix(G(img)) * 255
matrixB <- as.matrix(B(img)) * 255


rasRGB <- raster::stack(
  raster(matrixR),
  raster(matrixG),
  raster(matrixB)
)

extent(rasRGB) <- c(0, dim(img)[2], 0, dim(img)[1])

rasRGB         <- raster::flip(t(rasRGB), direction = "y")
names(rasRGB)  <- c("R", "G", "B")

patCoords <- SpatialPolygons(
  apply(patterns, 1, function(x){
    Polygons(
      list(
        Polygon(
          coords = data.frame(
            x = c(x[2], x[2], x[3], x[3], x[2]),
            y = c(x[4], x[5], x[5], x[4], x[4])
          ),
          hole = FALSE
        )
      ),
      ID = as.character(x[1])
    )
  })
)

patColors <- data.frame(
  id = patterns[ , "id"],
  raster::extract(rasRGB, patCoords, fun = mean)
)



# l1  <- c( 1, 115,  81,  69)
# l2  <- c( 2, 198, 148, 130)
# l3  <- c( 3,  91, 122, 158)
# l4  <- c( 4,  90, 108,  64)
# l5  <- c( 5, 127, 129, 177)
# l6  <- c( 6,  91, 191, 175)
# l7  <- c( 7, 222, 125,  50)
# l8  <- c( 8,  67,  92, 169)
# l9  <- c( 9, 197,  81,  97)
# l10 <- c(10,  92,  60, 107)
# l11 <- c(11, 157, 191,  64)
# l12 <- c(12, 228, 162,  39)
# l13 <- c(13,  35,  64, 148)
# l14 <- c(14,  65, 150,  73)
# l15 <- c(15, 176,  51,  58)
# l16 <- c(16, 236, 200,  21)
# l17 <- c(17, 191,  86, 153)
# l18 <- c(18,   0, 137, 171)
# l19 <- c(19, 244, 246, 244)
# l20 <- c(20, 198, 203, 203)
# l21 <- c(21, 158, 163, 163)
# l22 <- c(22, 120, 121, 123)
# l23 <- c(23,  79,  85,  86)
# l24 <- c(24,  48,  50,  52)


# ColorCheckerRGB <- as.data.frame(
#   rbind(
#     l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12,
#     l13, l14, l15, l16, l17, l18, l19, l20, l21, l22, l23, l24
#   )
# )
# colnames(ColorCheckerRGB) <- c("id", "trueR", "trueG", "trueB")

dat <- merge(patColors, ColorCheckerRGB, by = "id")

for (polynom in 1:2) {

  if (polynom == 1) {
    modelR <- lm(trueR ~ R + G + B, data = dat)
    modelG <- lm(trueG ~ R + G + B, data = dat)
    modelB <- lm(trueB ~ R + G + B, data = dat)
    # modelR <- lm(trueR ~ R, data = dat)
    # modelG <- lm(trueG ~ G, data = dat)
    # modelB <- lm(trueB ~ B, data = dat)
  }

  if (polynom == 2) {
    modelR <- lm(trueR ~ R + G + B + I(R^2) + I(G^2) + I(B^2), data = dat)
    modelG <- lm(trueG ~ R + G + B + I(R^2) + I(G^2) + I(B^2), data = dat)
    modelB <- lm(trueB ~ R + G + B + I(R^2) + I(G^2) + I(B^2), data = dat)
    # modelR <- lm(trueR ~ R + I(R^2), data = dat)
    # modelG <- lm(trueG ~ G + I(G^2), data = dat)
    # modelB <- lm(trueB ~ B + I(B^2), data = dat)
  }

  if (polynom == 3) {
    modelR <- lm(trueR ~ R + G + B + I(R^2) + I(G^2) + I(B^2) + I(R^3) + I(G^3) + I(B^3), data = dat)
    modelG <- lm(trueG ~ R + G + B + I(R^2) + I(G^2) + I(B^2) + I(R^3) + I(G^3) + I(B^3), data = dat)
    modelB <- lm(trueB ~ R + G + B + I(R^2) + I(G^2) + I(B^2) + I(R^3) + I(G^3) + I(B^3), data = dat)
  }

  if (polynom == 0) {
    modelR <- lm(trueR ~ I(R^2) + I(G^2) + I(B^2), data = dat)
    modelG <- lm(trueG ~ I(R^2) + I(G^2) + I(B^2), data = dat)
    modelB <- lm(trueB ~ I(R^2) + I(G^2) + I(B^2), data = dat)
  }


  ypred <- data.frame(
    R = matrix(matrixR, ncol = 1),
    G = matrix(matrixG, ncol = 1),
    B = matrix(matrixB, ncol = 1)
  )

  predR <- predict(modelR, ypred)
  predG <- predict(modelG, ypred)
  predB <- predict(modelB, ypred)

  imCal <- array(dim = dim(img))
  imCal[ , , , 1] <- matrix(predR, nrow = dim(img)[1])
  imCal[ , , , 2] <- matrix(predG, nrow = dim(img)[1])
  imCal[ , , , 3] <- matrix(predB, nrow = dim(img)[1])

  imCal <- imager::as.cimg(imCal)

  # plot(imCal, axes = FALSE, ann = FALSE, bty = "n")


  print(Sys.time() - start)

  save.image(imCal, file = paste0(gsub("\\.JPG|\\.jpg", "", fls[z]), "_calibrated_polynom-", polynom, ".jpg"), quality = 0.7)
}




# text(patterns[ , 6:7], as.character(1:24), cex = .5)


#
# plot(c(0, 255), c(0, 255), type = "n", xlab = "True colors", ylab = "Calibrated colors")
# for (i in 1:24){
#   points(dat[i, "trueG"], modelG$fit[i], pch = 15, cex = 2, col = rgb(dat[i, "trueR"], dat[i, "trueG"], dat[i, "trueB"], maxColorValue = 255))
# }
# lines(c(0, 255), c(0, 255))
# img <- load.image(paste0(img_path, "cora_ori.jpg"))

# plot(img, axes = FALSE, ann = FALSE, bty = "n")

# i <- 1
# plot(1, 1, xlim = c(0, 3), pch = 15, cex = 10, col = rgb(dat[i, 2], dat[i, 3], dat[i, 4], maxColorValue = 255))
# points(2, 1, pch = 15, cex = 10, col = rgb(dat[i, 5], dat[i, 6], dat[i, 7], maxColorValue = 255))
