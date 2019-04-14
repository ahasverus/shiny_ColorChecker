
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(raster)
library(imager)
library(sp)



shinyServer(function(input, output) {


  ### LOAD INPUT FILE

  myimage <- reactive({

    if (is.null(input$file) && input$action2 != 0){

      return(imager::load.image("butterfly_example.jpg"))
    }

    if (!is.null(input$file)){

      return(imager::load.image(input$file$datapath))
    }
  })


  ### GET CLICKED CORNER COORDINATES

  xyT <- reactiveValues(df = NULL)

  observeEvent(input$plot_click, {

    if (is.null(xyT$df) || nrow(xyT$df) < 4){

      xyT$df <- rbind(xyT$df, c(input$plot_click$x, input$plot_click$y))
      colnames(xyT$df) <- c("x", "y")
    }
  })


  ### DISPLAY CLICKED CORNER COORDINATES (IN TABLE)

  output$plot_clickedpoints <- renderTable({

    return(xyT$df)
  })


  ### PLOT POINTS AND POLYGONS ON INPUT PICTURE

  output$OriginalImage1 <- renderPlot({

    if (!is.null(input$file) || input$action2 != 0){

      par(xaxs = "i", yaxs = "i", mar = rep(1, 4), family = "serif")
      plot(myimage(), axes = FALSE, ann = FALSE, bty = "n")
      title(main = "ORIGINAL IMAGE", cex = 3)

      if (!is.null(xyT$df) && nrow(xyT$df) == 4){

        polygon(xyT$df[ , 1], xyT$df[ , 2], border = "green")

        points(mydataProc()[[1]], pch = 20, col = "green")
        points(mydataProc()[[2]], pch = 20, col = "red")
        points(mydataProc()[[3]], pch = 20, col = "red")

        points(mydataProc()[[4]], pch = 20, col = "green")
        points(mydataProc()[[5]], pch = 20, col = "red")
        points(mydataProc()[[6]], pch = 20, col = "red")

        points(mydataProc()[[7]], pch = 20, col = "green")
        points(mydataProc()[[8]], pch = 20, col = "red")
        points(mydataProc()[[9]], pch = 20, col = "red")

        points(mydataProc()[[10]], pch = 20, col = "green")
        points(mydataProc()[[11]], pch = 20, col = "red")
        points(mydataProc()[[12]], pch = 20, col = "red")

        text(mydataProc2()[[1]][ , 1:2], label = mydataProc2()[[1]][ , 3], col = "green")

        for(e in 1:nrow(mydataProc2()[[2]])) {

          polygon(
            x = c(
              mydataProc2()[[2]]$x1[e],
              mydataProc2()[[2]]$x2[e],
              mydataProc2()[[2]]$x4[e],
              mydataProc2()[[2]]$x3[e]
            ),
            y = c(
              mydataProc2()[[2]]$y1[e],
              mydataProc2()[[2]]$y2[e],
              mydataProc2()[[2]]$y4[e],
              mydataProc2()[[2]]$y3[e]
            ),
            border = "red"
          )
        }
      }

    } else {

      return()
    }
  })


  ### PLOT CALIBRATED IMAGE

  output$OriginalImage2 <- renderPlot({

    if (!is.null(mydataProc3())){

      par(xaxs = "i", yaxs = "i", mar = rep(1, 4))
      plot(mydataProc3(), axes = FALSE, ann = FALSE, bty = "n")
      title(main = "CALIBRATED IMAGE", cex = 3)

    } else {

      return()
    }
  })


  ### CALCULATE OUTER POINTS

  mydataProc <- reactive({

    patchSize <- input$decimal
    prop      <- (1 - patchSize)

    if (!is.null(input$file) || input$action2 != 0){

      if (!is.null(xyT$df) && nrow(xyT$df) == 4){

        xyDF <- as.data.frame(xyT$df)

        line1 <- xyDF[c(1, 2), ]
        line2 <- xyDF[c(2, 3), ]
        line3 <- xyDF[c(3, 4), ]
        line4 <- xyDF[c(4, 1), ]

        ###
        ###
        ###

        xdiff <- (line1$x[2] - line1$x[1]) / 6
        ydiff <- (line1$y[2] - line1$y[1]) / 6

        xmin  <- line1$x[1]
        ymin  <- line1$y[1]

        xySubA <- list(
          x = c(
            (xmin + xdiff * 0 + xmin + xdiff * 1) / 2,
            (xmin + xdiff * 1 + xmin + xdiff * 2) / 2,
            (xmin + xdiff * 2 + xmin + xdiff * 3) / 2,
            (xmin + xdiff * 3 + xmin + xdiff * 4) / 2,
            (xmin + xdiff * 4 + xmin + xdiff * 5) / 2,
            (xmin + xdiff * 5 + xmin + xdiff * 6) / 2
          ),
          y = c(
            (ymin + ydiff * 0 + ymin + ydiff * 1) / 2,
            (ymin + ydiff * 1 + ymin + ydiff * 2) / 2,
            (ymin + ydiff * 2 + ymin + ydiff * 3) / 2,
            (ymin + ydiff * 3 + ymin + ydiff * 4) / 2,
            (ymin + ydiff * 4 + ymin + ydiff * 5) / 2,
            (ymin + ydiff * 5 + ymin + ydiff * 6) / 2
          )
        )

        xySubBa <- list(
          x = c(
            (xmin + xdiff * 1 - (xdiff / 2) * prop),
            (xmin + xdiff * 2 - (xdiff / 2) * prop),
            (xmin + xdiff * 3 - (xdiff / 2) * prop),
            (xmin + xdiff * 4 - (xdiff / 2) * prop),
            (xmin + xdiff * 5 - (xdiff / 2) * prop),
            (xmin + xdiff * 6 - (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + ydiff * 1 - (ydiff / 2) * prop),
            (ymin + ydiff * 2 - (ydiff / 2) * prop),
            (ymin + ydiff * 3 - (ydiff / 2) * prop),
            (ymin + ydiff * 4 - (ydiff / 2) * prop),
            (ymin + ydiff * 5 - (ydiff / 2) * prop),
            (ymin + ydiff * 6 - (ydiff / 2) * prop)
          )
        )

        xySubBb <- list(
          x = c(
            (xmin + xdiff * 0 + (xdiff / 2) * prop),
            (xmin + xdiff * 1 + (xdiff / 2) * prop),
            (xmin + xdiff * 2 + (xdiff / 2) * prop),
            (xmin + xdiff * 3 + (xdiff / 2) * prop),
            (xmin + xdiff * 4 + (xdiff / 2) * prop),
            (xmin + xdiff * 5 + (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + ydiff * 0 + (ydiff / 2) * prop),
            (ymin + ydiff * 1 + (ydiff / 2) * prop),
            (ymin + ydiff * 2 + (ydiff / 2) * prop),
            (ymin + ydiff * 3 + (ydiff / 2) * prop),
            (ymin + ydiff * 4 + (ydiff / 2) * prop),
            (ymin + ydiff * 5 + (ydiff / 2) * prop)
          )
        )

        xySubDF_1A  <- as.data.frame(xySubA)
        xySubDF_1Ba <- as.data.frame(xySubBa)
        xySubDF_1Bb <- as.data.frame(xySubBb)

        ###
        ###
        ###

        xdiff <- (line3$x[2] - line3$x[1]) / 6
        ydiff <- (line3$y[2] - line3$y[1]) / 6

        xmin  <- line3$x[1]
        ymin  <- line3$y[1]

        xySubA <- list(
          x = c(
            (xmin + xdiff * 0 + xmin + xdiff * 1) / 2,
            (xmin + xdiff * 1 + xmin + xdiff * 2) / 2,
            (xmin + xdiff * 2 + xmin + xdiff * 3) / 2,
            (xmin + xdiff * 3 + xmin + xdiff * 4) / 2,
            (xmin + xdiff * 4 + xmin + xdiff * 5) / 2,
            (xmin + xdiff * 5 + xmin + xdiff * 6) / 2
          ),
          y = c(
            (ymin + ydiff * 0 + ymin + ydiff * 1) / 2,
            (ymin + ydiff * 1 + ymin + ydiff * 2) / 2,
            (ymin + ydiff * 2 + ymin + ydiff * 3) / 2,
            (ymin + ydiff * 3 + ymin + ydiff * 4) / 2,
            (ymin + ydiff * 4 + ymin + ydiff * 5) / 2,
            (ymin + ydiff * 5 + ymin + ydiff * 6) / 2
          )
        )

        xySubBa <- list(
          x = c(
            (xmin + xdiff * 1 - (xdiff / 2) * prop),
            (xmin + xdiff * 2 - (xdiff / 2) * prop),
            (xmin + xdiff * 3 - (xdiff / 2) * prop),
            (xmin + xdiff * 4 - (xdiff / 2) * prop),
            (xmin + xdiff * 5 - (xdiff / 2) * prop),
            (xmin + xdiff * 6 - (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + ydiff * 1 - (ydiff / 2) * prop),
            (ymin + ydiff * 2 - (ydiff / 2) * prop),
            (ymin + ydiff * 3 - (ydiff / 2) * prop),
            (ymin + ydiff * 4 - (ydiff / 2) * prop),
            (ymin + ydiff * 5 - (ydiff / 2) * prop),
            (ymin + ydiff * 6 - (ydiff / 2) * prop)
          )
        )

        xySubBb <- list(
          x = c(
            (xmin + xdiff * 0 + (xdiff / 2) * prop),
            (xmin + xdiff * 1 + (xdiff / 2) * prop),
            (xmin + xdiff * 2 + (xdiff / 2) * prop),
            (xmin + xdiff * 3 + (xdiff / 2) * prop),
            (xmin + xdiff * 4 + (xdiff / 2) * prop),
            (xmin + xdiff * 5 + (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + xdiff * 0 + (ydiff / 2) * prop),
            (ymin + ydiff * 1 + (ydiff / 2) * prop),
            (ymin + ydiff * 2 + (ydiff / 2) * prop),
            (ymin + ydiff * 3 + (ydiff / 2) * prop),
            (ymin + ydiff * 4 + (ydiff / 2) * prop),
            (ymin + ydiff * 5 + (ydiff / 2) * prop)
          )
        )

        xySubDF_3A  <- as.data.frame(xySubA)
        xySubDF_3Ba <- as.data.frame(xySubBa)
        xySubDF_3Bb <- as.data.frame(xySubBb)

        ###
        ###
        ###

        xdiff <- (line2$x[2] - line2$x[1]) / 4
        ydiff <- (line2$y[2] - line2$y[1]) / 4

        xmin  <- line2$x[1]
        ymin  <- line2$y[1]

        xySubA <- list(
          x = c(
            (xmin + xdiff * 0 + xmin + xdiff * 1) / 2,
            (xmin + xdiff * 1 + xmin + xdiff * 2) / 2,
            (xmin + xdiff * 2 + xmin + xdiff * 3) / 2,
            (xmin + xdiff * 3 + xmin + xdiff * 4) / 2
          ),
          y = c(
            (ymin + ydiff * 0 + ymin + ydiff * 1) / 2,
            (ymin + ydiff * 1 + ymin + ydiff * 2) / 2,
            (ymin + ydiff * 2 + ymin + ydiff * 3) / 2,
            (ymin + ydiff * 3 + ymin + ydiff * 4) / 2
          )
        )

        xySubBa <- list(
          x = c(
            (xmin + xdiff * 1 - (xdiff / 2) * prop),
            (xmin + xdiff * 2 - (xdiff / 2) * prop),
            (xmin + xdiff * 3 - (xdiff / 2) * prop),
            (xmin + xdiff * 4 - (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + ydiff * 1 - (ydiff / 2) * prop),
            (ymin + ydiff * 2 - (ydiff / 2) * prop),
            (ymin + ydiff * 3 - (ydiff / 2) * prop),
            (ymin + ydiff * 4 - (ydiff / 2) * prop)
          )
        )

        xySubBb <- list(
          x = c(
            (xmin + xdiff * 0 + (xdiff / 2) * prop),
            (xmin + xdiff * 1 + (xdiff / 2) * prop),
            (xmin + xdiff * 2 + (xdiff / 2) * prop),
            (xmin + xdiff * 3 + (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + ydiff * 0 + (ydiff / 2) * prop),
            (ymin + ydiff * 1 + (ydiff / 2) * prop),
            (ymin + ydiff * 2 + (ydiff / 2) * prop),
            (ymin + ydiff * 3 + (ydiff / 2) * prop)
          )
        )

        xySubDF_2A  <- as.data.frame(xySubA)
        xySubDF_2Ba <- as.data.frame(xySubBa)
        xySubDF_2Bb <- as.data.frame(xySubBb)

        ###
        ###
        ###

        xdiff <- (line4$x[2] - line4$x[1]) / 4
        ydiff <- (line4$y[2] - line4$y[1]) / 4

        xmin  <- line4$x[1]
        ymin  <- line4$y[1]

        xySubA <- list(
          x = c(
            (xmin + xdiff * 0 + xmin + xdiff * 1) / 2,
            (xmin + xdiff * 1 + xmin + xdiff * 2) / 2,
            (xmin + xdiff * 2 + xmin + xdiff * 3) / 2,
            (xmin + xdiff * 3 + xmin + xdiff * 4) / 2
          ),
          y = c(
            (ymin + ydiff * 0 + ymin + ydiff * 1) / 2,
            (ymin + ydiff * 1 + ymin + ydiff * 2) / 2,
            (ymin + ydiff * 2 + ymin + ydiff * 3) / 2,
            (ymin + ydiff * 3 + ymin + ydiff * 4) / 2
          )
        )

        xySubBa <- list(
          x = c(
            (xmin + xdiff * 1 - (xdiff / 2) * prop),
            (xmin + xdiff * 2 - (xdiff / 2) * prop),
            (xmin + xdiff * 3 - (xdiff / 2) * prop),
            (xmin + xdiff * 4 - (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + ydiff * 1 - (ydiff / 2) * prop),
            (ymin + ydiff * 2 - (ydiff / 2) * prop),
            (ymin + ydiff * 3 - (ydiff / 2) * prop),
            (ymin + ydiff * 4 - (ydiff / 2) * prop)
          )
        )

        xySubBb <- list(
          x = c(
            (xmin + xdiff * 0 + (xdiff / 2) * prop),
            (xmin + xdiff * 1 + (xdiff / 2) * prop),
            (xmin + xdiff * 2 + (xdiff / 2) * prop),
            (xmin + xdiff * 3 + (xdiff / 2) * prop)
          ),
          y = c(
            (ymin + ydiff * 0 + (ydiff / 2) * prop),
            (ymin + ydiff * 1 + (ydiff / 2) * prop),
            (ymin + ydiff * 2 + (ydiff / 2) * prop),
            (ymin + ydiff * 3 + (ydiff / 2) * prop)
          )
        )

        xySubDF_4A  <- as.data.frame(xySubA)
        xySubDF_4Ba <- as.data.frame(xySubBa)
        xySubDF_4Bb <- as.data.frame(xySubBb)

        return(
          list(
            xySubDF_1A, xySubDF_1Ba, xySubDF_1Bb,
            xySubDF_2A, xySubDF_2Ba, xySubDF_2Bb,
            xySubDF_3A, xySubDF_3Ba, xySubDF_3Bb,
            xySubDF_4A, xySubDF_4Ba, xySubDF_4Bb
          )
        )
      }
    }
  })


  ### CALCULATE LABEL POSITIONS

  mydataProc2 <- reactive({

    patchSize <- input$decimal
    prop      <- (1 - patchSize)

    xySubDF_1A  <- mydataProc()[[1]]
    xySubDF_1Ba <- mydataProc()[[2]]
    xySubDF_1Bb <- mydataProc()[[3]]
    xySubDF_2A  <- mydataProc()[[4]]
    xySubDF_2Ba <- mydataProc()[[5]]
    xySubDF_2Bb <- mydataProc()[[6]]
    xySubDF_3A  <- mydataProc()[[7]]
    xySubDF_3Ba <- mydataProc()[[8]]
    xySubDF_3Bb <- mydataProc()[[9]]
    xySubDF_4A  <- mydataProc()[[10]]
    xySubDF_4Ba <- mydataProc()[[11]]
    xySubDF_4Bb <- mydataProc()[[12]]

    if (!is.null(input$file) || input$action2 != 0){

      if (!is.null(xyT$df) && nrow(xyT$df) == 4){

        labels <- list(
          c( 1,  7, 13, 19),
          c( 2,  8, 14, 20),
          c( 3,  9, 15, 21),
          c( 4, 10, 16, 22),
          c( 5, 11, 17, 23),
          c( 6, 12, 18, 24)
        )

        xyTot <- xyMid <- NULL

        for(e in 1:nrow(xySubDF_1A)){

          xyLine1 <- xySubDF_1A[e, ]
          xyLine2 <- xySubDF_3A[6:1, ][e, ]

          xdiff   <- (xyLine2$x - xyLine1$x) / 4
          ydiff   <- (xyLine2$y - xyLine1$y) / 4

          xmin    <- xyLine1$x
          ymin    <- xyLine1$y

          xySub <- list(
            x = c(
              (xmin + xdiff * 0 + xmin + xdiff * 1) / 2,
              (xmin + xdiff * 1 + xmin + xdiff * 2) / 2,
              (xmin + xdiff * 2 + xmin + xdiff * 3) / 2,
              (xmin + xdiff * 3 + xmin + xdiff * 4) / 2
            ),
            y = c(
              (ymin + ydiff * 0 + ymin + ydiff * 1) / 2,
              (ymin + ydiff * 1 + ymin + ydiff * 2) / 2,
              (ymin + ydiff * 2 + ymin + ydiff * 3) / 2,
              (ymin + ydiff * 3 + ymin + ydiff * 4) / 2
            )
          )

          xySubDF <- as.data.frame(xySub)

          xySubDFLabel <- cbind(xySubDF, label = labels[[e]])

          xyMid <- rbind(xyMid, xySubDFLabel)

          xyLine1a <- xySubDF_1Ba[e, ]
          xyLine1b <- xySubDF_1Bb[e, ]

          xyLine3a <- xySubDF_3Bb[6:1, ][e, ]
          xyLine3b <- xySubDF_3Ba[6:1, ][e, ]

          xdiffa <- (xyLine3a$x - xyLine1a$x) / 4
          ydiffa <- (xyLine3a$y - xyLine1a$y) / 4

          xdiffb <- (xyLine3b$x - xyLine1b$x) / 4
          ydiffb <- (xyLine3b$y - xyLine1b$y) / 4

          xmina <- xyLine1a$x
          ymina <- xyLine1a$y

          xminb <- xyLine1b$x
          yminb <- xyLine1b$y


          xySubAa <- list(
            x = c(
              (xmina + xdiffa * 1 - (xdiffa / 2) * prop),
              (xmina + xdiffa * 2 - (xdiffa / 2) * prop),
              (xmina + xdiffa * 3 - (xdiffa / 2) * prop),
              (xmina + xdiffa * 4 - (xdiffa / 2) * prop)
            ),
            y = c(
              (ymina + ydiffa * 1 - (ydiffa / 2) * prop),
              (ymina + ydiffa * 2 - (ydiffa / 2) * prop),
              (ymina + ydiffa * 3 - (ydiffa / 2) * prop),
              (ymina + ydiffa * 4 - (ydiffa / 2) * prop)
            )
          )

          xySubAb <- list(
            x = c(
              (xmina + xdiffa * 0 + (xdiffa / 2) * prop),
              (xmina + xdiffa * 1 + (xdiffa / 2) * prop),
              (xmina + xdiffa * 2 + (xdiffa / 2) * prop),
              (xmina + xdiffa * 3 + (xdiffa / 2) * prop)
            ),
            y = c(
              (ymina + ydiffa * 0 + (ydiffa / 2) * prop),
              (ymina + ydiffa * 1 + (ydiffa / 2) * prop),
              (ymina + ydiffa * 2 + (ydiffa / 2) * prop),
              (ymina + ydiffa * 3 + (ydiffa / 2) * prop)
            )
          )

          xySubBa <- list(
            x = c(
              (xminb + xdiffb * 1 - (xdiffb / 2) * prop),
              (xminb + xdiffb * 2 - (xdiffb / 2) * prop),
              (xminb + xdiffb * 3 - (xdiffb / 2) * prop),
              (xminb + xdiffb * 4 - (xdiffb / 2) * prop)
            ),
            y = c(
              (yminb + ydiffb * 1 - (ydiffb / 2) * prop),
              (yminb + ydiffb * 2 - (ydiffb / 2) * prop),
              (yminb + ydiffb * 3 - (ydiffb / 2) * prop),
              (yminb + ydiffb * 4 - (ydiffb / 2) * prop)
            )
          )

          xySubBb <- list(
            x = c(
              (xminb + xdiffb * 0 + (xdiffb / 2) * prop),
              (xminb + xdiffb * 1 + (xdiffb / 2) * prop),
              (xminb + xdiffb * 2 + (xdiffb / 2) * prop),
              (xminb + xdiffb * 3 + (xdiffb / 2) * prop)
            ),
            y = c(
              (yminb + ydiffb * 0 + (ydiffb / 2) * prop),
              (yminb + ydiffb * 1 + (ydiffb / 2) * prop),
              (yminb + ydiffb * 2 + (ydiffb / 2) * prop),
              (yminb + ydiffb * 3 + (ydiffb / 2) * prop)
            )
          )

          xySubAaDF <- as.data.frame(xySubAa)
          xySubAbDF <- as.data.frame(xySubAb)
          xySubBaDF <- as.data.frame(xySubBa)
          xySubBbDF <- as.data.frame(xySubBb)

          xySubRow <- cbind(xySubAaDF, xySubAbDF, xySubBaDF, xySubBbDF, label = labels[[e]])
          colnames(xySubRow) <- c("x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4", "label")

          xyTot <- rbind(xyTot, xySubRow)

        }

        return(
          list(xyMid, xyTot)
        )
      }
    }
  })


  ### CALCULATE RGB VALUES AND MODEL

  mydataProc3 <- reactive({

    if ((!is.null(input$file) || input$action2 != 0) && input$action1 != 0){

      if(!is.null(xyT$df) && nrow(xyT$df) == 4){

        withProgress(message = "Calibrating image...................", value = 0, {

          im    <- myimage()
          xyTot <- mydataProc2()[[2]]

          mR <- raster::as.matrix(R(im)) * 255
          mG <- raster::as.matrix(G(im)) * 255
          mB <- raster::as.matrix(B(im)) * 255

          rR <- raster::raster(mR)
          rG <- raster::raster(mG)
          rB <- raster::raster(mB)

          extent(rR) <- c(0, dim(im)[2], 0, dim(im)[1])
          extent(rG) <- c(0, dim(im)[2], 0, dim(im)[1])
          extent(rB) <- c(0, dim(im)[2], 0, dim(im)[1])

          rR <- flip(t(rR), "y")
          rG <- flip(t(rG), "y")
          rB <- flip(t(rB), "y")

          xyTot$imR <- NA
          xyTot$imG <- NA
          xyTot$imB <- NA

          xyTot <- xyTot[order(xyTot$label), ]

          n <- 27
          i <- 0

          cat("\n###\n")

          for (e in 1:nrow(xyTot)){

            cat(paste("\r### 1. Calculating observed RGB values for patch", e))

            polygon(
              x = c(xyTot$x1[e], xyTot$x2[e], xyTot$x4[e], xyTot$x3[e]),
              y = c(xyTot$y1[e], xyTot$y2[e], xyTot$y4[e], xyTot$y3[e]),
              border = "red"
            )

            outline <- rbind(
              c(xyTot$x1[e], xyTot$y1[e]),
              c(xyTot$x2[e], xyTot$y2[e]),
              c(xyTot$x4[e], xyTot$y4[e]),
              c(xyTot$x3[e], xyTot$y3[e])
            )

            poly      <- sp::Polygons(list(sp::Polygon(outline)), paste("r"))
            polyList  <- c(poly)
            polyNames <- c(paste("r"))
            sr        <- sp::SpatialPolygons(polyList)
            srdf      <- sp::SpatialPolygonsDataFrame(sr, data.frame(1:length(polyNames), row.names = polyNames))

            extrR <- raster::extract(rR, srdf)
            extrG <- raster::extract(rG, srdf)
            extrB <- raster::extract(rB, srdf)

            xyTot$imR[e] <- mean(extrR[[1]])
            xyTot$imG[e] <- mean(extrG[[1]])
            xyTot$imB[e] <- mean(extrB[[1]])

            i <- i + 1
            incProgress(1/n, detail = paste("Calculating observed RGB values for patch", i))
          }

          cat("\n")

          # Colorimetric values for ColorCheker targets
          # l1  <- c( 1, 115,  82,  68)
          # l2  <- c( 2, 194, 150, 130)
          # l3  <- c( 3,  98, 122, 157)
          # l4  <- c( 4,  87, 108,  67)
          # l5  <- c( 5, 133, 128, 177)
          # l6  <- c( 6, 103, 189, 170)
          # l7  <- c( 7, 214, 126,  44)
          # l8  <- c( 8,  80,  91, 166)
          # l9  <- c( 9, 193,  90,  99)
          # l10 <- c(10,  94,  60, 108)
          # l11 <- c(11, 157, 188,  64)
          # l12 <- c(12, 224, 163,  46)
          # l13 <- c(13,  56,  61, 150)
          # l14 <- c(14,  70, 148,  73)
          # l15 <- c(15, 175,  54,  60)
          # l16 <- c(16, 231, 199,  31)
          # l17 <- c(17, 187,  86, 149)
          # l18 <- c(18,   8, 133, 161)
          # l19 <- c(19, 243, 243, 242)
          # l20 <- c(20, 200, 200, 200)
          # l21 <- c(21, 160, 160, 160)
          # l22 <- c(22, 122, 122, 121)
          # l23 <- c(23,  85,  85,  85)
          # l24 <- c(24,  52,  52,  52)

          l1  <- c( 1, 115,  81,  67)
          l2  <- c( 2, 198, 148, 130)
          l3  <- c( 3,  89, 123, 157)
          l4  <- c( 4,  90, 109,  65)
          l5  <- c( 5, 128, 128, 177)
          l6  <- c( 6,  92, 190, 174)
          l7  <- c( 7, 221, 125,  48)
          l8  <- c( 8,  65,  92, 170)
          l9  <- c( 9, 195,  82,  97)
          l10 <- c(10,  92,  58, 106)
          l11 <- c(11, 158, 190,  65)
          l12 <- c(12, 228, 162,  39)
          l13 <- c(13,  34,  65, 147)
          l14 <- c(14,  67, 150,  73)
          l15 <- c(15, 178,  49,  58)
          l16 <- c(16, 237, 199,  22)
          l17 <- c(17, 191,  85, 151)
          l18 <- c(18,   4, 136, 169)
          l19 <- c(19, 244, 246, 244)
          l20 <- c(20, 198, 202, 202)
          l21 <- c(21, 159, 163, 163)
          l22 <- c(22, 120, 121, 123)
          l23 <- c(23,  81,  85,  86)
          l24 <- c(24,  48,  49,  51)

          ColorCheckerRGB <- as.data.frame(
            rbind(
              l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12,
              l13, l14, l15, l16, l17, l18, l19, l20, l21, l22, l23, l24
            )
          )
          colnames(ColorCheckerRGB) <- c("label", "sR", "sG", "sB")

          dat <- merge(xyTot, ColorCheckerRGB, by = "label")

          cat("### 2. Adjusting quadratic regressions...\n")

          sR  <- dat$sR
          sG  <- dat$sG
          sB  <- dat$sB

          imR <- dat$imR
          imG <- dat$imG
          imB <- dat$imB

          modelR <- lm(sR ~ imR + imG + imB + I(imR^2) + I(imG^2) + I(imB^2))

          i <- i + 1
          incProgress(1/n, detail = paste("Adjusting quadratic regression (Red)..."))

          modelG <- lm(sG ~ imR + imG + imB + I(imR^2) + I(imG^2) + I(imB^2))

          i <- i + 1
          incProgress(1/n, detail = paste("Adjusting quadratic regression (Blue)..."))

          modelB <- lm(sB ~ imR + imG + imB + I(imR^2) + I(imG^2) + I(imB^2))

          i <- i + 1
          incProgress(1/n, detail = paste("Adjusting quadratic regression (Green)..."))

          dfIm <- data.frame(
            imR = matrix(mR, ncol = 1),
            imG = matrix(mG, ncol = 1),
            imB = matrix(mB, ncol = 1)
          )

          cat("### 3. Calibrating original image colors...\n")

          i <- i + 1
          incProgress(1/n, detail = paste("Calibrating original image colors..."))

          prR <- predict(modelR, dfIm)
          prG <- predict(modelG, dfIm)
          prB <- predict(modelB, dfIm)

          dfCal <- as.data.frame(cbind(prR, prG, prB))

          cat("### 4. Rebuilding image...\n")

          i <- i + 1
          incProgress(1/n, detail = paste("Rebuilding image..."))

          R <- matrix(dfCal$prR, nrow = dim(im)[1])
          G <- matrix(dfCal$prG, nrow = dim(im)[1])
          B <- matrix(dfCal$prB, nrow = dim(im)[1])

          imCal <- array(dim = dim(im))
          imCal[ , , , 1] <- R
          imCal[ , , , 2] <- G
          imCal[ , , , 3] <- B

          imCal <- imager::as.cimg(imCal)

          cat("### Done!\n")
          cat("###\n")
        })

        return(imCal)
      }
    }
  })


  ###

  output$fileUploaded <- reactive({

    return(!is.null(input$file))
  })


  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)


  output$exampleloaded <- reactive({

    return(input$action2 != 0)
  })


  outputOptions(output, "exampleloaded", suspendWhenHidden = FALSE)


  output$coordinates_clicked <- reactive({

    return(!is.null(xyT$df))
  })


  outputOptions(output, "coordinates_clicked", suspendWhenHidden = FALSE)


  output$calibrated <- reactive({

    return(input$action1 != 0)
  })


  outputOptions(output, "calibrated", suspendWhenHidden = FALSE)


  ###

  fName <- renderText({

    return(
      gsub("\\.jpg|\\.jpeg","", input$file$name)
    )
  })

  output$downloadData <- downloadHandler(

    filename = function() {
      paste0(fName(), "_calibrated.jpg")
    },

    content = function(file) {
      save.image(mydataProc3(), file, quality = 1)
    }
  )
})


observe({
  reactive({input$action1 <- 0})
})
