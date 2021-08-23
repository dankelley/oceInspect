# vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4

# FIXME:
# 1. do I need lastPoint?
# 2. why do I need to reclick when removing points?
# 3. add T(z)
# 4. add S(z)
# [ ] buffer length box is wrong

library(argoFloats)

#' @importFrom graphics grid lines mtext par points
#' @importFrom grDevices gray
#' @importFrom oce as.ctd read.oce swZ
#' @importFrom stats predict smooth.spline
#' @importFrom utils head read.csv write.csv
#' @import shiny

appName <- "oceInspect"
appVersion <- "0.2"
plotHeight <- 500
pointsize <- 12
col <- 1
cex <- 1                               # FIXME: let user select via a UI tool
pch <- 20                              # FIXME: let user select via a UI tool
mgp <- c(2, 0.7, 0)
keyPressHelp <- "Keystroke commands
<ul>
<li> 0: save mouse location as category '0' (and similar for 1 through 9)</li>
<li> -: remove existing point near mouse location</li>
<li> u: undo last save</li>
<li> s: show saved data</li>
<li> w: write saved data to a csv file</li>
<li> ?: display this window</li>
</ul>"

maybeNull <- function(x, default)
    if (is.null(x)) default else x

pluralize <- function(n, name)
    if (n == 1L) paste0(n, " ", name) else paste0(n, " ", name, "s")

cacheEnv <- new.env(parent=emptyenv())

#. overallHelp <- "FIXME: write something useful here.  The best plan is to wait until the UI is stable."

ui <- shiny::fluidPage(
    shiny::headerPanel(title="", windowTitle="oceInspect"),
    shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="text-indent:1em ; background:#e6f3ff ; .btn.disabled { background-color: red; }",
    shiny::fluidRow(
        shiny::column(4,
            shiny::uiOutput(outputId="nameUI")),
        shiny::column(4,
            shiny::selectInput("plotChoice", "View",
                choices=c("T-S"="TS",
                    "\u03C3-\u03C0"="densitySpice",
                    "\u03C3 Profile"="densityProfile",
                    "S Profile"="SProfile",
                    "T Profile"="TProfile"),
                selected="Temperature-Salinity")),
        shiny::column(3,
            shiny::selectInput("plotType", "Type",
                choices=c("Point"="p", "Line"="l", "Both"="o"),
                selected="o"))),
    shiny::fluidRow(
        shiny::column(2,
            shiny::selectInput("data", "Data",
                choices=c("Raw"="raw", "Spline"="spline"),
                selected="Raw")),
        shiny::conditionalPanel("input.data == 'spline'",
            shiny::column(2,
                shiny::numericInput("Ndf", "N/df",
                    5, 1, 10, step=1))),
        shiny::column(1,
            shiny::checkboxInput("showSaved", "Hilite", TRUE)),
        shiny::column(1,
            shiny::checkboxInput("debug", "Debug", FALSE))
        ),
    shiny::fluidRow(
        shiny::column(7,
            shiny::uiOutput(outputId="UIinfo1")),
        shiny::column(3,
            shiny::uiOutput(outputId="UIinfo2")),
        shiny::column(2,
            shiny::actionButton("quit", "Quit"))
        ),
    shiny::fluidRow(
        shiny::plotOutput("plot",
            hover=shiny::hoverOpts("hover", delay=100),
            brush=shiny::brushOpts("brush", delay=2000, resetOnNew=TRUE)))
    )

server <- function(input, output, session)
{
    debug <- FALSE
    msg <- function(..., eol="\n")
    {
        if (!is.null(debug) && debug)
            cat(file=stderr(), ..., eol)
    }
    lastPoint <- list(view=NULL, x=NULL, y=NULL, i=NULL)
    buffer <- list(key=NULL, filename=NULL, view=NULL, x=NULL, y=NULL, i=NULL)
    state <- shiny::reactiveValues(
        i=0,
        bufferLength=0L,               # making this reactive means info2 updates when points are saved
        showSaved=FALSE)               # see "Options" UI element
    dataAll <- shiny::getShinyOption("data")
    names <- shiny::getShinyOption("names")
    msg("names: ", paste(names, collapse=" "))
    if (is.null(dataAll))
        stop("must supply a value for the 'data' parameter")

    output$nameUI <- shiny::renderUI({
        #shiny::numericInput("index", "Index", value=1L, min=1L, max=shiny::getShinyOption("ndata"), step=1L)
        names <- shiny::getShinyOption("names")
        shiny::selectInput("name", "Name", choices=names, selected=names[1])
    })

    bufferSave <- function()
    {
        msg("in bufferSave()")
        if (length(names[state$i])) {
            csv <- paste0(names[state$i], ".csv")
            msg("csv='", csv, "'")
            write.csv(as.data.frame(buffer), csv, row.names=FALSE)
            shiny::showNotification(paste0("Saved buffer in '", csv, "'"), type="message")
        }
    }

    bufferClear <- function()
    {
        msg("in bufferClear()")
        if (length(buffer$x) > 0L) {
            buffer$key <<- NULL
            buffer$filename <<- NULL
            buffer$view <<- NULL
            buffer$x <<- NULL
            buffer$y <<- NULL
            buffer$i <<- NULL
            state$savedNumber <<- 0L
        }
    }

    # update from a csv file, if there is one
    bufferUpdate <- function()
    {
        msg("in bufferUpdate()")
        bufferClear()
        w <- which(input$name == names)
        if (length(w)) {
            state$i <<- w
            csv <- paste0(names[state$i], ".csv")
            if (file.exists(csv)) {
                msg("reading from '", csv, "'")
                d <- read.csv(csv, header=TRUE)
                buffer$key <<- d$key
                buffer$filename <<- d$filename
                buffer$view <<- d$view
                buffer$x <<- d$x
                buffer$y <<- d$y
                buffer$i <<- d$i
                state$savedNumber <<- length(d$key)
            } else {
                msg("emptying buffer, since no '", csv, "' with existing data")
            }
        }
    }

    shiny::observeEvent(input$debug, {
        debug <<- input$debug
    })

    shiny::observeEvent(input$name, {
        names <- shiny::getShinyOption("names")
        msg(paste0("observeEvent(index); new name (input$name)=", input$name, "; old name ", names[state$i]))
        bufferSave()
        bufferUpdate()
        newIndex <- which(input$name == names)
        state$bufferLength <- length(buffer$i)
        msg("observeEvent(input$name): buffer (length=", state$bufferLength, "): ", paste(buffer$i, collapse=" "))
        if (length(newIndex)) {
            state$i <<- newIndex[1]
        } else {
            shiny::showNotification("observeEvent(input$name): unknown menu value '", input$name, "'", type="error")
        }
    })

    shiny::observeEvent(input$quit, {
        bufferSave()
        shiny::stopApp()
    })

    output$UIinfo1 <- shiny::renderUI({
        shiny::verbatimTextOutput("info1")
    })

    output$UIinfo2 <- shiny::renderUI({
        shiny::verbatimTextOutput("info2")
    })

    # Find index of data point nearest mouse.  This accounts for the plot
    # scale, and the plot geometry.
    #
    # @param which logical value indicating whether to look in the object's data
    # slot (the default), or in the buffer of saved values.
    #
    # @return list holding `i`, the index of nearest point (or 0 if the plot type
    # is not handled) and `distance`, the distance at that `i`.
    nearestIndex <- function(which="data")
    {
        if (which== "buffer" && length(buffer$x) < 1L)
            return(0)
        data <- if (is.null(state$i) || state$i == 0L) NULL else dataAll[[state$i]]
        ndata <- length(data[["pressure"]])
        if (ndata < 1L)
            return(i=0L, distance=NA)
        if (input$plotChoice == "TS") {
            if (which == "buffer" && state$bufferLength > 0L) {
                X <- rep(NA, ndata)
                Y <- rep(NA, ndata)
                X[buffer$i] <- data[["SA"]][buffer$i]
                Y[buffer$i] <- data[["CT"]][buffer$i]
            } else {
                X <- data[["SA"]]
                Y <- data[["CT"]]
            }
        } else if (input$plotChoice == "densitySpice") {
            if (which == "buffer" && state$bufferLength > 0L) {
                X <- rep(NA, ndata)
                Y <- rep(NA, ndata)
                X[buffer$i] <- data[["spice"]][buffer$i]
                Y[buffer$i] <- data[["sigma0"]][buffer$i]
            } else {
                X <- data[["spice"]]
                Y <- data[["sigma0"]]
            }
        } else if (input$plotChoice == "densityProfile") {
            if (which == "buffer" && state$bufferLength > 0L) {
                X <- rep(NA, ndata)
                Y <- rep(NA, ndata)
                X[buffer$i] <- data[["sigma0"]][buffer$i]
                Y[buffer$i] <- data[["z"]][buffer$i]
            } else {
                X <- data[["sigma0"]]
                Y <- data[["z"]]
            }
        } else if (input$plotChoice == "SProfile") {
            if (which == "buffer" && state$bufferLength > 0L) {
                X <- rep(NA, ndata)
                Y <- rep(NA, ndata)
                X[buffer$i] <- data[["SA"]][buffer$i]
                Y[buffer$i] <- data[["z"]][buffer$i]
            } else {
                X <- data[["SA"]]
                Y <- data[["z"]]
            }
        } else if (input$plotChoice == "TProfile") {
            if (which == "buffer" && state$bufferLength > 0L) {
                X <- rep(NA, ndata)
                Y <- rep(NA, ndata)
                X[buffer$i] <- data[["CT"]][buffer$i]
                Y[buffer$i] <- data[["z"]][buffer$i]
            } else {
                X <- data[["CT"]]
                Y <- data[["z"]]
            }
         } else {
            return(i=0L, distance=NA)
        }
        usr <- par("usr") # for scaling (to get closest data point)

        # I tried scaling by pin (and, at times, by fin), but I cannot reconcile those values with
        # what the geometry of the visible RStudio shiny window.  Since RStudio is very popular, I
        # gave up on the idea.  So, at the moment, the scaling would make sense if the plot were
        # square, but otherwise, the user is advised to get very close to a point of interest.
        #. pin <- par("pin")
        #. msg("pin=", paste(par("pin"), collapse=" "))
        #. msg("fin=", paste(par("fin"), collapse=" "))
        #. dX <- pin[1] * (input$hover$x - X) / (usr[2] - usr[1])
        #. dY <- pin[2] * (input$hover$y - Y) / (usr[4] - usr[3])
        dX <- (input$hover$x - X) / (usr[2] - usr[1])
        dY <- (input$hover$y - Y) / (usr[4] - usr[3])
        #. msg("dX starts: ", paste(head(dX), collapse=" "))
        #. msg("dY starts: ", paste(head(dY), collapse=" "))
        d <- sqrt(dX^2 + dY^2)
        #. msg("d starts: ", paste(head(d), collapse=" "))
        i <- which.min(d)
        msg(sprintf("nearestIndex() has i=%d, dX[i]=%.2f, dY[i]=%.2f, d[i]=%.2f", i, dX[i], dY[i], d[i]))
        list(i=i, distance=d[i])
    }

    output$info1 <- shiny::renderText({
        #. msg("output$info1 with state$i=", state$i)
        rval <- "Click in window to activate hover inspection"
        if (!is.null(input$hover$x)) {
            ni <- nearestIndex("data")
            #. msg("ni$i"=ni$i, ", ni$distance=", ni$distance)
            i <- ni$i
            if (i > 0L) {
                data <- if (is.null(state$i) || state$i == 0L) NULL else dataAll[[state$i]]
                p <- data[["pressure"]]
                rval <- sprintf("x=%.4g y=%.4g near %d-th data point at %.1f dbar",
                    input$hover$x, input$hover$y, i, p[i])
                lastPoint$view <<- input$plotChoice
                lastPoint$x <<- input$hover$x
                lastPoint$y <<- input$hover$y
                lastPoint$i <<- i
            }
        }
        rval
    })

    output$info2 <- shiny::renderText({
        #. msg("output$info1 with state$i=", state$i)
        if (state$bufferLength == 0L) "empty buffer" else paste(state$bufferLength, "in buffer")
    })

    output$plot <- shiny::renderPlot({
        message("output$plot: buffer (length=", state$bufferLength, "): ", paste(buffer$i, collapse=" "))
        colNormal <- gray(0.66, alpha=0.9)
        emphasize <- list(cex=1.4, lwd=3, col=2)
        data <- if (is.null(state$i) || state$i == 0L) NULL else dataAll[[state$i]]
        haveBuffer <- state$bufferLength > 0L # this triggers a redraw since state is reactive
        if (!is.null(data)) {
            par(mgp=mgp)
            highlight <- rep(FALSE, length(data[["pressure"]]))
            # Since state$savedNumber is altered by adding/removing points,
            # the plot will be kept up-to-date.
            if (state$showSaved && state$bufferLength > 0L) {
                #. msg("will overplot with i=", paste(buffer$i, collapse=" "))
                highlight[buffer$i] <- TRUE
            }
            # Define a spline with e.g. S=S(z), with  degree of freedom df=ndata/input$Ndf.
            if (input$data == "spline") {
                pressure <- data[["pressure"]]
                df <- length(pressure) / max(1L, as.integer(input$Ndf))
                P <- seq(min(pressure), max(pressure), 1)
                S <- predict(smooth.spline(pressure, data[["salinity"]], df=df), P)$y
                T <- predict(smooth.spline(pressure, data[["temperature"]], df=df), P)$y
                CTD <- as.ctd(S, T, P, longitude=data[["longitude"]], latitude=data[["latitude"]])
            }
            if (input$plotChoice == "TS") {
                par(mar=c(3.5,3.5,1.5,1.5))
                oce::plotTS(data, eos="gsw",
                    type="n", pch=pch, cex=cex, col=colNormal)
                grid()
                type <- maybeNull(input$plotType, "o")
                if (type %in% c("p", "o"))
                    points(data[["SA"]], data[["CT"]], pch=pch, col=colNormal, cex=cex)
                if (type %in% c("l", "o")) {
                    if (input$data == "raw") lines(data[["SA"]], data[["CT"]], lwd=2, col=colNormal)
                    else lines(CTD[["SA"]], CTD[["CT"]], lwd=2, col=4)
                }
                if (haveBuffer)
                    points(data[["SA"]][buffer$i], data[["CT"]][buffer$i],
                        pch=emphasize$pch, cex=emphasize$cex, lwd=emphasize$lwd, col=emphasize$col)
            } else if (input$plotChoice == "densitySpice") {
                par(mar=c(3.5,3.5,1.5,1.5))
                spice <- data[["spice"]]
                sigma0 <- data[["sigma0"]]
                plot(spice, sigma0, ylim=rev(range(data[["sigma0"]], na.rm=TRUE)),
                    xlab=oce::resizableLabel("spice"),
                    ylab=oce::resizableLabel("sigma0"),
                    type="n", pch=pch, cex=cex, col=colNormal)
                grid()
                type <- maybeNull(input$plotType, "o")
                if (type %in% c("p", "o"))
                    points(spice, sigma0, pch=pch, col=colNormal, cex=cex)
                if (type %in% c("l", "o")) {
                    if (input$data == "raw") lines(spice, sigma0, lwd=2, col=colNormal)
                    else lines(CTD[["spice"]], CTD[["sigma0"]], lwd=2, col=4)
                }
                if (haveBuffer)
                    points(spice[buffer$i], sigma0[buffer$i],
                        pch=emphasize$pch, cex=emphasize$cex, lwd=emphasize$lwd, col=emphasize$col)
            } else if (input$plotChoice == "densityProfile") {
                par(mar=c(3.5,3.5,1.5,1.5))
                # CTD objects respond to [["z"]], but argo objects do not, if oce < 1.5.0
                sigma0 <- data[["sigma0"]]
                z <- oce::swZ(data[["pressure"]], data[["longitude"]][1])
                plot(sigma0, z,
                    xlab=oce::resizableLabel("sigma0"),
                    ylab=oce::resizableLabel("z"),
                    type="n", pch=pch, cex=cex, col=colNormal)
                grid()
                type <- maybeNull(input$plotType, "o")
                if (type %in% c("p", "o"))
                    points(sigma0, z, pch=pch, col=colNormal, cex=cex)
                if (type %in% c("l", "o")) {
                    if (input$data == "raw") lines(sigma0, z, lwd=2, col=colNormal)
                    else lines(CTD[["sigma0"]], CTD[["z"]], lwd=2, col=4)
                }
                if (haveBuffer)
                    points(sigma0[buffer$i], z[buffer$i],
                        pch=emphasize$pch, cex=emphasize$cex, lwd=emphasize$lwd, col=emphasize$col)
            } else if (input$plotChoice == "SProfile") {
                par(mar=c(3.5,3.5,1.5,1.5))
                # CTD objects respond to [["z"]], but argo objects do not, if oce < 1.5.0
                SA <- data[["SA"]]
                z <- oce::swZ(data[["pressure"]], data[["longitude"]][1])
                plot(SA, z,
                    xlab=oce::resizableLabel("SA"),
                    ylab=oce::resizableLabel("z"),
                    type="n", pch=pch, cex=cex, col=colNormal)
                grid()
                type <- maybeNull(input$plotType, "o")
                if (type %in% c("p", "o"))
                    points(SA, z, pch=pch, col=colNormal, cex=cex)
                if (type %in% c("l", "o")) {
                    if (input$data == "raw") lines(SA, z, lwd=2, col=colNormal)
                    else lines(CTD[["SA"]], CTD[["z"]], lwd=2, col=4)
                }
                if (haveBuffer)
                    points(SA[buffer$i], z[buffer$i],
                        pch=emphasize$pch, cex=emphasize$cex, lwd=emphasize$lwd, col=emphasize$col)
            } else if (input$plotChoice == "TProfile") {
                par(mar=c(3.5,3.5,1.5,1.5))
                # CTD objects respond to [["z"]], but argo objects do not, if oce < 1.5.0
                CT <- data[["CT"]]
                z <- oce::swZ(data[["pressure"]], data[["longitude"]][1])
                plot(CT, z,
                    xlab=oce::resizableLabel("CT"),
                    ylab=oce::resizableLabel("z"),
                    type="n", pch=pch, cex=cex, col=colNormal)
                grid()
                type <- maybeNull(input$plotType, "o")
                if (type %in% c("p", "o"))
                    points(CT, z, pch=pch, col=colNormal, cex=cex)
                if (type %in% c("l", "o")) {
                    if (input$data == "raw") lines(CT, z, lwd=2, col=colNormal)
                    else lines(CTD[["CT"]], CTD[["z"]], lwd=2, col=4)
                }
                if (haveBuffer)
                    points(CT[buffer$i], z[buffer$i],
                        pch=emphasize$pch, cex=emphasize$cex, lwd=emphasize$lwd, col=emphasize$col)
            }
        }
    }, height=plotHeight, pointsize=16)

    shiny::observeEvent(input$showSaved, {
        state$showSaved <<- input$showSaved
        msg(paste0("observeEvent(plotOptions) set state$showSaved=", state$showSaved))
    })

    shiny::observeEvent(input$keypressTrigger, {
        msg("input$keypressTrigger: state$bufferLength=", state$bufferLength)
        key <- intToUtf8(input$keypress)
        msg(paste0("key=", key))
        data <- dataAll[[state$i]]
        if (key %in% as.character(0:9) && !is.null(lastPoint$x)) {
            msg(sprintf("%d %f %f %d", as.integer(key), lastPoint$x, lastPoint$y, lastPoint$i))
            n <- length(buffer$key)
            buffer$key[n + 1L] <<- as.integer(key)
            buffer$filename[n + 1L] <<- data[["filename"]]
            buffer$view[n + 1L] <<- lastPoint$view
            buffer$x[n + 1L] <<- lastPoint$x
            buffer$y[n + 1L] <<- lastPoint$y
            buffer$i[n + 1L] <<- lastPoint$i
            state$bufferLength <- n + 1L
        } else if (key == "-") {
            #. msg(sprintf("mouse at %.3f %.3f", input$hover$x, input$hover$y))
            ni <- nearestIndex("buffer")
            msg("keypress '-': ni$i"=ni$i, ", ni$distance=", round(ni$distance, 2))
            i <- ni$i
            w <- which(buffer$i == i)
            if (length(w)) {
                msg("removing buffer entry ", w)
                buffer$key <<- buffer$key[-w]
                buffer$filename <<- buffer$filename[-w]
                buffer$view <<- buffer$view[-w]
                buffer$x <<- buffer$x[-w]
                buffer$y <<- buffer$y[-w]
                buffer$i <<- buffer$i[-w]
                state$bufferLength <<- state$bufferLength - 1L # this is reactive
            }
        } else if (key == "u") {
            n <- length(buffer$key)
            if (n > 0L) {
                buffer$key <<- head(buffer$key, -1L)
                buffer$filename <<- head(buffer$filename, -1L)
                buffer$view <<- input$plotType
                buffer$x <<- head(buffer$x, -1L)
                buffer$y <<- head(buffer$y, -1L)
                buffer$i <<- head(buffer$i, -1L)
                state$bufferLength <<- state$bufferLength - 1L # this is reactive
            }
        } else if (key == "s") {
            if (length(buffer$x) < 1L) {
                shiny::showNotification("No data to show yet. Press '?' to learn how to save data",
                    type="error")
            } else {
                print(file=stderr(), as.data.frame(buffer))
            }
        } else if (key == "w") {
            csv <- paste0(names[state$i], ".csv")
            if (length(buffer$x) < 1L) {
                shiny::showNotification(paste0("No data to write to ", csv, "; Press '?' to learn how to save data"),
                    type="error")
            } else {
                write.csv(as.data.frame(buffer), csv, row.names=FALSE)
                shiny::showNotification(paste0("Wrote data to '", csv, "'"), type="message")
            }
        } else if (key == "?") {
            shiny::showModal(shiny::modalDialog(title="Key-stroke commands",
                    shiny::HTML(keyPressHelp), easyClose=TRUE))
        }
    })
}

#' Inspect an oce CTD-like object
#'
#' This is an R shiny app that displays the contents of CTD-like oce objects, i.e.
#' objects that contain salinity, temperature, pressure, longitude and latitude.
#' A combination of GUI and keystroke actions (type `?` to list) permit
#' flagging data points of interest,
#' and the app can save this information in a CSV file, for later processing.
#' This can be useful for tasks that benefit from a human eye, including
#' identification of features such as mixed layers and intrusions, and of
#' various errors that can result during measurement.
#'
#' GUI controls are provided for selecting the plotted field, and the app also responds to keystrokes.
#' A text box shows the current location of the mouse pointer in the (x,y) coordinates of the
#' graph, as well as the data point that is nearest.  Pressing the `0` key (or any other numeric
#' key, up to `9`) saves this information into a buffer along with the name of the input data
#' and the present plot view.  Points may be removed from this buffer by place the mouse
#' on them and typing '-'.
#'
#' The buffer data may be saved to a CSV file (the name of which is patterned on the input
#' filename). This happens if the context is switched (with the "Name" pulldown menu),
#' if the `w` key is pressed, or if the Quit button is pressed.  If `oceInspectApp()` is used
#' again in the same directory, it will load such files at startup.
#'
#' @param objects either a list containing either CTD-like oce objects or a names of
#' files containing such objects.  For the file-name case, the data are read with
#' [oce::read.oce()].
#'
#' @examples
#' library(oceInspect)
#' # Handle an argo file and a ctd file.
#' if (interactive()) {
#'     f1 <- system.file("extdata/D6903548_029.nc", package="oceInspect")
#'     f2 <- system.file("extdata/BED0302.cnv", package="oceInspect")
#'     oceInspectApp(c(f1, f2))
#' }
#'
#' @export
#'
#' @author Dan Kelley
oceInspectApp <- function(objects=NULL)
{
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages(\"shiny\") for this to work")
    # Convert to a list of oce objects
    ndata <- length(objects)
    if (ndata < 1L)
        stop("must supply at least one element in 'data'")
    data <- lapply(objects, function(x) if (is.character(x)) read.oce(x) else x)
    # For argo data, we focus on the first profile within a cycle, since the other profiles (if they
    # exist) are usually almost all NA values.
    for (i in seq_along(data)) {
        if (inherits(data[[i]], "argo"))
            data[[i]] <- data[[i]][["profile", 1]]
    }
    names <- paste0(gsub("\\..*$", "", gsub(".*/","", sapply(data, function(x) x[["filename"]]))))
    #. print(file=stderr(), names)
    shiny::shinyOptions(data=data, names=names, ndata=length(data))
    print(shiny::shinyApp(ui=ui, server=server))
}

