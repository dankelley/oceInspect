# vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4

library(argoFloats)

#' @importFrom graphics grid lines mtext par points
#' @importFrom grDevices gray
#' @importFrom oce as.ctd read.oce
#' @importFrom stats predict smooth.spline
#' @importFrom utils head read.csv write.csv

appName <- "oceInspect"
appVersion <- "0.2"

debug <- FALSE


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

#. pointInBrush <- function(x, y, brush)
#.     brush$xmin <= x & x <= brush$xmax & brush$ymin <= y & y <= brush$ymax

cacheEnv <- new.env(parent=emptyenv())

#. overallHelp <- "FIXME: write something useful here.  The best plan is to wait until the UI is stable."

ui <- shiny::fluidPage(
    shiny::headerPanel(title="", windowTitle="oceInspect"),
    shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="text-indent:1em ; background:#e6f3ff ; .btn.disabled { background-color: red; }",
    shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(outputId="nameUI")),
        #?shiny::column(1,
        #?    shiny::numericInput("index", "Index", 1L, min=1L, max=shiny::getShinyOption("ndata"), step=1L)),
        shiny::column(2,
            shiny::selectInput("plotChoice", "View",
                choices=c("T-S"="TS", "\u03C3-\u03C0"="densitySpice"),
                selected="Temperature-Salinity")),
        shiny::column(2,
            shiny::selectInput("plotType", "Type",
                choices=c("Point"="p", "Line"="l", "Both"="o"),
                selected="o")),
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
            hover=shiny::hoverOpts("hover"),
            brush=shiny::brushOpts("brush", delay=2000, resetOnNew=TRUE)))
    )

server <- function(input, output, session)
{
    lastPoint <- list(view=NULL, x=NULL, y=NULL, i=NULL)
    buffer <- list(key=NULL, filename=NULL, view=NULL, x=NULL, y=NULL, i=NULL)
    state <- shiny::reactiveValues(
        i=0,
        savedNumber=0,                 # making this reactive means info2 updates when points are saved
        showSaved=FALSE)               # see "Options" UI element
    msg <- function(..., eos="\n")
    {
        if (!is.null(input$debug) && input$debug)
            cat(file=stderr(), ..., eos)
    }
    dataAll <- shiny::getShinyOption("data")
    names <- shiny::getShinyOption("names")
    message("names: ", paste(names, collapse=" "))
    if (is.null(dataAll))
        stop("must supply a value for the 'data' parameter")
    #? data <- dataAll[[state$index]]
    #? message("index=", index)
    #?data <- dataAll[[index]] # FIXME: does this work?
    #?DAN3<<-data

    output$nameUI <- shiny::renderUI({
        #shiny::numericInput("index", "Index", value=1L, min=1L, max=shiny::getShinyOption("ndata"), step=1L)
        names <- shiny::getShinyOption("names")
        shiny::selectInput("name", "Name", choices=names, selected=names[1])
    })

    bufferSave <- function()
    {
        message("in bufferSave()")
        if (length(names[state$i])) {
            csv <- paste0(names[state$i], ".csv")
            message("csv='", csv, "'")
            write.csv(as.data.frame(buffer), csv, row.names=FALSE)
            shiny::showNotification(paste0("Wrote data to '", csv, "'", type="message"))
        }
    }

    bufferClear <- function()
    {
        message("in bufferClear()")
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
        message("in bufferUpdate()")
        bufferClear()
        w <- which(input$name == names)
        if (length(w)) {
            state$i <<- w
            csv <- paste0(names[state$i], ".csv")
            if (file.exists(csv)) {
                message("reading from '", csv, "'")
                d <- read.csv(csv, header=TRUE)
                buffer$key <<- d$key
                buffer$filename <<- d$filename
                buffer$view <<- d$view
                buffer$x <<- d$x
                buffer$y <<- d$y
                buffer$i <<- d$i
                state$savedNumber <<- length(d$key)
            } else {
                message("emptying buffer, since no '", csv, "' with existing data")
            }
        }
    }

    shiny::observeEvent(input$name, {
        names <- shiny::getShinyOption("names")
        message(paste0("observeEvent(index); new name (input$name)=", input$name, "; old name ", names[state$i]))
        bufferSave()
        bufferUpdate()
        newIndex <- which(input$name == names)
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
            if (which == "buffer") {
                X <- rep(NA, ndata)
                Y <- rep(NA, ndata)
                X[buffer$i] <- data[["SA"]][buffer$i]
                Y[buffer$i] <- data[["CT"]][buffer$i]
            } else {
                X <- data[["SA"]]
                Y <- data[["CT"]]
            }
        } else if (input$plotChoice == "densitySpice") {
            if (which == "buffer") {
                X <- rep(NA, ndata)
                Y <- rep(NA, ndata)
                X[buffer$i] <- data[["spice"]][buffer$i]
                Y[buffer$i] <- data[["sigma0"]][buffer$i]
            } else {
                X <- data[["spice"]]
                Y <- data[["sigma0"]]
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
        #. message("pin=", paste(par("pin"), collapse=" "))
        #. message("fin=", paste(par("fin"), collapse=" "))
        #. dX <- pin[1] * (input$hover$x - X) / (usr[2] - usr[1])
        #. dY <- pin[2] * (input$hover$y - Y) / (usr[4] - usr[3])
        dX <- (input$hover$x - X) / (usr[2] - usr[1])
        dY <- (input$hover$y - Y) / (usr[4] - usr[3])
        #. message("dX starts: ", paste(head(dX), collapse=" "))
        #. message("dY starts: ", paste(head(dY), collapse=" "))
        d <- sqrt(dX^2 + dY^2)
        #. message("d starts: ", paste(head(d), collapse=" "))
        i <- which.min(d)
        message(sprintf("nearestIndex() has i=%d, dX[i]=%.2f, dY[i]=%.2f, d[i]=%.2f", i, dX[i], dY[i], d[i]))
        list(i=i, distance=d[i])
    }

    output$info1 <- shiny::renderText({
        #. message("output$info1 with state$i=", state$i)
        rval <- "Click in window to activate hover inspection"
        if (!is.null(input$hover$x)) {
            ni <- nearestIndex("data")
            #. message("ni$i"=ni$i, ", ni$distance=", ni$distance)
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
        #. message("output$info1 with state$i=", state$i)
        sprintf("%s in buffer ", pluralize(state$savedNumber, "point"))
    })

    output$plot <- shiny::renderPlot({
        #. message("output$plot with state$i=", state$i, " (length(dataAll)=", length(dataAll), ")")
        colNormal <- gray(0.66, alpha=0.9)
        colHighlight <- 2
        data <- if (is.null(state$i) || state$i == 0L) NULL else dataAll[[state$i]]
        if (!is.null(data)) {
            par(mgp=mgp)
            highlight <- rep(FALSE, length(data[["pressure"]]))
            # Since state$savedNumber is altered by adding/removing points,
            # the plot will be kept up-to-date.
            if (state$showSaved && state$savedNumber > 0L) {
                msg("will overplot with i=", paste(buffer$i, collapse=" "))
                highlight[buffer$i] <- TRUE
            }
            # Define X and Y for the line, depending on whether it shows data or spline.
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
                oce::plotTS(data,
                    eos="gsw",
                    type="n",
                    pch=pch,
                    cex=ifelse(highlight, 2*cex, cex),
                    col=colNormal)
                grid()
                type <- maybeNull(input$plotType, "o")
                if (type %in% c("p", "o"))
                    points(data[["SA"]], data[["CT"]], pch=pch, col=colNormal, cex=cex)
                # Lines are black for data, blue for spline
                if (type %in% c("l", "o")) {
                    if (input$data == "raw") {
                        lines(data[["SA"]], data[["CT"]], lwd=2, col=colNormal)
                    } else {
                        lines(CTD[["SA"]], CTD[["CT"]], lwd=2, col=4)
                    }
                }
                if (length(buffer$i))
                    points(data[["SA"]][buffer$i], data[["CT"]][buffer$i],
                        pch=pch, cex=2*cex, col=colHighlight)
            } else if (input$plotChoice == "densitySpice") {
                par(mar=c(3.5,3.5,1.5,1.5))
                plot(data[["spice"]], data[["sigma0"]], ylim=rev(range(data[["sigma0"]], na.rm=TRUE)),
                    xlab=oce::resizableLabel("spice"),
                    ylab=oce::resizableLabel("sigma0"),
                    type="n",
                    pch=pch,
                    cex=ifelse(highlight, 2*cex, cex),
                    col=colNormal)
                grid()
                type <- maybeNull(input$plotType, "o")
                if (type %in% c("p", "o"))
                    points(data[["spice"]], data[["sigma0"]], pch=pch, col=colNormal, cex=cex)
                # Lines are black for data, blue for spline
                if (type %in% c("l", "o")) {
                    if (input$data == "raw") {
                        lines(data[["spice"]], data[["sigma0"]], lwd=2, col=colNormal)
                    } else {
                        lines(CTD[["spice"]], CTD[["sigma0"]], lwd=2, col=4)
                    }
                }
                if (length(buffer$i))
                    points(data[["spice"]][buffer$i], data[["sigma0"]][buffer$i],
                        pch=pch, cex=2*cex, col=colHighlight)
            }
        }
    }, height=plotHeight, pointsize=16)

    output$plotProfileRho <- shiny::renderPlot({
        data <- dataAll[[state$i]]
        if (!is.null(data)) {
            par(mar=c(1,3,3,1))
            S <- data[["salinity"]]
            T <- data[["temperature"]]
            p <- data[["pressure"]]
            lon <- data[["longitude"]][1]
            lat <- data[["latitude"]][1]
            oce::plotProfile(oce::as.ctd(S, T, p, longitude=lon, latitude=lat),
                xtype="sigmaTheta",
                type=maybeNull(input$plotType, "o"),
                pch=pch,
                cex=cex,
                col=col)
        }
    }, height=plotHeight, pointsize=pointsize)

    output$plotProfileT <- shiny::renderPlot({
        data <- dataAll[[state$i]]
        if (!is.null(data)) {
            par(mar=c(1,3,3,1))
            S <- data[["salinity"]]
            T <- data[["temperature"]]
            p <- data[["pressure"]]
            lon <- data[["longitude"]][1]
            lat <- data[["latitude"]][1]
            oce::plotProfile(oce::as.ctd(S, T, p, longitude=lon, latitude=lat),
                xtype="temperature",
                type=maybeNull(input$plotType, "o"),
                pch=pch,
                cex=cex,
                col=col)
        }
    }, height=plotHeight, pointsize=pointsize)

    output$plotProfileS <- shiny::renderPlot({
        data <- dataAll[[state$i]]
        if (!is.null(data)) {
            par(mar=c(1,3,3,1))
            S <- data[["salinity"]]
            T <- data[["temperature"]]
            p <- data[["pressure"]]
            lon <- data[["longitude"]][1]
            lat <- data[["latitude"]][1]
            oce::plotProfile(oce::as.ctd(S, T, p, longitude=lon, latitude=lat),
                xtype="salinity",
                type=maybeNull(input$plotType, "o"),
                pch=pch,
                cex=cex,
                col=col)
        }
    }, height=plotHeight, pointsize=pointsize)

    shiny::observeEvent(input$showSaved, {
        state$showSaved <<- input$showSaved
        msg(paste0("observeEvent(plotOptions) set state$showSaved=", state$showSaved))
    })

    shiny::observeEvent(input$keypressTrigger, {
        message("observing input$keypressTrigger with state$i=", state$i, " (length(dataAll)=", length(dataAll))
        key <- intToUtf8(input$keypress)
        msg(paste0("key=", key))
        data <- dataAll[[state$i]]
        if (key %in% as.character(0:9) && !is.null(lastPoint$x)) {
            msg(sprintf("%d %f %f %d", as.integer(key), lastPoint$x, lastPoint$y, lastPoint$i))
            n <- length(buffer$key)
            buffer$key[n+1] <<- as.integer(key)
            buffer$filename[n+1] <<- data[["filename"]]
            buffer$view[n+1] <<- lastPoint$view
            buffer$x[n+1] <<- lastPoint$x
            buffer$y[n+1] <<- lastPoint$y
            buffer$i[n+1] <<- lastPoint$i
            state$savedNumber <- state$savedNumber + 1L
        } else if (key == "-") {
            #. message(sprintf("mouse at %.3f %.3f", input$hover$x, input$hover$y))
            ni <- nearestIndex("buffer")
            message("keypress '-': ni$i"=ni$i, ", ni$distance=", round(ni$distance, 2))
            i <- ni$i
            w <- which(buffer$i == i)
            if (length(w)) {
                message("removing buffer entry ", w)
                buffer$key <<- buffer$key[-w]
                buffer$filename <<- buffer$filename[-w]
                buffer$view <<- buffer$view[-w]
                buffer$x <<- buffer$x[-w]
                buffer$y <<- buffer$y[-w]
                buffer$i <<- buffer$i[-w]
                state$savedNumber <- state$savedNumber - 1L
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
                state$savedNumber <- state$savedNumber - 1L
                msg("removed last point; new length=", length(buffer$i), " or ", state$savedNumber)
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
#' This is an R shiny app that displays the contents of either an argo-class or ctd-class object.
#' GUI and keystroke actions permit flagging data points of interest, and the app can save
#' a list of such points, for separate processing.  Although simple, this scheme can be useful
#' in tasks such as identifying points of interest (e.g. mixed layers, intrusions, erroneous
#' data) as part of quality-control processing or scientific analyses.
#'
#' GUI controls are provided for selecting the plotted field, and the app also responds to keystrokes.
#' A text box shows the current location of the mouse pointer in the (x,y) coordinates of the
#' graph, as well as the data point that is nearest.  Pressing the '0' key (or any other numeric
#' key, up to '9') saves this information into a buffer along with the name of the input data
#' and the present plot view.  Pressing the 'u' key performs an undo operation by removing the
#' item that was most recently added to the buffer.  Pressing the 's' key shows the contents
#' of this buffer, and pressing 'w' writes those contents to a comma-separated-value file whose
#' name, patterned on the name of the input data file, is indicated in a text box above the plot.
#'
#' @param objects either a list containing either CTD-like oce objects or a names of
#' files containing such objects.  For the file-name case, the data are read with
#' [oce::read.oce()].  A CTD-like object is one that holds salinity, temperature, pressure,
#' longitude and latitude.
#'
#' @examples
#' library(oceInspect)
#' # Example 1: argo file (specified as an oce object)
#' if (interactive()) {
#'     f1 <- system.file("extdata/R6903548_029.nc", package="oceInspect")
#'     d1 <- oce::read.oce(f1)
#'     oceInspectApp(d1)
#' }
#' # Example 2: ctd file (specified by name)
#' if (interactive()) {
#'     f2 <- system.file("extdata/ctd.cnv", package="oceInspect")
#'     oceInspectApp(f2)
#' }
#'
#' @export
#'
#' @author Dan Kelley
oceInspectApp <- function(objects=NULL)
{
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages(\"shiny\") for this to work")
    # Convert to a lilst of oce objects
    ndata <- length(objects)
    if (ndata < 1L)
        stop("must supply at least one element in 'data'")
    data <- lapply(objects, function(x) if (is.character(x)) read.oce(x) else x)
    #?DAN1<<-data
    # Take just first column of argo data
    for (i in seq_along(data)) {
        if (inherits(data[[i]], "argo")) {
            #.message("i=", i, " is argo")
            for (name in names(data[[i]]@data)) {
                #.message("name=",name)
                if (name %in% c("latitude", "longitude", "time")) {
                    #.message("name=", name, ": take 1st value")
                    data[[i]]@data[[name]] <- data[[i]]@data[[name]][1]
                } else {
                    #.message("name=", name, ": take 1st column")
                    data[[i]]@data[[name]] <- data[[i]]@data[[name]][,1, drop=FALSE]
                }
            }
        }
    }
    #?DAN2<<-data
    names <- paste0(gsub("\\..*$", "", gsub(".*/","", sapply(data, function(x) x[["filename"]]))))
    #. print(file=stderr(), names)
    shiny::shinyOptions(data=data, names=names, ndata=length(data))
    print(shiny::shinyApp(ui=ui, server=server))
}

