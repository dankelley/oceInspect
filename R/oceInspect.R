## vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)

#' @importFrom graphics grid lines mtext par points
#' @importFrom grDevices gray
#' @importFrom oce as.ctd read.oce
#' @importFrom stats predict smooth.spline
#' @importFrom utils head write.csv

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
        shiny::column(5,
            shiny::uiOutput(outputId="UIinfo2"))
        ),
    shiny::fluidRow(
        shiny::plotOutput("plot",
            hover=shiny::hoverOpts("hover"),
            brush=shiny::brushOpts("brush", delay=2000, resetOnNew=TRUE)))
    )

server <- function(input, output, session)
{
    lastPoint <- list(view=NULL, x=NULL, y=NULL, i=NULL)
    allPoints <- list(key=NULL, filename=NULL, view=NULL, x=NULL, y=NULL, i=NULL)
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

    shiny::observeEvent(input$name, {
        names <- shiny::getShinyOption("names")
        message(paste0("observeEvent(index); new name (input$name)=", input$name, "; old name ", names[state$i]))
        if (length(allPoints$x) > 0L) {
            csv <- paste0(names[state$i], ".csv")
            message("csv='", csv, "'")
            write.csv(as.data.frame(allPoints), csv, row.names=FALSE)
            shiny::showNotification(paste0("Wrote data to '", csv, "'", type="message"))
            allPoints$key <<- NULL
            allPoints$filename <<- NULL
            allPoints$view <<- NULL
            allPoints$x <<- NULL
            allPoints$y <<- NULL
            allPoints$i <<- NULL
            state$savedNumber <- 0L
        }
        w <- which(input$name == names)
        if (length(w))
            state$i <<- w[1]
    })

    output$UIinfo1 <- shiny::renderUI({
        shiny::verbatimTextOutput("info1")
    })

    output$UIinfo2 <- shiny::renderUI({
        shiny::verbatimTextOutput("info2")
    })

    output$info1 <- shiny::renderText({
        #. message("output$info1 with state$i=", state$i)
        rval <- "Click in window to activate hover inspection"
        data <- dataAll[[state$i]]
        if (!is.null(input$hover$x)) {
            usr <- par("usr") # for scaling (to get closest data point)
            pin <- par("pin")
            if (input$plotChoice == "TS") {
                X <- data[["SA"]]
                Y <- data[["CT"]]
                p <- data[["pressure"]]
                dX <- pin[1] * (input$hover$x - X) / (usr[2] - usr[1])
                dY <- pin[2] * (input$hover$y - Y) / (usr[4] - usr[3])
                d <- dX^2 + dY^2
                i <- which.min(d)
            } else if (input$plotChoice == "densitySpice") {
                X <- data[["spice"]]
                Y <- data[["sigma0"]]
                p <- data[["pressure"]]
                dX <- pin[1] * (input$hover$x - X) / (usr[2] - usr[1])
                dY <- pin[2] * (input$hover$y - Y) / (usr[4] - usr[3])
                d <- dX^2 + dY^2
                i <- which.min(d)
            }
            rval <- sprintf("x=%.4g y=%.4g near %d-th data point at %.1f dbar",
                input$hover$x, input$hover$y, i, p[i])
            lastPoint$view <<- input$plotChoice
            lastPoint$x <<- input$hover$x
            lastPoint$y <<- input$hover$y
            lastPoint$i <<- i
        }
        rval
    })

    output$info2 <- shiny::renderText({
        #. message("output$info1 with state$i=", state$i)
        sprintf("%s in buffer ", pluralize(state$savedNumber, "point"))
    })

    output$plot <- shiny::renderPlot({
        message("output$plot with state$i=", state$i, " (length(dataAll)=", length(dataAll), ")")
        colNormal <- gray(0.66, alpha=0.9)
        colHighlight <- 2
        data <- dataAll[[state$i]]
        if (!is.null(data)) {
            par(mgp=mgp)
            highlight <- rep(FALSE, length(data[["pressure"]]))
            # Since state$savedNumber is altered by adding/removing points,
            # the plot will be kept up-to-date.
            if (state$showSaved && state$savedNumber > 0L) {
                msg("will overplot with i=", paste(allPoints$i, collapse=" "))
                highlight[allPoints$i] <- TRUE
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
                if (length(allPoints$i))
                    points(data[["SA"]][allPoints$i], data[["CT"]][allPoints$i],
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
                if (length(allPoints$i))
                    points(data[["spice"]][allPoints$i], data[["sigma0"]][allPoints$i],
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
            n <- length(allPoints$key)
            allPoints$key[n+1] <<- as.integer(key)
            allPoints$filename[n+1] <<- data[["filename"]]
            allPoints$view[n+1] <<- lastPoint$view
            allPoints$x[n+1] <<- lastPoint$x
            allPoints$y[n+1] <<- lastPoint$y
            allPoints$i[n+1] <<- lastPoint$i
            state$savedNumber <- state$savedNumber + 1L
        } else if (key == "u") {
            n <- length(allPoints$key)
            if (n > 0L) {
                allPoints$key <<- head(allPoints$key, -1L)
                allPoints$filename <<- head(allPoints$filename, -1L)
                allPoints$view <<- input$plotType
                allPoints$x <<- head(allPoints$x, -1L)
                allPoints$y <<- head(allPoints$y, -1L)
                allPoints$i <<- head(allPoints$i, -1L)
                state$savedNumber <- state$savedNumber - 1L
                msg("removed last point; new length=", length(allPoints$i), " or ", state$savedNumber)
            }
        } else if (key == "s") {
            if (length(allPoints$x) < 1L) {
                shiny::showNotification("No data to show yet. Press '?' to learn how to save data",
                    type="error")
            } else {
                print(file=stderr(), as.data.frame(allPoints))
            }
        } else if (key == "w") {
            csv <- paste0(names[state$i], ".csv")
            if (length(allPoints$x) < 1L) {
                shiny::showNotification(paste0("No data to write to ", csv, "; Press '?' to learn how to save data"),
                    type="error")
            } else {
                write.csv(as.data.frame(allPoints), csv, row.names=FALSE)
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
    print(file=stderr(), names)
    shiny::shinyOptions(data=data, names=names, ndata=length(data))
    print(shiny::shinyApp(ui=ui, server=server))
}

