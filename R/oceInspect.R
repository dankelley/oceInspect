## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)

#' @importFrom graphics grid mtext par
#' @importFrom utils head write.csv

appName <- "oceInspect"
appVersion <- "0.1"

debug <- FALSE

pluralize <- function(n, name)
{
    if (n == 1L) paste0(n, " ", name) else paste0(n, " ", name, "s")
}

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

cacheEnv <- new.env(parent=emptyenv())

overallHelp <- "FIXME: write something useful here.  The best plan is to wait until the UI is stable."

pointInBrush <- function(x, y, brush)
    brush$xmin <= x & x <= brush$xmax & brush$ymin <= y & y <= brush$ymax

ui <- shiny::fluidPage(
    shiny::headerPanel(title="", windowTitle="oceInspect"),
    shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="text-indent:1em ; background:#e6f3ff ; .btn.disabled { background-color: red; }",
    shiny::fluidRow(
        shiny::column(3,
            shiny::selectInput("plotType", "Plot Type",
                choices=c("Point"="p", "Line"="l", "Point+Line"="o"),
                selected="o")),
        shiny::column(3,
            shiny::selectInput("plotChoice", "View",
                choices=c("T-S"="TS", "density-spice"="densitySpice"),
                selected="Temperature-Salinity")),
        shiny::column(2,
            shiny::checkboxInput("showSaved", "Show Saved", TRUE)),
        shiny::column(2,
            shiny::checkboxInput("debug", "Debug", FALSE))
        ),
    shiny::fluidRow(
        shiny::column(7,
            shiny::uiOutput(outputId="UIinfo1")),
        shiny::column(5,
            shiny::uiOutput(outputId="UIinfo2"))),
    shiny::fluidRow(
        shiny::plotOutput("plot",
            hover=shiny::hoverOpts("hover"),
            brush=shiny::brushOpts("brush", delay=2000, resetOnNew=TRUE)))
    )

server <- function(input, output, session)
{
    data <- NULL
    lastPoint <- list(x=NULL, y=NULL, i=NULL, view=NULL, file=NULL)
    allPoints <- list(key=NULL, x=NULL, y=NULL, i=NULL, view=NULL, filename=NULL)
    state <- shiny::reactiveValues(
        savedNumber=0,                 # making this reactive means info2 updates when points are saved
        showSaved=FALSE)               # see "Options" UI element
    msg <- function(..., eos="\n")
    {
        if (!is.null(input$debug) && input$debug)
            cat(file=stderr(), ..., eos)
    }
    tmp <- shiny::getShinyOption("data")
    if (!is.null(tmp)) {
        data <- tmp
        for (n in names(data@data)) {
            if (is.matrix(data@data[[n]]))
                data@data[[n]] <- data@data[[n]][,1]
            else if (n %in% c("longitude", "latitude", "time"))
                data@data[[n]] <- data@data[[n]][1]
        }
    } else {
        stop("must supply a value for the 'data' parameter")
    }

    output$UIinfo1 <- shiny::renderUI({
        shiny::verbatimTextOutput("info1")
    })

    output$UIinfo2 <- shiny::renderUI({
        shiny::verbatimTextOutput("info2")
    })

    output$info1 <- shiny::renderText({
        rval <- "Click in window to activate hover inspection"
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
        csv <- paste0(gsub("\\..*$", "", gsub(".*/","", data[["filename"]])), ".csv")
        sprintf("%s in %s ", pluralize(state$savedNumber, "point"), csv)
    })

    output$plot <- shiny::renderPlot({
        colNormal <- gray(0.75, alpha=0.9)
        colHighlight <- 2
        if (!is.null(data)) {
            par(mgp=mgp)
            highlight <- rep(FALSE, length(data[["pressure"]]))
            msg("about to plot (state$showSaved=", state$showSaved, ")")
            # Since state$savedNumber is altered by adding/removing points,
            # the plot will be kept up-to-date.
            if (state$showSaved && state$savedNumber > 0L) {
                msg("should show saved now; i=", paste(allPoints$i, collapse=" "))
                highlight[allPoints$i] <- TRUE
                msg(paste("indices: ", paste(which(highlight), collapse=" ")))
            }
            if (input$plotChoice == "TS") {
                par(mar=c(3.5,3.5,1.5,1.5))
                oce::plotTS(data,#oce::as.ctd(S, T, p),
                    eos="gsw",
                    type=maybeNull(input$plotType, "o"),
                    pch=pch,
                    cex=ifelse(highlight, 2*cex, cex),
                    col=colNormal)
                grid()
                if (length(allPoints$i))
                    points(data[["SA"]][allPoints$i], data[["CT"]][allPoints$i],
                        pch=pch, cex=2*cex, col=colHighlight)
            } else if (input$plotChoice == "densitySpice") {
                spice <- data[["spice"]]
                sigma0 <- data[["sigma0"]]
                par(mar=c(3.5,3.5,1.5,1.5))
                plot(spice, sigma0, ylim=rev(range(sigma0, na.rm=TRUE)),
                    xlab=oce::resizableLabel("spice"),
                    ylab=oce::resizableLabel("sigma0"),
                    type=maybeNull(input$plotType, "o"),
                    pch=pch,
                    cex=ifelse(highlight, 2*cex, cex),
                    col=colNormal)
                grid()
                if (length(allPoints$i))
                    points(data[["spice0"]][allPoints$i], data[["sigma0"]][allPoints$i],
                        pch=pch, cex=2*cex, col=colHighlight)
            }
            #mtext(sprintf("%d points saved", state$savedNumber))
        }
    }, height=plotHeight, pointsize=16)

    output$plotProfileRho <- shiny::renderPlot({
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
        key <- intToUtf8(input$keypress)
        msg(paste0("key=", key))
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
            csv <- paste0(gsub("\\..*$", "", gsub(".*/","", data[["filename"]])), ".csv")
            if (length(allPoints$x) < 1L) {
                shiny::showNotification(paste0("No data to write to ", csv, "; Press '?' to learn how to save data"),
                    type="error")
            } else {
                write.csv(as.data.frame(allPoints), csv, row.names=FALSE)
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
#' @param data an oce object (optional). If provided, then `ID`, `cycle`, `age`, `server` and `destdir` are all ignored.
#'
#' @param debug integer value that controls how much information this app prints to the console as it works.
#' The default value of 0 leads to a fairly limited amount of printing, while higher values lead to
#' more information. This information can be helpful in diagnosing problems or bottlenecks.
#'
#' @examples
#' library(oceInspect)
#' # Example 1: argo file (specified as an oce object)
#' if (interactive()) {
#'     f <- system.file("extdata/R6903548_029.nc", package="oceInspect")
#'     d <- oce::read.oce(f)
#'     oceInspectApp(d)
#' }
#' # Example 2: ctd file (specified by name)
#' if (interactive()) {
#'     f <- system.file("extdata/ctd.cnv", package="oceInspect")
#'     oceInspectApp(f)
#' }
#'
#' @export
#'
#' @author Dan Kelley
oceInspectApp <- function( data=NULL, debug=0)
{
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages(\"shiny\") for this to work")
    debug <- as.integer(max(0, min(debug, 3))) # put in range from 0 to 3
    if (length(data) != 1L)
        stop("for now, 'data' must be of length 1")
    if (is.character(data)) {
        dataName <- data
        data <- oce::read.oce(dataName)
    }
    shiny::shinyOptions(data=data, debug=debug)
    print(shiny::shinyApp(ui=ui, server=server))
}

