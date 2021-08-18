## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)

#' @importFrom graphics grid mtext par
#' @importFrom utils head write.csv

appName <- "oceInspect"
appVersion <- "0.1"

msg <- function(..., eos="\n")
{
    cat(file=stderr(), ..., eos)
}

plotHeight <- 500
pointsize <- 12
cex <- 1                               # FIXME: let user select via a UI tool
pch <- 20                              # FIXME: let user select via a UI tool
mgp <- c(2, 0.7, 0)
keyPressHelp <- "FIXME (these are wrong) <ul>
<li> 0: save mouse location as category '0' (and similar for 1 through 9)</li>
<li> u: undo last saved operation</li>
<li> s: show saved points</li>
<li> w: write saved points to oceInspect.csv</li>
<li> ?: display this message</li>
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
    shiny::fluidRow(shiny::uiOutput(outputId="UIinfo")),
    shiny::fluidRow(
        shiny::column(3,
            shiny::selectInput("plotType", "Plot Type",
                choices=c("Point"="p", "Line"="l", "Point+Line"="o"),
                selected="o")),
        shiny::column(3,
            shiny::selectInput("plotChoice", "View",
                choices=c("T-S"="TS", "density-spice"="densitySpice"),
                selected="Temperature-Salinity"))
        ),
    shiny::fluidRow(
        shiny::plotOutput("plot",
            hover=shiny::hoverOpts("hover"),
            brush=shiny::brushOpts("brush", delay=2000, resetOnNew=TRUE)))
    )

server <- function(input, output, session)
{
    data <- NULL
    lastPoint <- list(x=NULL, y=NULL, i=NULL, view=NULL)
    allPoints <- list(key=NULL, x=NULL, y=NULL, i=NULL, view=NULL)
    state <- shiny::reactiveValues(
        flagged=NULL,
        zoomed=NULL)
    tmp <- shiny::getShinyOption("data")
    if (!is.null(tmp)) {
        data <- tmp
        for (n in names(data@data)) {
            if (is.matrix(data@data[[n]]))
                data@data[[n]] <- data@data[[n]][,1]
            else if (n %in% c("longitude", "latitude", "time"))
                data@data[[n]] <- data@data[[n]][1]
        }
        state$flagged <- rep(FALSE, length(data[["pressure"]]))
        state$zoomed <- rep(TRUE, length(data[["pressure"]]))
    } else {
        stop("must supply a value for the 'data' parameter")
    }

    output$UIinfo <- shiny::renderUI({
        shiny::verbatimTextOutput("info")
    })

    output$info <- shiny::renderText({
        msg <- "Place mouse inside plot to see info."
        if (!is.null(input$hover$x)) {
            msg <- sprintf("x=%.4g y=%.4g", input$hover$x, input$hover$y)
            usr <- par("usr") # for scaling (to get closest data point)
            if (input$plotChoice == "TS") {
                X <- data[["SA"]]
                Y <- data[["CT"]]
                p <- data[["pressure"]]
                dX <- (input$hover$x - X) / (usr[2]-usr[1])
                dY <- (input$hover$y - Y) / (usr[4]-usr[2])
                d <- dX^2 + dY^2
                i <- which.min(d)
                msg <- paste0(msg, sprintf(" near %d-th point at %.1f dbar: SA=%.4f, CT=%.4f",
                        i, p[i], X[i], Y[i]))
            } else if (input$plotChoice == "densitySpice") {
                X <- data[["spice"]]
                Y <- data[["sigma0"]]
                p <- data[["pressure"]]
                dX <- (input$hover$x - X) / (usr[2]-usr[1])
                dY <- (input$hover$y - Y) / (usr[4]-usr[2])
                d <- dX^2 + dY^2
                i <- which.min(d)
                msg <- paste0(msg, sprintf(" near %d-th point at %.1f dbar: spice=%.4f, sigma0=%.4f",
                        i, p[i], X[i], Y[i]))
            }
            lastPoint$view <<- input$plotChoice
            lastPoint$x <<- input$hover$x
            lastPoint$y <<- input$hover$y
            lastPoint$i <<- i
            #message(str(lastPoint))
        }
        msg
    })

    output$plot <- shiny::renderPlot({
        if (!is.null(data)) {
            msg(input$plotChoice)
            par(mgp=mgp)
            if (input$plotChoice == "TS") {
                par(mar=c(3.5,3.5,1.5,1.5))
                oce::plotTS(data,#oce::as.ctd(S, T, p),
                    eos="gsw",
                    type=maybeNull(input$plotType, "o"),
                    pch=pch,
                    cex=cex)
                grid()
            } else if (input$plotChoice == "densitySpice") {
                spice <- data[["spice"]]
                sigma0 <- data[["sigma0"]]
                par(mar=c(3.5,3.5,1.5,1.5))
                plot(spice, sigma0, ylim=rev(range(sigma0, na.rm=TRUE)),
                    xlab=oce::resizableLabel("spice"),
                    ylab=oce::resizableLabel("sigma0"),
                    type=maybeNull(input$plotType, "o"),
                    pch=pch,
                    cex=cex)
                grid()
            }
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
            flagged <- state$flagged
            if (!is.null(state$zoomed)) {
                S <- S[state$zoomed]
                T <- T[state$zoomed]
                p <- p[state$zoomed]
                flagged <- flagged[state$zoomed]
            }
            oce::plotProfile(oce::as.ctd(S, T, p, longitude=lon, latitude=lat),
                xtype="sigmaTheta",
                type=maybeNull(input$plotType, "o"),
                pch=pch,
                cex=cex,
                col=if (is.null(flagged)) 1 else ifelse(flagged, 2, 1))
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
            flagged <- state$flagged
            if (!is.null(state$zoomed)) {
                S <- S[state$zoomed]
                T <- T[state$zoomed]
                p <- p[state$zoomed]
                flagged <- flagged[state$zoomed]
            }
            oce::plotProfile(oce::as.ctd(S, T, p, longitude=lon, latitude=lat),
                xtype="temperature",
                type=maybeNull(input$plotType, "o"),
                pch=pch,
                cex=cex,
                col=if (is.null(flagged)) 1 else ifelse(flagged, 2, 1))
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
            flagged <- state$flagged
            if (!is.null(state$zoomed)) {
                S <- S[state$zoomed]
                T <- T[state$zoomed]
                p <- p[state$zoomed]
                flagged <- flagged[state$zoomed]
            }
            oce::plotProfile(oce::as.ctd(S, T, p, longitude=lon, latitude=lat),
                xtype="salinity",
                type=maybeNull(input$plotType, "o"),
                pch=pch,
                cex=cex,
                col=if (is.null(flagged)) 1 else ifelse(flagged, 2, 1))
        }
    }, height=plotHeight, pointsize=pointsize)

    shiny::observeEvent(input$keypressTrigger, {
        key <- intToUtf8(input$keypress)
        #message(input$keypress)
        #message(key)
        if (key %in% as.character(0:9) && !is.null(lastPoint$x)) {
            #message("flagging number: ", key, "...")
            message(sprintf("%d %f %f %d", as.integer(key), lastPoint$x, lastPoint$y, lastPoint$i))
            n <- length(allPoints$key)
            allPoints$key[n+1] <<- as.integer(key)
            allPoints$view[n+1] <<- lastPoint$view
            allPoints$x[n+1] <<- lastPoint$x
            allPoints$y[n+1] <<- lastPoint$y
            allPoints$i[n+1] <<- lastPoint$i
        } else if (key == "u") {
            n <- length(allPoints$key)
            if (n > 0L) {
                allPoints$key <<- head(allPoints$key, -1L)
                allPoints$view <<- input$plotType
                allPoints$x <<- head(allPoints$x, -1L)
                allPoints$y <<- head(allPoints$y, -1L)
                allPoints$i <<- head(allPoints$i, -1L)
                msg("removed last point.")
            }
        } else if (key == "s") {
            print(file=stderr(), allPoints)
        } else if (key == "w") {
            names(allPoints) <- c("key", "x", "y", "i", "view")
            write.csv(allPoints, "oceInspect.csv", row.names=FALSE)
        } else if (key == "?") {
            shiny::showModal(shiny::modalDialog(title="Key-stroke commands",
                    shiny::HTML(keyPressHelp), easyClose=TRUE))
        }
    })

}

#' Inspect an oce CTD-like object
#'
#'
#' @param data an oce object (optional). If provided, then `ID`, `cycle`, `age`,
#' `server` and `destdir` are all ignored.
#'
#' @param debug integer value that controls how much information this app prints
#' to the console as it works.  The default value of 0 leads to a fairly limited
#' amount of printing, while higher values lead to more information. This information
#' can be helpful in diagnosing problems or bottlenecks.
#'
#' @examples
#' library(oceInspect)
#' if (interactive()) {
#'     f <- system.file("extdata/R6903548_029.nc", package="oceInspect")
#'     d <- oce::read.oce(f)
#'     oceInspectApp(d)
#' }
#'
#' @export
#'
#' @author Dan Kelley
oceInspectApp <- function(
    data=NULL,
    debug=0)
{
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages(\"shiny\") for this to work")
    debug <- as.integer(max(0, min(debug, 3))) # put in range from 0 to 3
    shiny::shinyOptions(
        data=data,
        debug=debug)
    print(shiny::shinyApp(ui=ui, server=server))
}

