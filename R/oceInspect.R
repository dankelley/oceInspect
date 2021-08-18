## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)

#' @importFrom graphics mtext par
#' @importFrom utils head

appName <- "aqc"
appVersion <- "0.1"

msg <- function(..., eos="\n")
{
    cat(file=stderr, ..., eos)
}

plotHeight <- 300
pointsize <- 12
cex <- 1                               # FIXME: let user select via a UI tool
pch <- 20                              # FIXME: let user select via a UI tool
keyPressHelp <- "<ul>
<li> f: <b>f</b>lag points in brushed region</li>
<li> u: <b>u</b>nflag brushed points</li>
<li> z: <b>z</b>oom to selected region</li>
<li> 1: <b>1</b>:1 plot scales (full range)</li>
<li> ?: display this message</li>
</ul>"

maybeNull <- function(x, default)
    if (is.null(x)) default else x

cacheEnv <- new.env(parent=emptyenv())

isCached <- function(name)
{
    name %in% names(cacheEnv)
}

getFromCache <- function(name)
{
    cacheEnv[[name]]
}

storeInCache <- function(name, value)
{
    cacheEnv[[name]] <- value
    invisible(NULL)
}

overallHelp <- "FIXME: write something useful here.  The best plan is to wait until the UI is stable."

pointInBrush <- function(x, y, brush)
    brush$xmin <= x & x <= brush$xmax & brush$ymin <= y & y <= brush$ymax

uiAQC <- shiny::fluidPage(
    shiny::headerPanel(title="", windowTitle="aqc"),
    shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="text-indent:1em; background:#e6f3ff ; .btn.disabled { background-color: red; }",
     shiny::fluidRow(
        #shiny::column(2,
            #shiny::uiOutput(outputId="getID")),
         shiny::uiOutput(outputId="getID"),
         #shiny::column(2, shiny::uiOutput(outputId="getCycle")),
         shiny::uiOutput(outputId="getCycle"),
         #shiny::column(2, shiny::uiOutput(outputId="getPlotType")),
         shiny::uiOutput(outputId="getPlotType"),
        ),
    shiny::fluidRow(shiny::uiOutput(outputId="UIinfo")),
    shiny::fluidRow(
        shiny::column(6,
            shiny::plotOutput("plotTS",
                hover=shiny::hoverOpts("hover_TS"),
                brush=shiny::brushOpts("brush_TS", delay=2000, resetOnNew=TRUE))),
        shiny::column(6,
            shiny::plotOutput("plotProfileRho",
                brush=shiny::brushOpts("brush_rho_profile", delay=2000, resetOnNew=TRUE))),
        ),
    shiny::fluidRow(
        shiny::column(6,
            shiny::plotOutput("plotProfileS",
                brush=shiny::brushOpts("brush_S_profile", delay=2000, resetOnNew=TRUE))),
        shiny::column(6,
            shiny::plotOutput("plotProfileT",
                brush=shiny::brushOpts("brush_T_profile", delay=2000, resetOnNew=TRUE))),
        )
    )

serverAQC <- function(input, output, session)
{
    data <- NULL
    state <- shiny::reactiveValues(
        argo=NULL,
        ID=NULL,
        cycle=NULL,
        argo=NULL,
        flagged=NULL,
        zoomed=NULL)
    tmp <- shiny::getShinyOption("data")
    if (!is.null(tmp)) {
        message("using provided data object")
        data <- tmp
        state$flagged <- rep(FALSE, length(data[["pressure"]]))
        state$zoomed <- rep(TRUE, length(data[["pressure"]]))
    }

    #FIXME: if (!is.null(shiny::getShinyOption("ID"))) {
    #FIXME:     state$ID <- shiny::getShinyOption("ID")
    #FIXME:     shiny::updateTextInput(session, "getID", value=state$ID)
    #FIXME: }
    #FIXME: if (!is.null(shiny::getShinyOption("cycle"))) {
    #FIXME:     state$cycle <- sprintf("%03d", as.integer(shiny::getShinyOption("cycle")))
    #FIXME:     shiny::updateTextInput(session, "getCycle", value=state$cycle)
    #FIXME: }
    if (is.null(data)) {
        age <- shiny::getShinyOption("age")
        destdir <- shiny::getShinyOption("destdir")
        argoServer <- shiny::getShinyOption("argoServer")
        colLand <- shiny::getShinyOption("colLand")
        debug <- shiny::getShinyOption("debug")
        if (!requireNamespace("shiny", quietly=TRUE))
            stop("must install.packages('shiny') for mapApp() to work")
        ## State variable: reactive!
        if (isCached("argoIndex")) {
            message("using cached argoIndex")
            argoIndex <- getFromCache("argoIndex")
        } else {
            message("downloading the argoIndex")
            argoIndex <- argoFloats::getIndex(age=age, destdir=destdir, server=argoServer, debug=debug)
            message("caching the argoIndex")
            storeInCache("argoIndex", argoIndex)
            message("done with caching operation")
        }
        if (isCached("IDs")) {
            IDs <- getFromCache("IDs")
        } else if (isCached("argoIndex")) {
            IDs <- argoIndex[["ID"]]
            storeInCache("IDs", IDs)
        }
        if (isCached("cycles")) {
            cycles <- getFromCache("cycles")
        } else if (isCached("argoIndex")) {
            cycles <- argoIndex[["cycle"]]
            storeInCache("cycles", cycles)
        }
    }

    shiny::observeEvent(input$ID,
        {
            if (is.null(data)) {
                if (0 == nchar(input$ID)) {
                    message("empty ID")
                } else {
                    if (input$ID %in% IDs) {
                        state$ID <<- input$ID
                        message("set state$ID to ", state$ID)
                    } else {
                        message("unknown ID: ", input$ID)
                    }
                }
            }
        })

    shiny::observeEvent(input$cycle,
        {
            if (is.null(data)) {
                if (0 == nchar(input$cycle)) {
                    message("ignoring cycle, since ID is empty")
                } else {
                    cycle <- sprintf("%03d", as.integer(input$cycle))
                    message("state$ID=", state$ID)
                    message("head(IDs)=", paste(head(IDs), collapse=" "))
                    message("head(cycles)=", paste(head(cycles), collapse=" "))
                    message("input$cycle=", input$cycle)
                    message("cycle=", cycle)
                    if (is.null(state$cycle) || cycle != state$cycle) { # new cycle
                        if (cycle %in% cycles[IDs == state$ID]) {
                            state$cycle <<- cycle
                            message("set state$cycle to ", state$cycle)
                            message("subsetting for ID")
                            sub <- argoFloats::subset(argoIndex, ID=state$ID)
                            message("subsetting for cycle")
                            sub <- argoFloats::subset(sub, cycle=state$cycle)
                            message("getting profile")
                            profile <- argoFloats::getProfiles(sub)
                            message("reading profile")
                            data <- argoFloats::readProfiles(profile)[[1]] # FIXME: what to store?
                            state$flagged <<- rep(FALSE, length(data[["pressure"]]))
                            state$zoomed <<- rep(TRUE, length(data[["pressure"]]))
                            message("done")
                        } else {
                            message("Float ", state$ID, " has no cycle '", input$cycle, "' (translated to ", cycle, ")")
                        }
                    }
                }
            }
        })

    output$getID <- shiny::renderUI({
        if (is.null(data)) {
            if (isCached("argoIndex"))
                shiny::textInput("ID", "Float ID", value="1901574", width="8em")
        }
        })

    output$getCycle <- shiny::renderUI({
        if (is.null(data)) {
            if (!is.null(state$ID))
                shiny::textInput("cycle", "Float cycle", value="1", width="8em")
        }
    })

    output$getPlotType <- shiny::renderUI({
        if (!is.null(data)) {
            shiny::selectInput("plotType", "Plot", choices=c("p","l","o"), selected="o")
        }
    })

    output$UIinfo <- shiny::renderUI({
        shiny::verbatimTextOutput("info")
    })

    output$info <- shiny::renderText({
        msg <- "Place mouse inside a plot to see info."
        if (!is.null(input$hover_TS$x))
            msg <- sprintf("S=%.3f theta=%.3f", input$hover_TS$x, input$hover_TS$y)
        msg
    })

    output$plotTS <- shiny::renderPlot({
        if (!is.null(data)) {
            par(mar=c(3,3,1.5,1.5))
            S <- data[["salinity"]]
            T <- data[["temperature"]]
            p <- data[["pressure"]]
            flagged <- state$flagged
            # message(sprintf("plotTS INITIAL sum(flagged)=%d, length(flagged)=%d", sum(flagged), length(flagged)))
            if (!is.null(state$zoomed)) {
                S <- S[state$zoomed]
                T <- T[state$zoomed]
                p <- p[state$zoomed]
                flagged <- flagged[state$zoomed]
                # message(sprintf("plotTS LATER   sum(flagged)=%d, length(flagged)=%d", sum(flagged), length(flagged)))
            }
            oce::plotTS(oce::as.ctd(S, T, p),
                eos="unesco",
                type=maybeNull(input$plotType, "o"),
                pch=pch,
                cex=cex,
                col=if (is.null(flagged)) 1 else ifelse(flagged, 2, 1))
            mtext(paste0("Argo float ", state$ID, ", cycle ", state$cycle), line=1)
        }
    }, height=plotHeight, pointsize=pointsize)

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
        # message(input$keypress)
        # message(key)
        if (key == "f") { # flag points in flagged region
            message("flagging")
            if (!is.null(input$brush_TS)) {
                state$flagged[pointInBrush(data[["salinity"]], data[["theta"]], input$brush_TS)] <- TRUE
            } else if (!is.null(input$brush_rho_profile)) {
                state$flagged[pointInBrush(data[["sigmaTheta"]], data[["pressure"]], input$brush_rho_profile)] <- TRUE
            } else if (!is.null(input$brush_T_profile)) {
                state$flagged[pointInBrush(data[["theta"]], data[["pressure"]], input$brush_T_profile)] <- TRUE
            } else if (!is.null(input$brush_S_profile)) {
                state$flagged[pointInBrush(data[["salinity"]], data[["pressure"]], input$brush_S_profile)] <- TRUE
            }
        } else if (key == "u") { # unflag points in brushed region
            message("unflagging")
            if (!is.null(input$brush_TS)) {
                state$flagged[pointInBrush(data[["salinity"]], data[["theta"]], input$brush_TS)] <- FALSE
            } else if (!is.null(input$brush_rho_profile)) {
                state$flagged[pointInBrush(data[["sigmaTheta"]], data[["pressure"]], input$brush_rho_profile)] <- FALSE
            } else if (!is.null(input$brush_T_profile)) {
                state$flagged[pointInBrush(data[["theta"]], data[["pressure"]], input$brush_T_profile)] <- FALSE
            } else if (!is.null(input$brush_S_profile)) {
                state$flagged[pointInBrush(data[["salinity"]], data[["pressure"]], input$brush_S_profile)] <- FALSE
            }
        } else if (key == "1") { # 1:1 plots
            message("switching to 1:1 plot view")
            state$zoomed <<- rep(TRUE, length(data[["pressure"]]))
        } else if (key == "z") { # zoom
            message("zooming")
            if (!is.null(input$brush_TS)) {
                state$zoomed <- pointInBrush(data[["salinity"]], data[["theta"]], input$brush_TS)
            } else if (!is.null(input$brush_rho_profile)) {
                state$zoomed <- pointInBrush(data[["sigmaTheta"]], data[["pressure"]], input$brush_rho_profile)
            } else if (!is.null(input$brush_T_profile)) {
                state$zoomed <- pointInBrush(data[["theta"]], data[["pressure"]], input$brush_T_profile)
            } else if (!is.null(input$brush_S_profile)) {
                state$zoomed <- pointInBrush(data[["salinity"]], data[["pressure"]], input$brush_S_profile)
            }
        } else if (key == "?") {
            shiny::showModal(shiny::modalDialog(title="Key-stroke commands",
                    shiny::HTML(keyPressHelp), easyClose=TRUE))
        }
    })

}

#' Argo QC skeleton
#'
#'
#' @param data an oce object (optional). If provided, then `ID`, `cycle`, `age`,
#' `server` and `destdir` are all ignored.
#'
#' @param ID ...
#'
#' @param cycle ...
#'
#' @param age numeric value indicating how old a downloaded file
#' must be (in days), for it to be considered out-of-date.  The
#' default, `argoDefaultIndexAge()`, limits downloads to once per day, as a way
#' to avoid slowing down a workflow with a download that might take
#' a minute or so. Note that setting `age=0` will force a new
#' download, regardless of the age of the local file.
#'
#' @param server ...
#'
#' @param destdir ...
#'
#' @param debug integer value that controls how much information this app prints
#' to the console as it works.  The default value of 0 leads to a fairly limited
#' amount of printing, while higher values lead to more information. This information
#' can be helpful in diagnosing problems or bottlenecks.
#'
#' @examples
#' # Case 1. pre-loaded data
## f <- system.file("extdata/D4900785_048.nc", package="argoFloats")
#' f <- system.file("extdata/R6903548_029.nc", package="aqc")
#' if (nchar(f)) {
#'     d <- oce::read.argo(f)
#'     aqcApp(d)
#'}
#'
#' @export
#'
#' @author Dan Kelley
aqcApp <- function(
    data=NULL,
    ID=NULL,
    cycle=NULL,
    age=argoFloats::argoDefaultIndexAge(),
    server=argoFloats::argoDefaultServer(),
    destdir=argoFloats::argoDefaultDestdir(),
    debug=0)
{
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages(\"shiny\") for this to work")
    debug <- as.integer(max(0, min(debug, 3))) # put in range from 0 to 3
    shiny::shinyOptions(
        data=data,
        age=age,
        destdir=destdir,
        argoServer=server,
        ID=ID,
        cycle=cycle,
        debug=debug)
    print(shiny::shinyApp(ui=uiAQC, server=serverAQC))
}

