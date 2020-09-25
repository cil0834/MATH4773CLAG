# Load libraries needed
library(shiny)
library(purrr)
library(rootSolve)
library(scatterplot3d)
data(trees)
library(plotly)
library(plot3D)

#' threeD_app
#'
#' @return
#' @export
#' This function runs an app that models a piecewise function of the spruce data set.
#' The app allows the user to adjust the confidence interval of two x knots to find the optimal x knot locations to maximize
#' the r^2 value
#' @examples
