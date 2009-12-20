#!/usr/bin/env Rscript

# R statistics for the course "Sandsynlighedsregning og statistik"
# at the Department of Mathematical Sciences, University of Copenhagen

##########################################
#
# Project 2
# Part 2
#
##########################################

f <- function(y, mu, sigma) {
    return (1/(y*sqrt(2*pi*sigma^2)))*exp(-(log(y) - mu)^2/(2*sigma^2))
}

# Assignment 1
assignment1 <- function() {
    x <- rnorm(5000, 5, sqrt(0.25));
    y <- exp(x);

    # Set the filename for the plot and plot the graph
    name <- "plot_1.png";
    png(name);
    hist(y, prob=TRUE, nclass=25);

    yval <- seq(0, 1000, 1);
    fval <- f(yval, 5, sqrt(0.25));

    points(yval, fval, type="l");

    dev.off();

    print(median(y))
}

##########################################


##########################################
# Main

assignment1();
