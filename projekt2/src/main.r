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

  return (1/(sqrt(2 * pi * sigma^2) * y) * exp(-((log( y ) - mu)^2 / (2 * sigma^2))))

}

# Assignment 1
assignment1 <- function() {

    set.seed(42)

    mymu <- 5
    mysigma <- sqrt(0.25)

    xlog <- rlnorm(5000, meanlog=mymu, sdlog=mysigma)

    # Set the filename for the plot and plot the graph
    name <- "plot_1.png";
    png(name);
    hist(xlog, prob=TRUE, nclass=50);

    yval <- seq(1, 1000, 1);
    fval <- f(yval, mu=mymu, sigma=mysigma);

    points(yval, fval, type="l", col="red3");

    dev.off();

    print(mean(xlog))
    print(sd(xlog))
    print(var(xlog))

}

##########################################


##########################################
# Main

assignment1();
