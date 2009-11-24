#!/usr/bin/env Rscript

# R statistics for the course "Sandsynlighedsregning og statistik"
# at the Department of Mathematics, University of Copenhagen
#
# Ulrik Bonde, 2009

##########################################
#
# Project 1
#
##########################################

# Assignment 1
assignment1 <- function() {
    # We define n as an array with the values 8, 20 and 100
    n    <- c(8, 20, 100);
    prob <- 0.25;

    # Traverse the n-array
    for (size in n) {
        # Define E
        E <- 0:size;

        # Define the probability function
        sshfunk <- dbinom(E, size, prob);

        # Set the filename for the plot and plot the graph
        name <- paste(size, "_plot_1.png", sep="");
        png(name);
        plot(E, sshfunk, "h", ylab="Sandsynlighedsfunktion");
        dev.off();

        # Clean up for next iteration
        rm(name, E);
    }
}

##########################################

# Assigment 2
assignment2 <- function() {
    # Define the simulation using a random binom
    n    <- 1000;
    size <- c(8, 20, 100);
    prob <- 0.25;

    for (s in size) {
        # Do a simulation and count the observations of each occurrence
        sim <- rbinom(n, s, prob);
        simulation <- table(sim);

        # Get the values coresponding to an occurrence
        values <- as.numeric(names(simulation));

        theoretical <- 1000*dbinom(values, s, prob);

        # Combine the simulation and the theoretical vectors
        comb <- rbind(simulation, theoretical);

        # Set up the filename for the plot, draw, close pipe and release variables
        name <- paste(s, "_sim-theo_plot_2.png", sep="");
        png(name);
        barplot(comb, beside=TRUE, legend.text=TRUE);
        dev.off();
    }
}

##########################################

# Assigment 3
assignment3 <- function() {
    # Define the simulation using a random binom
    n    <- 1000;
    size <- c(8, 20, 100);
    prob <- 0.25;

    for (s in size) {
        # Do a simulation and print
        sim <- rbinom(n, s, prob);
        print(paste("Mean for simulation with size ",s, " is: ", mean(sim), sep=""));
        print(paste("Var for simulation with size ",s, " is: ", var(sim), sep=""));
        print("");
    }
}

##########################################

# Assigment 5
assignment5 <- function() {
    # Define the simulation using a random binom
    n    <- 1000;
    size <- c(8, 20, 100);
    prob <- 0.25;

    for (s in size) {
        # Do a simulation and count the observations of each occurrence
        sim <- rbinom(n, s, prob);
        simulation <- table(sim);

        # Get the values coresponding to an occurrence
        values <- as.numeric(names(simulation));

        # Set up the filename for the plot, draw, close pipe and release variables
        histname <- paste(s, "_hist_5.png", sep="");
        png(histname);
        hist(sim, prob=TRUE);
        curve(dnorm(x, mean = mean(sim), sd = sd(sim)), add=TRUE);
        dev.off();
    }
}

##########################################
# Main

assignment1();
assignment2();
assignment3();
assignment5();
