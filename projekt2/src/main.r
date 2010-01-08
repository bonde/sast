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

    set.seed(42);

    mymu <- 5;
    mysigma <- sqrt(0.25);

    xlog <- rlnorm(5000, meanlog=mymu, sdlog=mysigma);

    # Set the filename for the plot and plot the graph
    name <- "plot_1.png";
    png(name);
    hist(xlog, density=NULL, prob=TRUE, nclass=50, xlim=range(0,1000,100));

    yval <- seq(1, 1000, 1);
    fval <- f(yval, mu=mymu, sigma=mysigma);

    points(yval, fval, type="l");

    dev.off();

    print("Assignment 7");
    print("Median");
    print(median(xlog));
    print("Middelvaerdi");
    print(mean(xlog));
    print("Standardafvigelse");
    print(sd(xlog));
    print("Varians");
    print(var(xlog));
    print("");

}

# Assignment 3
assignment2 <- function() {

   avitdata <- read.table("avit.txt", header=TRUE);
   attach(avitdata);

   avitM <- avit[sex==1];
   avitK <- avit[sex==2];

   logavitM <- log(avitM);
   logavitK <- log(avitK);

   print("Number of men in the experiment:");
   print(length(avitM));
   print("Number of females in the experiment:");
   print(length(avitK));

#   print(length(avitK)+length(avitM))
    
    name <- "plot_3_avitM.png";
    png(name);
    hist(avitM, prob=TRUE);

    name <- "plot_3_avitK.png";
    png(name);
    hist(avitK, prob=TRUE);

    name <- "plot_3_logavitK.png";
    png(name);
    hist(logavitK, prob=TRUE);

    name <- "plot_3_logavitM.png";
    png(name);
    hist(logavitM, prob=TRUE);

    name <- "plot_3_avitM_qq.png";
    png(name);
    qqnorm(avitM);
   
    name <- "plot_3_avitK_qq.png";
    png(name);
    qqnorm(avitK);

    name <- "plot_3_logavitM_qq.png";
    png(name);
    qqnorm(logavitM);
   
    name <- "plot_3_logavitK_qq.png";
    png(name);
    qqnorm(logavitK);

    logavit <- log(avit);
    mloga <- mean(logavit);
    print("Mean of the log");
    print(mloga);

    sdloga <- sd(logavit);
    print("log standard deviation:");
    print(sdloga);

    dev.off();

    # Exercise 11
    # logavitM og logavitK ~ N(mean(log(avit)), sd(log(avit)))

    # Exercise 12

    name <- "plot_3_logavitM_with_normal.png";
    png(name);
    hist(logavitM, prob=TRUE);

    yval <- seq(1,12,0.1);
    fval <- dnorm(yval, mean=mloga, sd=sdloga);

    points(yval, fval, type="l");

    dev.off();

    name <- "plot_3_avitM_with_lognormal.png";
    png(name);
    hist(avitM, prob=TRUE);

    yval <- seq(0,8000,1);
    fval <- f(yval, mloga, sdloga);

    points(yval, fval, type="l");

    dev.off();


    name <- "plot_3_logavitK_with_normal.png";
    png(name);
    hist(logavitK, prob=TRUE);

    yval <- seq(1,26,0.1);
    fval <- dnorm(yval, mean=mloga, sd=sdloga);

    points(yval, fval, type="l");

    dev.off();

    name <- "plot_3_avitK_with_lognormal.png";
    png(name);
    hist(avitK, prob=TRUE);

    yval <- seq(0,8000,1);
    fval <- f(yval, mloga, sdloga);

    points(yval, fval, type="l");

    dev.off();

    print(median(avitM));
    print(median(avitK));

    t.test(logavitM, logavitK, var.equal=TRUE);

}


##########################################


##########################################
# Main

assignment1();
assignment2();
