##########################################################################################
#Final Project/BIOS 815                                                                  #
#Empirical Power of a T-Test - R Shiny App                                               #
#Hannah Baiyor                                                                           #
#December 19, 2025                                                                       #
#                                                                                        #
#This function determines either the empirical power of a t-test or the required sample  #
#size to achieve a stated empirical power.                                               #
#The empirical power is computed by generating 100 samples                               #
#with a given hypothesized mean, standard deviation, and size, performing                #
#a t-test on each sample/set of samples, and determining how                             #         
#many iterations produce a statistically significant result.                             #
#                                                                                        #
#To determine the sample size required to achieve a given empirical power,               #
#sample(s) of size n=10 will be generated and empirical power tested in the same         #
#manner as above.If this power is less than the specified beta, sample                   #
#size will increase by 5 and the test will repeat, increasing sample size by 5 each      #
#time until an empirical power of greater than or equal to beta is achieved.             #
#                                                                                        #
#The user will be presented with several input fields in the R Shiny App. These inputs   #
#are defined as follows:                                                                 #
#testType: The user will select if they wish to conduct a one-sample, paired, or two     #
#independent samples t-test                                                              #
#powerType: The user will select if they wish to determine the empirical power of the    #
#test or if they wish to determine the required N for the test                           #
#mu1: Hypothesized mean of sample 1                                                      #
#sd1: Hypothesized standard deviation of sample 1                                        #
#muhyp: Hypothetical mean against which to compare a one-sample t-test. If no value is   #
#specified, the default is 0. This option will only be presented if a one-sample test is #
#selected.                                                                               #
#mu2: Hypothesized mean of sample 2.This option will only be displayed for paired and    #
#two-sample tests.                                                                       #
#sd2: Hypothesized standard deviation of sample 2. This option will only be displayed    #
#for paired and two-sample tests.                                                        #
#rho: Correlation coefficient for paired data, required for paired test. This option     #
#will only be displayed for paired t-tests.                                              #
#varType: Variance type for two-sample t-tests, equal or unequal. This option will       #
#only be displayed for two-sample t-tests.                                               #
#n1: Size of sample 1. This option will only be displayed if the empirical power is      #
#the desired output type.                                                                #
#n2: Optional. Size of sample 2. This option will only be displayed if the empirical     #
#power is the desired output type and a two-sample test is being performed. If left      #
#blank, the default is matching sample sizes (n2=n1).                                    #
#beta: desired power. This option will only be displayed if "Required N" is the          #
#output type desired.                                                                    #
#alpha: desired type I error level                                                       #
#alt: "less", "two.sided" or "greater". This determines the type of alternative          #
#hypothesis being tested. The default is "two.sided".                                    #
#plotOn: Option to display plots                                                         #
#meanOn: Option to display means/SDs of generated samples                                #
#                                                                                        #
##########################################################################################

#Define functions used in the Shiny App
library(mvtnorm)

reps <- 100 #set number of reps

#create a function to generate various types of samples, outputting samp1/samp2 matricies
samp.gen <- function(mu1,sd1,mu2,sd2,rho,n1,n2,type){
  samp1 <- matrix(NA,nrow=reps,ncol=n1) #initialize matrix to hold each iteration of sample 1
  if (type=="equal.var"|type=="unequal.var"){
    samp2 <- matrix(NA,nrow=reps,ncol=n2) #initialize matrix to hold each iteration of sample 2
  } else {
    samp2 <- matrix(NA,nrow=reps,ncol=n1)
  }
  samp.diff <- matrix(NA,nrow=reps,ncol=n1)
  samps <- matrix(NA,nrow=n1,ncol=reps*2) #initialize matrix to hold each iteration of correlated samples
  for (i in 1:reps){
    if (type=="paired"){
      sig <- matrix(c(sd1^2,rho*sd1*sd2,rho*sd1*sd2,sd2^2),ncol=2) #generate covariance matrix
      samps[,((2*i)-1):(2*i)] <- rmvnorm(n1,mean=c(mu1,mu2),sigma=sig) #generate two samples of given means and standard deviations, correlated based on the covariance matrix defined above
      #break up large matrix of all samples into two matrices, samp1 and samp2, for consistency with other tests
      samp1[i,] <- samps[,((2*i)-1)] 
      samp2[i,] <- samps[,(2*i)]
      samp.diff[i,] <- samp1[i,]-samp2[i,] #create matrix of difference between samps for paired samples
    } else if (type=="one.samp"){
      samp1[i,] <- rnorm(n1,mu1,sd1) #generate one independent sample
    } else {
      samp1[i,] <- rnorm(n1,mu1,sd1) #generate one independent sample
      samp2[i,] <- rnorm(n2,mu2,sd2) #generate second independent sample
    }
  }
  list('sample1'=samp1,'sample2'=samp2,'samplediff'=samp.diff)
}

#create a function to run various types of t-tests, outputting the list "test", the "pass" vector, and empirical power
fun.test <- function(samp1,samp2,type,alt,alpha,muhyp){
  test <- list(NA) #initialize list to hold output of each iteration of the t-test
  pass <- NULL
  for (i in 1:reps){
    if (type=="paired"){ #paired t-test
      test[[i]] <- t.test(samp1[i,],samp2[i,],alternative=alt,paired=T)
    } else if (type=="one.samp"){ #one-sample t-test
      test[[i]] <- t.test(samp1[i,],alternative=alt,mu=muhyp)
    } else if (type=="unequal.var"){ #two-sample unequal variance
      test[[i]] <- t.test(samp1[i,],samp2[i,],alternative=alt,var.equal=F)
    } else if (type=="equal.var"){ #two sample equal variance
      test[[i]] <- t.test(samp1[i,],samp2[i,],alternative=alt,var.equal=T)
    }
    
    if (test[[i]]$p.value<alpha){ #check p-value vs. alpha and record passes/failures
      pass[i] <- 1
    } else {
      pass[i] <- 0
    }
  }
  e.power <- sum(pass)/reps #compute empirical power based on passes/failures
  list('test'=test,'pass'=pass,'e.power'=e.power)
}

emp.power.1 <- function(mu1, sd1, mu2, sd2, rho, n1, n2, alpha, beta, muhyp, type, alt, out){
  set.seed(4320458) #set seed
  
  #if either n1 or n2 is missing, use equal sample sizes
  if (type!="one.samp"){
    if (is.na(n2)==T & is.na(n1)==F){
      n2 <- n1
    }
    if (is.na(n1)==T & is.na(n2)==F){
      n1 <- n2
    }
  }
  
  #generate an error if rho is entered but any means or sds are missing
  if (type=="paired"){
    if (is.na(rho)==F & (is.na(mu1)==T|is.na(mu2)==T|is.na(sd1)==T|is.na(sd2)==T)){
      stop("Rho is defined, but a required sample mean or standard deviation is missing. Please ensure two sets of means and sds are input for a paired test.")
    }
  }  
      
     
  
  ############################
  #empirically determining n #
  ############################
   if (out=="size"){
    e.power <- 0 #initialize empirical power variable at 0
    n <- 5 #start with sample size of 10 (initialize at n=5 as we will immediately add 5 to n at the beginning of our loop)
    while (e.power<beta){ #check if empirical power is greater than/equal to desired power
      n <- n+5 #for each iteration, increase sample size by 5
      samplist <- samp.gen(mu1,sd1,mu2,sd2,rho,n1=n,n2=n,type) #generate samples
      samp1 <- samplist$sample1
      samp2 <- samplist$sample2
      fun.out <- fun.test(samp1,samp2,type,alt,alpha,muhyp) #run t-tests
      e.power <- fun.out$e.power #determine empirical power
    }
    return(list("beta"=beta,"n"=n,"samplist"=samplist)) #once desired power is reached, output final sample size
  }
  
  ###############################   
  # Determining empirical power #
  ###############################
   if (out=="power"){
    samplist <- samp.gen(mu1,sd1,mu2,sd2,rho,n1,n2,type)
    samp1 <- samplist$sample1
    samp2 <- samplist$sample2
    fun.out <- fun.test(samp1,samp2,type,alt,alpha,muhyp)
    e.power <- fun.out$e.power
    return(list("e.power"=e.power,"samplist"=samplist))
  }
}

#function for finding means/sds of each generated sample
e.means <- function(samplist,type){
  samp1 <- samplist$sample1
  samp2 <- samplist$sample2
  samp.diff <- samplist$samplediff
  #set up values for means and plots displays if requested
  mean.1 <- apply(samp1,1,mean) #means for sample 1
  sd.1 <- apply(samp1,1,sd) #sds for sample 1
  mean.2 <- apply(samp2,1,mean) #means for sample 2
  sd.2 <- apply(samp2,1,sd) #sds for sample 2
  mean.diff <- apply(samp.diff,1,mean) #means of differences for paired samples
  sd.diff <- apply(samp.diff,1,sd) #sds of differences for paired samples
  
  if (type=="paired"){ #for a paired t-test, print out means/sds of differences as well
    mean.mat <- matrix(c(mean.1,sd.1,mean.2,sd.2,mean.diff,sd.diff),nrow=reps,ncol=6)
    colnames(mean.mat) <- c("Sample 1 Mean","Sample 1 SD","Sample 2 Mean","Sample 2 SD","Means of Difference","SDs of Difference")
  } else if (sum(is.na(mean.2))>0){ #for a one-sample t-test, only print means and sds of the single sample
    mean.mat <- matrix(c(mean.1,sd.1),nrow=reps,ncol=2)
    colnames(mean.mat) <- c("Sample 1 Mean","Sample 1 SD")
  } else {
    mean.mat <- matrix(c(mean.1,sd.1,mean.2,sd.2),nrow=reps,ncol=4) #for two-sample t-test, print means/sds of both samples
    colnames(mean.mat) <- c("Sample 1 Mean","Sample 1 SD","Sample 2 Mean","Sample 2 SD") 
  }
  return(mean.mat)
}  

#function to generate plots
e.plot <- function(samplist,type,muhyp){
  samp1 <- samplist$sample1
  samp2 <- samplist$sample2
  samp.diff <- samplist$samplediff
  #set up values for means and plots displays if requested
  mean.1 <- apply(samp1,1,mean) #means for sample 1
  sd.1 <- apply(samp1,1,sd) #sds for sample 1
  mean.2 <- apply(samp2,1,mean) #means for sample 2
  sd.2 <- apply(samp2,1,sd) #sds for sample 2
  mean.diff <- apply(samp.diff,1,mean) #means of differences for paired samples
  sd.diff <- apply(samp.diff,1,sd) #sds of differences for paired samples
  
  # Define the mean and standard deviation
  mean_val1 <- mean(mean.1)
  sd_val1 <- mean(sd.1)
  mean_val2 <- mean(mean.2)
  sd_val2 <- mean(sd.2)
  mean_diff <- mean(mean.diff)
  sd_diff <- mean(sd.diff)
  
  #set up min/max values for axes (range of +/- 3*sd from the mean)
  samp1.low <- mean_val1 - 3 * sd_val1
  samp2.low <- mean_val2 - 3 * sd_val2
  sampdiff.low <- mean_diff - 3 * sd_diff
  samp1.hi <- mean_val1 + 3 * sd_val1
  samp2.hi <- mean_val2 + 3 * sd_val2
  sampdiff.hi <- mean_diff + 3 * sd_diff
  
  #find the minimum of low values and max of high values for each set of samples to be plotted and use those as the
  #min and max for the axes
  if (type=="paired"){
    x.low <- min(sampdiff.low,0)
    x.hi <- max(sampdiff.hi,0)
    min.sd <- sd_diff
  } else if (is.na(mean_val2)==T){
    x.low <- min(samp1.low,muhyp)
    x.hi <- max(samp1.hi,muhyp)
    min.sd <- sd_val1
  } else {
    x.low <- min(samp1.low,samp2.low)
    x.hi <- max(samp1.hi,samp2.hi)
    min.sd <- min(sd_val1,sd_val2)
  }
  max.y <- 1/(min.sd*sqrt(2*pi))
  
  
  # Plot the normal curve
  plot(NA, NA, xlim = c(x.low,x.hi), ylim = c(0,max.y),
       xlab = 'X values', ylab = 'Density')
  if (type=="paired"){
    curve(dnorm(x, mean = mean_diff, sd = sd_diff),
          from = sampdiff.low,
          to = sampdiff.hi,
          col = 'red',
          add = T)
    lines(c(0,0),c(0,1),col='blue')
    title(main = 'Normal Distribution Density Function \nPaired T-Test')
    legend("topright",legend=c("Sample 1 - Sample 2","Diff=0"),col=c('red','blue'),lty=1)
  } else {
    curve(dnorm(x, mean = mean_val1, sd = sd_val1),
          from = samp1.low,
          to = samp1.hi,
          col = 'red',
          add=T)
    if (is.na(mean_val2)==F){
      curve(dnorm(x, mean = mean_val2, sd = sd_val2),
            from = samp2.low,
            to = samp2.hi,
            col = 'blue',
            add=T)
      title(main = 'Normal Distribution Density Function \nTwo Independent Samples T-Test')
      legend("topright",legend=c("Sample 1","Sample 2"),col=c("red","blue"),lty=1)
    } else {
      lines(c(muhyp,muhyp),c(0,1),col='blue')
      title(main = 'Normal Distribution Density Function \nOne Sample T-Test')
      legend("topright",legend=c("Sample 1","Hypothesized Mean"),col=c('red','blue'),lty=1)
    }
  }
}

#Build the Shiny App

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Empirical Power of a T-Test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          radioButtons(inputId="testType",
                      label="Test Type",
                      choices=c("One Sample T-Test","Paired Samples T-Test","Two Independent Samples T-Test")),
          radioButtons(inputId="powerType",
                      label="Find power or find N?",
                      choices=c("Power","Required N")),
          
          numericInput(inputId="mu1",
                       label="Mu 1",
                       value=14.5),
          numericInput(inputId="sd1",
                       label="SD 1",
                       value=3.1),
          conditionalPanel(
            condition = "input.testType=='One Sample T-Test'",
            numericInput(inputId="muhyp",
            label="Hypothesized Mean",
            value=0)
          ),
          conditionalPanel(
            condition = "input.testType!='One Sample T-Test'",
            numericInput(inputId="mu2",
            label="Mu 2",
            value=15.8),
            numericInput(inputId="sd2",
                         label="SD 2",
                         value=3)
          ),
          conditionalPanel(
            condition = "input.testType=='Paired Samples T-Test'",
            numericInput(inputId="rho",
                         label="Rho",
                         value=0.7)
          ),
          conditionalPanel(
            condition = "input.testType=='Two Independent Samples T-Test'",
            selectInput(inputId = "varType",
                         label = "Equal or Unequal Variance Assumed?",
                         choices = c("Equal Variance","Unequal Variance"))
          ),
          conditionalPanel(
            condition = "input.powerType=='Power'",
            numericInput(inputId="n1",
                         label="N1",
                         value=100),
            conditionalPanel(
              condition = "input.testType=='Two Independent Samples T-Test'",
              numericInput(inputId="n2",
                           label="N2",
                           value=100)
            )
          ),
          conditionalPanel(
            condition = "input.powerType=='Required N'",
            numericInput(inputId="beta",
                         label="Power",
                         value=0.8)
          ),
          numericInput(inputId="alpha",
                       label="Alpha Level",
                       value=0.05),
          selectInput(inputId="alt",
                      label="Alternative Hypothesis Type",
                      choices=c("less","two.sided","greater"),
                      selected="two.sided"),
            
          actionButton("run_test", "Run T-Test")    
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("results"),
           radioButtons(inputId="plotOn",
                        label="Plots On?",
                        choices=c("Yes","No"),
                        selected="No"),
           conditionalPanel(
             condition = "input.plotOn == 'Yes'",
             plotOutput("plot")
           ),
           radioButtons(inputId="meanOn",
                        label="Show table of sample means and standard deviations?",
                        choices=c("Yes","No"),
                        selected="No"),
           conditionalPanel(
             condition = "input.meanOn == 'Yes'",
             tableOutput("means")
           ),
           
           
        )
    )
)

# Define server logic 
server <- function(input, output) {
  
  #update test type when test is run
  test_type <- eventReactive(input$run_test,{
    if (input$testType=="Paired Samples T-Test"){
      type <- "paired"
    } else if (input$testType=="Two Independent Samples T-Test" & input$varType=="Equal Variance"){
      type <- "equal.var"
    } else if (input$testType=="Two Independent Samples T-Test" & input$varType == "Unequal Variance"){
      type <- "unequal.var"
    } else if (input$testType=="One Sample T-Test"){
      type <- "one.samp"
    }
    type
  })
  
  #update desired output type when test is run
  out_type <- eventReactive(input$run_test,{
    if (input$powerType=="Power"){
      out <- "power"
    } else if (input$powerType=="Required N"){
      out <- "size"
    }
    out
  })
  
  
  #get empirical power when test is run
  test_results <- eventReactive( input$run_test, {
    emp.power.1(mu1=input$mu1,sd1=input$sd1,mu2=input$mu2,sd2=input$sd2,rho=input$rho,n1=input$n1,n2=input$n2,alpha=input$alpha,beta=input$beta,muhyp=input$muhyp,type=test_type(),alt=input$alt,out=out_type())
  })
  
  #output power/required N information
  output$results <- renderPrint({
    if (out_type()=="power"){
      paste("The empirical power of the test is",test_results()$e.power)
    } else if (out_type()=="size"){
      paste("To acheive desired power level",test_results()$beta,"a sample size of",test_results()$n,"is required.")
    }
    
  })
  
  #output means/sds in a table
  output$means <- renderTable({
    e.means(test_results()$samplist,test_type())
  })
  
  #output plots
  get_plots <- eventReactive(input$run_test,{
    e.plot(test_results()$samplist,test_type(),input$muhyp) #uses a reactive event such that muhyp does not automatically update on plots, only updates when test is run
  })
    output$plot <- renderPlot({
      get_plots()
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
