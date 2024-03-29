
# FNS FOR THEORY SANITY CHECKS  -----------------------------------------------------------

# fns in this section are only used for theory sanity checks in analyze_applied_NMAR.R;
#  the analyses are easiest to conduct by simply calling evalues.RD(), as we do in the smoking cessation example
#  in analysis script

# full expression
bound1 = function(pr, pa, p1, p0,
                  # sens params
                  B, rd_0) {
  
  pr*(p1 - p0*B)*(pa + (1-pa)/B) + (1-pr)*rd_0
}


# bias factor required to shift ATE to the value "true"
# this is equivalent to Eq 4.1 in Supplement, i.e.: (1 / (2*p0*pa) ) * ( sqrt( (alpha + gamma)^2 + 4*p1*p0*pa*(1-pa) ) - alpha - gamma )
# true: the true rd_0 to shift to
get_B = function(pr, pa, p1, p0, rd_0, true = 0) {
  optim( par = 1,  # start value for B
         lower = 1,
         upper = 300,
         # minimize distance between lower bound on rd_0 and true one
         fn = function(.B) abs( bound1(pr = pr,
                                       pa = pa,
                                       p1 = p1, 
                                       p0 = p0,  
                                       B = .B,
                                       rd_0 = rd_0) - true ),
         method = "Brent" )$par
}

# more parsimonious version of this fn would be:
# B = (1 / (2*p0*pa) ) * ( sqrt( (alpha + gamma)^2 + 4*p1*p0*pa*(1-pa) ) - alpha - gamma )
# evalue = g(B)



# transform bias factor to E-value
#g_trans = function(RR) RR + sqrt(RR^2 - RR)

# internal fn from R package EValue, for use customizing sens_plot
g_trans = Vectorize( function(x) {
  # define transformation in a way that is monotonic over the effective range of B (>1)
  # to avoid ggplot errors in sens_plot
  # helper function for confounded_meta
  if ( is.na(x) ) return(NA)
  if (x < 1) return( x / 1e10 )
  x + sqrt( x^2 - x )
} )


# if we assume rd_0 >= 0
bound2 = function(pa, p1, p0, B) {
  pr*(p1 - p0*B)*(pa + (1-pa)/B)
}


# only used for sanity checks but not for bound fns above
get_alpha = function(pr, rd_0, true = 0){
  (1 - 1/pr)*rd_0 + true/pr
}

get_gamma = function(pa, p0, p1){
  p0*(1-pa) - p1*pa
}


# MISC HELPERS FOR THIS PROJECT -----------------------------------------------------------

prelims = function() {
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(data.table)
  library(stringr)
  library(tidyverse)
  library(fastDummies)
  library(here)
  library(xtable)
  library(testthat)
  library(plotly)
  library(experiment)
  library(testthat)
  library(EValue)
  
  overleaf.dir <<- "/Users/mmathur/Dropbox/Apps/Overleaf/Sensitivity analysis for NMAR missing data (Overleaf)/results_from_R"
  results.dir <<- here("Results from R")
}




# GENERIC HELPERS  -----------------------------------------------------------

# read/write intermediate work
write_interm = function(x, filename){
  setwd(prepped.data.dir)
  #setwd("Intermediate work")
  write.csv(x, filename)
}

read_interm = function(filename){
  setwd(prepped.data.dir)
  #setwd("Intermediate work")
  read.csv(filename)
}

# like View(), but opens the extra tab if global var useView = TRUE
View2 = function(x){
  if ( useView == TRUE ) View(x) 
}

# quick length(unique) equivalent
uni = function(x){
  length(unique(x))
}

# quick mean with NAs removed
meanNA = function(x){
  mean(x, na.rm = TRUE)
}

# return strings containing anything in pattern vector
stringsWith = function(pattern, x){
  # make regex expression 
  patterns = paste(pattern, collapse="|")
  x[ grepl(pattern = patterns, x = x)]
}
# stringsWith( pattern = c("dog", "cat"),
#  x = c("dogcat", "horse", "cat", "lion") )


# return indices of strings containing anything in pattern vector
whichStrings = function(pattern, x){
  patterns = paste(pattern, collapse="|")
  grepl(pattern = pattern, x = x)
}

# stands for "wipe results"
wr = function(){
  setwd(results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# stands for "view results"
vr = function(){
  setwd(results.dir)
  View( read.csv("stats_for_paper.csv") )
}


# make a string for estimate and CI
stat_CI = function(est, lo, hi){
  paste( est, " [", lo, ", ", hi, "]", sep = "" )
}
# stat_CI( c(.5, -.1), c(.3, -.2), c(.7, .0) )


# return percent true for 0/1 variable, counting NA as own category
percTRUE_incl_NA = function(x) {
  prop.table( table(x, useNA = "ifany") )[2]
}

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
# expects "study" to be a global var
update_result_csv = function( name,
                              .section = NA,
                              value = NA,
                              print = FALSE ) {
  setwd(results.dir)
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(.section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    res.overleaf <<- read.csv( "stats_for_paper.csv",
                               stringsAsFactors = FALSE,
                               colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% res.overleaf$name) ) res.overleaf[ res.overleaf$name %in% name, ] <<- new.rows
    else res.overleaf <<- rbind(res.overleaf, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    res.overleaf <<- new.rows
  }
  
  write.csv( res.overleaf, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  # also write to Overleaf
  setwd(overleaf.dir)
  write.csv( res.overleaf, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(res.overleaf)
  }
}

quick_ci = function( est, var ) {
  c( est - qnorm(.975) * sqrt(var),
     est + qnorm(.975) * sqrt(var) )
}

quick_pval = function( est, var ) {
  2 * ( 1 - pnorm( abs( est / sqrt(var) ) ) )
}


# writes estimate, CI, and pval to results file
# and returns them as a df
#**if using RRs, must pass log-RR, not RR itself
write_est_inf = function( est, var, prefix, takeExp ) {
  
  CIs = quick_ci( est = est, var = var )
  pval = quick_pval( est = est, 
                     var = var)
  
  transf = function(x) x
  if (takeExp == TRUE ) transf = function(x) exp(x)
  
  # write results
  update_result_csv( name = paste( prefix, "est" ),
                     value = round( transf(est), 2) )
  
  update_result_csv( name = paste( prefix, "lo" ),
                     value = round( transf(CIs[1]), 2) )
  
  update_result_csv( name = paste( prefix, "hi" ),
                     value = round( transf(CIs[2]), 2) )
  
  update_result_csv( name = paste( prefix, "pval" ),
                     value = format.pval( pval, eps=0.0001) )
  
  # also return results as (unrounded) df
  res = data.frame( est = transf(est),
                    lo = transf(CIs[1]),
                    hi = transf(CIs[2]),
                    pval = pval )
}


# one or both dirs can be NA
my_ggsave = function(name,
                     .plot = last_plot(),
                     .width,
                     .height,
                     .results.dir = results.dir,
                     .overleaf.dir = overleaf.dir) {
  
  dirs = c(.results.dir, .overleaf.dir)
  dirIsNA = sapply(dirs, is.na)
  validDirs = dirs[ !dirIsNA ]
  
  
  for ( dir in validDirs ) {
    setwd(dir)
    ggsave( name,
            plot = .plot,
            width = .width,
            height = .height,
            device = "pdf" )
  }
}