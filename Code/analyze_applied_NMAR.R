
rm(list=ls())

#@USE RENV AT THE END!
# Also wipe and re-run again because you have some nonsense entries in stats_for_paper.csv

# PRELIMINARIES -----------------------------------------------

library(here)
setwd(here("Code"))
source("helper_applied_NMAR.R")

# load packages, etc.
prelims()


# MAIN-TEXT EXAMPLE: SMOKING CESSATION (VOLPP) ----------------------------------

# ~ Enter data ----------------------------------
# see annotated paper on OSF for notes on the origins of these numbers

n.randomized.trt = 436
n.randomized.cntrl = 442

# at 3 or 6 mos 
n.dropout.trt = 66
n.dropout.cntrl = 59

# retention by arm
n1.retained = n.randomized.trt - n.dropout.trt
n1.retained / n.randomized.trt

n0.retained = n.randomized.cntrl - n.dropout.cntrl
n0.retained / n.randomized.cntrl

# overall retention at 3 or 6 mos
( pr = (n0.retained + n1.retained) / (n.randomized.trt + n.randomized.cntrl) )

( pa = n1.retained / (n1.retained + n0.retained) )

# Table 2 for confirmed cessation at 3 or 6 mos (as reported in Abstract)
( p1 = 91/n1.retained )   
# vs. their calculation, which essentially assumes anyone who dropped
#  out of either arm did NOT cease smoking:
expect_equal( 0.209, round( 91/n.randomized.trt, 3 ) )


( p0 = 52/n0.retained ) 
# vs. their calculation:
expect_equal( 0.118, round( 52/n.randomized.cntrl, 3 ) )




# ~ Observed RD --------------------------

# observed RD
( rd_obs = (p1-p0) )

# observed CI
rd_obs_var = ( ( p1 * (1 - p1) ) / n1.retained ) + ( ( p0 * (1 - p0) ) / n0.retained )

( rd_obs_CI = rd_obs + c(-1,1)*qnorm(0.975) * sqrt(rd_obs_var) )

# from body of EValue::evalues.RD: lowerCI = 0.054477
expect_equal( rd_obs_CI[1], 0.054477 )


# ~ Write stats: Descriptives and unadjusted stats --------------------------

analysis = "Volpp"


### write results
update_result_csv( name = paste( analysis, "Retention rate" ),
                   value = round(100* (n0.retained+n1.retained)/(n.randomized.cntrl+n.randomized.trt) ) )

update_result_csv( name = paste( analysis, "Retention rate control" ),
                   value = round(100*n0.retained/n.randomized.cntrl) )

update_result_csv( name = paste( analysis, "Retention rate trt" ),
                   value = round(100*n1.retained/n.randomized.trt) )

update_result_csv( name = paste( analysis, "pa" ),
                   value = round(pa,2) )

update_result_csv( name = paste( analysis, "p0" ),
                   value = round(p0,2) )

update_result_csv( name = paste( analysis, "p1" ),
                   value = round(p1,2) )

update_result_csv( name = paste( analysis, "rd_obs" ),
                   value = round(rd_obs,2) )

update_result_csv( name = paste( analysis, "rd_obs lo" ),
                   value = round(rd_obs_CI[1], 2) )

update_result_csv( name = paste( analysis, "rd_obs hi" ),
                   value = round(rd_obs_CI[2], 2) )

update_result_csv( name = paste( analysis, "Authors' rd_obs" ),
                   value = round(0.209 - 0.118,2) )



# ~ E-values --------------------------

# get E-value for each of several values of the sens parameter RD_0
rd_0_vec = c(0, -0.10)

for ( .rd_0 in rd_0_vec ) {

  alpha = get_alpha(pr = pr,
                    rd_0 = .rd_0,
                    true = 0)
  
  evalue0 = evalues.RD( n11 = n1.retained*p1,
                        n10 = n1.retained*(1-p1),
                        n01 = n0.retained*p0,
                        n00 = n0.retained*(1-p0),
                        #*note that equivalence arises when we set true = alpha:
                        true = alpha )
  
  update_result_csv( name = paste( analysis, " Evalue est rd_0=", round(.rd_0, 2), sep = "" ),
                     value = round(evalue0$est.Evalue, 2) )
  
  update_result_csv( name = paste( analysis, " Evalue lo rd_0=", round(.rd_0, 2), sep = "" ),
                     value = round(evalue0$lower.Evalue, 2) )
  
}





# ~ Plots: Sensitivity parameters vs. E-value -----------------------------------



colors = rev( c("#1B9E77", "#ff9900", "red") )

# ~~ (Main text) Figure 1: Shopeifting to null vs. to other values ------------------------

# add two hypothetical lower amounts of retention
dp = expand_grid(.pr = pr,
                 .pa = pa,
                 .p1 = p1,
                 .p0 = p0,
                 .rd_0 = seq(-rd_obs, rd_obs, 0.001),
                 .true = c(-0.05, 0, 0.05) )

dp = dp %>%
  rowwise() %>%
  mutate(B = get_B(pr = .pr,
                   pa = .pa,
                   p1 = .p1, 
                   p0 = .p0,  
                   rd_0 = .rd_0,
                   true = .true) )


( breaks.y1 = seq(1.2, 2.6, .2) )
( breaks.y2 = round( g_trans(breaks.y1), 2 ) )


plt0 = ggplot( data = dp,
               aes(x = .rd_0,
                   y = B,
                   color = as.factor(.true) ) ) +
  
  
  # reference lines
  geom_vline( xintercept = 0,
              lty = 2,
              color = "gray") +
  
  # observed treatment effect
  geom_vline( xintercept = p1-p0,
              lty = 2,
              color = "black") +
  
  geom_line() +
  
  # base_size controls all text sizes; default is 11
  # https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_bw(base_size = 20) +
  
  scale_color_manual(values = colors) +
  
  xlab( bquote( bold("Sensitivity parameter") ~ bold( {RD}[AY * "|" * R == "0"] ) ) ) +
  
  scale_x_continuous( breaks = seq( -0.10, 0.10, 0.02 ) ) +

  ylab("Bounding factor (B) among retained participants") +
  coord_cartesian( ylim = c( min(breaks.y1), max(breaks.y1) ) ) +
  scale_y_continuous( breaks = breaks.y1,
                      sec.axis = sec_axis( ~g_trans(.),
                                           name = "M-value",
                                           breaks = breaks.y2),
                      trans = "log10" ) +
  
  
  guides( color = guide_legend(title = bquote( bold( {RD^p}[AY]) ) ) ) +
  theme_bw() +
  theme( text = element_text(face = "bold"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank() ) 

plt0

my_ggsave(name = "plot0_rd0_vs_B_vary_true.pdf",
          .plot = plt0,
          .width = 8,
          .height = 4.5)



# ~~ (Not shown) Extra figure 2: Show other levels of retention ------------------------

# add two hypothetical lower amounts of retention
dp = expand_grid(.pr = c(0.2, 0.5, pr),
                 .pa = pa,
                 .p1 = p1,
                 .p0 = p0,
                 .rd_0 = seq(-rd_obs, rd_obs, 0.001) )

dp = dp %>%
  rowwise() %>%
  mutate(B = get_B(pr = .pr,
                   pa = .pa,
                   p1 = .p1, 
                   p0 = .p0,  
                   rd_0 = .rd_0) )  %>%
  mutate(.pr = as.character( round(.pr,2) ) )


( breaks.y1 = seq(1.4, 2, .1) )
( breaks.y2 = round( g_trans(breaks.y1), 2 ) )


plt1 = ggplot( data = dp,
               aes(x = .rd_0,
                   y = B,
                   color = .pr) ) +
  
  
  # reference lines
  geom_vline( xintercept = 0,
              lty = 2,
              color = "gray") +
  
  # observed treatment effect
  geom_vline( xintercept = p1-p0,
              lty = 2,
              color = "black") +
  
  geom_line() +
  
  # base_size controls all text sizes; default is 11
  # https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_bw(base_size = 20) +
  
  scale_color_manual(values = colors) +
  
  xlab( bquote( bold( {RD^t}[XY * "|" * R == "0"] ) ) ) +
  
  scale_x_continuous( breaks = seq( -0.10, 0.10, 0.02 ) ) +
  
  ylab("Bias factor (B)") +
  coord_cartesian( ylim = c( min(breaks.y1), max(breaks.y1) ) ) +
  scale_y_continuous( breaks = breaks.y1,
                      sec.axis = sec_axis( ~g_trans(.),
                                           name = "Minimum strength of both confounding RRs",
                                           breaks = breaks.y2),
                      trans = "log10" ) +
  
  
  guides( color = guide_legend(title = bquote( bold("Retention (") * bold(p[R]) * bold(")") ) ) ) +
  theme_bw() +
  theme( text = element_text(face = "bold"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank() ) 

plt1



# ~~ (Not shown) Extra figure 3: Simplified; show other the actual retention ------------------------

dp2 = dp %>% filter(.pr == "0.86")


( breaks.y1 = seq(1.6, 2, .1) )
( breaks.y2 = round( g_trans(breaks.y1), 2 ) )


plt2 = ggplot( data = dp2,
               aes(x = .rd_0,
                   y = B) ) +
  
  
  # reference lines
  geom_vline( xintercept = 0,
              lty = 2,
              color = "gray") +
  
  # observed treatment effect
  geom_vline( xintercept = p1-p0,
              lty = 2,
              color = "black") +
  
  geom_line() +
  
  # base_size controls all text sizes; default is 11
  # https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_bw(base_size = 20) +

  
  xlab( bquote( bold( {RD^t}[XY * "|" * R == "0"] ) ) ) +
  
  scale_x_continuous( breaks = seq( -0.10, 0.10, 0.02 ) ) +
  
  ylab("Bias factor (B)") +
  coord_cartesian( ylim = c( min(breaks.y1), max(breaks.y1) ) ) +
  scale_y_continuous( breaks = breaks.y1,
                      sec.axis = sec_axis( ~g_trans(.),
                                           name = "Minimum strength of both confounding RRs",
                                           breaks = breaks.y2 ),
                      trans = "log10" ) +
  
  
  guides( color = guide_legend(title = bquote( bold("Retention (") * bold(p[R]) * bold(")") ) ) ) +
  theme_bw() +
  theme( text = element_text(face = "bold"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank() ) 

plt2



# ~ Theory sanity checks ------------------

# ~~ get_B should agree with Eq 4.1 in main text --------------------------
( B = get_B(pr = pr,
            pa = pa,
            p1 = p1,
            p0 = p0,
            rd_0 = 0.08) )

alpha = get_alpha(pr = pr,
                  rd_0 = 0.08,
                  true = 0)

gamma = get_gamma(pa = pa, 
                  p1 = p1,
                  p0 = p0)


B_check = (1 / (2*p0*pa) ) * ( sqrt( (alpha + gamma)^2 + 4*p1*p0*pa*(1-pa) ) - alpha - gamma )
expect_equal(B, B_check)



# ~~ get_B should agree with EValue package --------------------------

evalue_check = evalues.RD( n11 = n1.retained*p1,
                           n10 = n1.retained*(1-p1),
                           n01 = n0.retained*p0,
                           n00 = n0.retained*(1-p0),
                           #*note that equivalence arises when we set true = alpha:
                           true = alpha )

# oh yassss :)
expect_equal(evalue_check$est.Evalue,
             g_trans(B))






# SUPP EXAMPLE: DRUG ABUSE & HOMELESSNESS (THOMPSON) ----------------------------------

# ~ Enter data ----------------------------------

# https://ajph.aphapublications.org/doi/pdf/10.2105/AJPH.2013.301302?casa_token=qP-MLjDA1aIAAAAA:TJmp9AeQnLhrBF4J-jBGquUUsUsvwYDLTVtsyfnwd9_eijBElfYGLXss0WTemP8UyQuYcn94QFk

# focus on alcohol use disorder among ALL PARTICIPANTS
# alcohol use --> R (not having impairment, etc., which were formal eligibility criteria) --> homelessness

# proportions with and without alcohol use disorder at baseline
# top of "Results" text
# they don't report drug use prevalence among all wave-1 subjects, so assume
#  it's the same as in retained subjects
pa = 0.064
# n.randomized.trt = round(n.randomized * 0.064)
# n.randomized.cntrl = n.randomized - n.randomized.trt

n.retained = 30558

( n1.retained = round( n.retained * pa ) )
( n0.retained = round( n.retained * (1-pa) ) )
expect_equal( n.retained, n1.retained + n0.retained )

# sanity check
#  c.f. reported response rate ("Methods"):
n.randomized = 43093
pr = n.retained/n.randomized
pr.reported = 0.702
expect_equal( pr, pr.reported, tol = 0.01 )

# outcome probabilities
# Table 1
p1 = 0.126
p0 = 0.064



# ANALYZE ----------------------------------

# ~ Observed RD --------------------------

# observed RD
( rd_obs = (p1-p0) )

# observed CI
rd_obs_var = ( ( p1 * (1 - p1) ) / n1.retained ) + ( ( p0 * (1 - p0) ) / n0.retained )

( rd_obs_CI = rd_obs + c(-1,1)*qnorm(0.975) * sqrt(rd_obs_var) )


# ~ Write stats: Descriptives and unadjusted stats --------------------------

analysis = "Thompson"


### write results
update_result_csv( name = paste( analysis, "pa" ),
                   value = round(pa,2) )

update_result_csv( name = paste( analysis, "pr" ),
                   value = 100*round(pr,3) )

update_result_csv( name = paste( analysis, "Authors' pr" ),
                   value = 100*round(pr.reported,3) )

update_result_csv( name = paste( analysis, "p0" ),
                   value = round(p0,2) )

update_result_csv( name = paste( analysis, "p1" ),
                   value = round(p1,2) )

update_result_csv( name = paste( analysis, "rd_obs" ),
                   value = round(rd_obs,2) )

update_result_csv( name = paste( analysis, "rd_obs lo" ),
                   value = round(rd_obs_CI[1], 2) )

update_result_csv( name = paste( analysis, "rd_obs hi" ),
                   value = round(rd_obs_CI[2], 2) )




# ~ E-values --------------------------

# get E-value for each of several values of the sens parameter RD_0
rd_0_vec = c(0, -0.10)

for ( .rd_0 in rd_0_vec ) {
  
  alpha = get_alpha(pr = pr,
                    rd_0 = .rd_0,
                    true = 0)
  
  evalue0 = evalues.RD( n11 = n1.retained*p1,
                        n10 = n1.retained*(1-p1),
                        n01 = n0.retained*p0,
                        n00 = n0.retained*(1-p0),
                        #*note that equivalence arises when we set true = alpha:
                        true = alpha )
  
  update_result_csv( name = paste( analysis, " Evalue est rd_0=", round(.rd_0, 2), sep = "" ),
                     value = round(evalue0$est.Evalue, 2) )
  
  update_result_csv( name = paste( analysis, " Evalue lo rd_0=", round(.rd_0, 2), sep = "" ),
                     value = round(evalue0$lower.Evalue, 2) )
  
}


