

# PRELIMINARIES -----------------------------------------------

setwd(here())
source("helper_applied_NMAR.R")
prelims()



# GENERIC RISK DIFFERENCE ----------------------------------

# ~ Plot: rd_0 vs. B needed to explain away ----------------------------------

# Fake example of a risk difference of 0.2
pa = 0.5 
p1 = 0.2  
p0 = 0.1 
pr = 0.25
rd_obs = (p1-p0)


# PLOT ----------------------------------

dp = expand_grid(.pr = c(0.2, 0.5, 0.75),
                 .pa = pa,
                 .p1 = p1,
                 .p0 = p0,
                 .rd_0 = seq(-rd_obs, rd_obs, 0.01) )

dp = dp %>%
  rowwise() %>%
  mutate(B = get_B(pr = .pr,
                   pa = .pa,
                   p1 = .p1, 
                   p0 = .p0,  
                   rd_0 = .rd_0) ) %>%
  mutate(.pr = as.character(.pr) )


colors = c("#1B9E77", "#ff9900", "red")
p = ggplot( data = dp,
            aes(x = .rd_0,
                y = B,
                color = .pr ) ) +
  
  # reference lines
  geom_vline( xintercept = 0,
              lty = 2,
              color = "gray") +
  
  # observed treatment effect
  geom_vline( xintercept = p1-p0,
              lty = 2,
              color = "black") +
  
  geom_line() +
  
  scale_color_manual(values = colors) + 
  
  #ggtitle("Fascinating example for risk difference with p0 = 0.1, p1 = 0.2") +
  
  # base_size controls all text sizes; default is 11
  # https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme_bw(base_size = 20) +
  
  # use all values of
  #scale_x_log10( breaks = unique(.dp$n) )
  # use only some values
  #scale_x_log10( breaks = c(500, 1000) ) +
  
  xlab( bquote( bold( {RD^t}[XY * "|" * R == "0"] ) ) ) +
  scale_x_continuous( breaks = c( seq( min(dp$.rd_0), max(dp$.rd_0), 0.02 ) ) ) +
  
  ylab("Bias factor (B)") +
  scale_y_continuous( breaks = seq(1, 10, 0.5) ) +
  
  guides( color = guide_legend(title = bquote( bold("Retention (") * bold(p[R]) * bold(")") ) ) ) +
  theme_bw() +
  theme( text = element_text(face = "bold"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank() ) 

#@ couldn't get this to work well - try again
# https://bookdown.dongzhuoer.com/hadley/ggplot2-book/direct-labelling.html
# library(directlabels)
# directlabels::geom_dl(aes(label = class), method = "smart.grid")



# ~ Compare to Horowitz & Manski for a given value --------------------------

# look at individual values
bound1(ps = ps,
       pa = pa,
       p1 = p1, 
       p0 = p0, 
       B = 1.1,
       rd_0 = 0)

# Horowitz & Manski, 1998

# need to make a fake dataframe of study data
# does their bound actually require this?
n = 1000
d = data.frame( X = sample(x = c( rep(0, n/2), rep(1, n/2) ),
                           size = n,
                           replace = FALSE) ) %>%
  rowwise() %>%
  mutate(p_Y = ifelse(X == 1, p1, p0),
         Y = rbinom(n = 1,
                    size = 1,
                    prob = p_Y) )

d %>% group_by(X) %>%
  summarise(mean(Y))

d %>% add_row(X = NA, Y=NA, p_Y=NA)

#bm: this is breaking
# debug(ATEbounds) indicates it breaks at line 27, but unclear why
ATEbounds(Y ~ X,
          data = d,
          maxY = 1,
          minY = 0)

# NOT IN USE? GENERAL NONNEGATIVE OUTCOME: BEARNE DATA -----------------------------------------------

# observed d in completer stratum
# Bearne abstract
ps = 148/190  # abstract; 78% retention and CC analysis
pa = 0.5 # from flowchart figure
( p1 = 380.6 - 352.9 )  # pre/post hange in walk time for treatment group
( p0 = 372.1 - 369.8 )  # change in walk time for treatment group
# this is NOT the estimate that adjusts for covariates, though

( rd_obs = p1 - p0 )


# if deltas are perfectly balanced, no bias, and 50% dropout
expect_equal( 0, bound1(ps = 0.5,
                        pa = 0.5,
                        p1 = p1,  
                        p0 = p0, 
                        B = 1,
                        rd_0 = -rd_obs) )

# regular E-value case: ps=1
# Ding Supplement, page 30
bound1(ps = 1,
       pa = pa,
       p1 = p1,  
       p0 = p0, 
       B = 2,
       rd_0 = 0)
(p1 - p0*2)*(pa + (1-pa)/2)

# solve for B required to explain away if rd_0 = 0
#bm: think about why this doesn't depend on ps
#  which is confirmed by the form of bound2()
( B = get_B(ps = ps,
            pa = pa,
            p1 = p1, 
            p0 = p0,  
            rd_0 = 0) )

# convert to E-value of sorts, but note that MR_UY is on the mean ratio scale
( Eval = g_trans(B) )

#  mean ratios of U on D with and without exposure, and MRUD = max(MRUD|E=1,MRUD=0) as the maximum of these two mean ratios.


# sanity check: yes, this gets it close to 0
bound1(ps = ps,
       pa = pa,
       p1 = p1, 
       p0 = p0,  
       B = 12,
       rd_0 = 0) 




# # play with other parameters
# ps = 0.67
# pa = 0.5
# p1 = 50
# p0 = 20
# p1 - p0
# 
# 
# get_B(ps = ps,
#       pa = pa,
#       p1 = p1, 
#       p0 = p0,  
#       rd_0 = 0)
# 
# 
# # convert to E-value, but note that ONE parameter is on mean ratio scale
# #  while the other is on RR scale
# # that's an important limitation of doing this for ATE rather than risk difference
# ( Eval = B + sqrt(B^2 - 1) )


# ~ Plot: rd_0 vs. B needed to explain away ----------------------------------


# for Bearne example
dp = expand_grid(.ps = ps,
                 .pa = pa,
                 .p1 = p1,
                 .p0 = p0,
                 .rd_0 = seq(-25, 25, 0.01) )

dp = dp %>% rowwise() %>%
  mutate(B = get_B(ps = .ps,
                   pa = .pa,
                   p1 = .p1, 
                   p0 = .p0,  
                   rd_0 = .rd_0) )

# interesting! almost exactly linear 
ggplot( data = dp,
        aes(x = .rd_0,
            y = B) ) +
  geom_line() 




