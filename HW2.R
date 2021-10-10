x = seq(0, 25, length.out = 1000)
plot(x, dgamma(x, shape = 5.4489, scale = 1.077325))
#messing around -- this shape and scale is similar to normal distribution :)
#Very cool


library(e1071)

set.seed(1) #set seed to ensure re-testing

n_10 = rgamma(10, shape = 1, scale =2)
n_25 = rgamma(25, shape = 1, scale =2)
n_100 = rgamma(100, shape =1, scale =2)
n_1000 = rgamma(1000, shape =1, scale =2)
#making random samples)

mean(n_10)
mean(n_25)
mean(n_100)
mean(n_1000)

sd(n_10)
sd(n_25)
sd(n_100)
sd(n_1000)

skewness(n_10)
skewness(n_25)
skewness(n_100)
skewness(n_1000)

kurtosis(n_10)
kurtosis(n_25)
kurtosis(n_100)
kurtosis(n_1000)

library(nleqslv)
library(ggplot2)
library(patchwork)
gamma.MOM<-function(par, data){
  #borrowed from Professor Cipolli
  shape <- par[1]
  scale <- par[2]
  
  EX1 <- scale*shape
  EX2 <- (scale^2)*(shape * (shape +1))
  #moments found online. Reminder to self: cite.
  
  xbar1 <- mean(data)
  xbar2 <- mean(data^2)
  
  c(EX1-xbar1, EX2-xbar2)
}

gamma.LL<-function(par, data, neg=T){
  #Also borrowed from Professor Cipolli
  shape <- par[1]
  scale <- par[2]
  
  ll <- sum(dgamma(x=data, shape = shape, scale = scale, log =T))
  ifelse(neg, -ll, ll)
}

find.MOM.MLE = function(n, par){
  Density = rgamma(n, shape = par[1], scale =par[2])
  MOM = nleqslv(x = c(1,1),
                fn = gamma.MOM, 
                data = Density)
  MLE = optim(par = c(1,1),
              fn = gamma.LL,
              data = Density)
  #finds MOM and MLE for random data
  
  print(c(MOM$x, MLE$par))
  #print for testing
  
  
  #using Freedman-Diaconis rule which is found to be robust for histograms
  #reminder to self: cite
  estimated.MOM = rgamma(n, shape = MOM$x[1], scale = MOM$x[2])
  bw.MOM = 2 * IQR(estimated.MOM, na.rm =T) /
    length(na.omit(estimated.MOM))^(1/3)
  
  true.dist = dgamma(seq(0,10, 0.1), shape = par[1],
                     scale = par[2])
  
  plot_MOM = ggplot() +
       geom_histogram(aes(x= estimated.MOM,
                          y= ..density..),
                      binwidth = bw.MOM,
                      color = "mediumpurple", fill ="white")+
       theme_minimal()+ ylab("Density")+xlab("Observations")+
       geom_hline(yintercept = 0)+
    geom_line(aes(x= seq(0,10, 0.1),y = true.dist),
              color ="forestgreen", lwd = 0.7)+
    ggtitle(paste("Methods of Moments Estimator n =", n, sep = " "))+
    theme(plot.title = element_text(hjust = 0.5, size = 9))+
    xlim(c(0,10))+ylim(c(0, max(true.dist)))
  
  plot_MOM = plot_MOM +
    annotate("text", x = 8, y = 0.9*max(true.dist),
             label = paste("Shape =", round(MOM$x[1],3)))+
    annotate("text", x = 8, y = 0.85*max(true.dist),
             label = paste("Scale =", round(MOM$x[2],3)))
  
  estimated.MLE = rgamma(n, shape = MLE$par[1], scale = MLE$par[2])
  bw.MLE = 2 * IQR(estimated.MLE, na.rm = T) /
    length(na.omit(estimated.MLE))^(1/3)
  plot_MLE = ggplot() +
    geom_histogram(aes(x= estimated.MLE,
                       y= ..density..),
                   binwidth = bw.MLE,
                   color = "mediumpurple", fill ="white")+
    theme_minimal()+ ylab("Density")+xlab("Observations")+
    geom_hline(yintercept = 0)+
    geom_line(aes(x= seq(0,10, 0.1),y = true.dist),
              color ="forestgreen", lwd = 0.7)+
    ggtitle(paste("Maximum Likelihood Estimator n =", n, sep=" ")) +
    theme(plot.title = element_text(hjust = 0.5, size = 9))+
    xlim(c(0,10)) +ylim(c(0, max(true.dist)))
  
  plot_MLE = plot_MLE+
    annotate("text", x = 8, y = 0.9*max(true.dist),
             label = paste("Shape =", round(MLE$par[1],3)))+
    annotate("text", x = 8, y = 0.85*max(true.dist),
             label = paste("Scale =", round(MLE$par[2],3)))
  
  (plot_MOM + plot_MLE)
}

#Other random code I was working on. The following allows you to superimpose
#all the distributions onto one plot. Feel free to use any of it!

# colors = c("Methods of Moments" = "forestgreen",
#            "Maximum Likelihood Estimator" = "red3")
# linetypes = c("Methods of Moments" = 1,
#               "Maximum Likelihood Estimator" = 2)
# plot = ggplot() +
#   geom_histogram(aes(x= Density, y= stat(count)/sum(count)),
#                  binwidth = 2 * IQR(Density) / length(Density)^(1/3),
#                  color = "mediumpurple", fill ="white")+
#   theme_minimal()+ ylab("Relative Frequency")+xlab("Observations")+
#   geom_hline(yintercept = 0)+
#   geom_density(mapping = aes(x =rgamma(n, shape = MOM$x[1], scale = MOM$x[2]),
#                              color = "Methods of Moments",
#                              linetype = "Methods of Moments"), 
#                lwd = 1) +
#   geom_density(aes(x = rgamma(n, shape = MLE$par[1], MLE$par[2]),
#                    color = "Maximum Likelihood Estimator",
#                    linetype = "Maximum Likelihood Estimator"),
#                lwd = 1)+
#   labs(color = 'Type of Estimator', linetype = 'Type of Estimator')+
#   scale_color_manual(values = colors ) +
#   scale_linetype_manual(values = linetypes,guide =guide_legend(
#     override.aes = list(linetype = c(1,2), shape = c(NA,NA))))+
#   theme(legend.position = c(0.5, 0.8))
# 
# plot
# 
#ks.MLE = ks.test(density(estimated.MLE)$y,
true.dist, alternative = "two.sided")

plot_MLE = plot_MLE+
  annotate("text", x = 8, y = 0.8*max(true.dist),
           label = paste("p = ", round(ks.MLE[[2]], 3)))

ks.MOM = ks.test(density(estimated.MOM)$y, true.dist, alternative = "two.sided")
plot_MOM = plot_MOM +
  annotate("text", x = 8, y = 0.8*max(true.dist),
           label = paste("p =", round(ks.MOM[[2]],3)))