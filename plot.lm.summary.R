coef(lm(formula = moments_metab.tsh20_otus$kurtosis ~ poly(moments_metab.tsh20_otus$skewness, 2, raw=T)))
curve(-2.3894249 +  0.9830116*x^2 + 0.9467821*x,1,8, add=T, col="blue")

modello <-lm(formula = moments_metab.tsh20_otus$kurtosis ~ poly(moments_metab.tsh20_otus$skewness, 2, raw=T))
broom::glance(modello)$r.squared
broom::glance(modello)$p.value 

moments_metab.tsh20_otus %>%
  ggplot() + 
  aes(x=skewness, y=kurtosis) + 
  #geom_point(col= "gray", size = 0.5) +
  geom_point(aes(col=n_stations), size = 0.5) +
  scale_colour_viridis_c() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
  theme_bw() +
  ggtitle(paste0("Skewness and Kurtosis; ",
                 n_distinct(moments_metab.tsh20_otus$OTU), " OTUs"),
          subtitle = ("metaB, diatoms, 20-180, SUR")) +
  geom_label(aes(x = 0, y = 60), hjust = 0,
             label = paste("Eq.:", signif(modello$coef[[1]],5 ), 
                           "x^2 +", signif(modello$coef[[2]], 5),
                           "x + ", signif(modello$coef[[3]], 5),
                           "\nAdj R2 = ", signif(summary(modello)$adj.r.squared, 5),
                           " \nP =",signif(broom::glance(modello)$p.value)))



#######################################


lm.metab.meanvar <-lm(formula = log10(moments_metab.tsh20_otus$variance) ~ log10(moments_metab.tsh20_otus$mean))

paste("y = ", signif(lm.metab.meanvar$coef[[1]],5 ), 
      "x +", signif(lm.metab.meanvar$coef[[2]], 5),
      "\nAdj R2 = ", signif(summary(lm.metab.meanvar)$adj.r.squared, 5),
      " \nP =",signif(broom::glance(lm.metab.meanvar)$p.value))



moments_metag.tsh20 %>%
  ggplot() + 
  aes(x=mean, y=variance) + 
  geom_point(aes(col=n_stations), size = 0.5) +
  scale_colour_viridis_c() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle(paste0("Mean and Variance; ",
                 n_distinct(moments_metag.tsh20$geneid), " unigenes"),
          subtitle = ("metaG, diatoms, 20-180, SUR")) +
  geom_label(aes(x = 0.001, y = 1e-02), hjust = 0,
             label = paste("y = ", signif(lm.metag.meanvar$coef[[1]],5), 
                           "x +", signif(lm.metag.meanvar$coef[[2]], 5),
                           "\nAdj R2 = ", signif(summary(lm.metag.meanvar)$adj.r.squared, 5),
                           " \nP =",broom::glance(lm.metag.meanvar)$p.value))

glance(lm.metag.meanvar)$p.value
summary(lm.metag.meanvar)

signif(summary(lm.metag.meanvar)$coefficients[1,"Pr(>|t|)"])
coef(summary(lm.metag.meanvar))[, "Pr(>|t|)"]
