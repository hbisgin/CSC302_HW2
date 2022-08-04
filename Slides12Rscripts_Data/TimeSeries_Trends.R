library(tidyverse)
library(ggplot2)
install.packages('ggridges')
library(ggridges)
library(lubridate)
library(ggrepel)
library(colorspace)

#put your folder's path inside quotes below
folder_location='...'

setwd(folder_location)

#source('/Volumes/GoogleDrive/My Drive/TEACHINGDrive/CSC302/R/themes.R')
#source('/Volumes/GoogleDrive/My Drive/TEACHINGDrive/CSC302/R/plot_grid.R')
#source('/Volumes/GoogleDrive/My Drive/TEACHINGDrive/CSC302/R/moving_average.R')

source('./themes.R')
source('./plot_grid.R')
source('./moving_average.R')

#1

load("./preprint_growth.rda")
#load("/Volumes/GoogleDrive/My Drive/DATA/preprint_growth.rda")

head(preprint_growth)

preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth


ggplot(biorxiv_growth, aes(date, count)) + 
  geom_point(color = "white", fill = "#0072B2", shape = 21, size = 2) +
  scale_y_continuous(limits = c(0, 1600), expand = c(0, 0),
                     name = "preprints / month") + 
  scale_x_date(name = "year") +
  theme(plot.margin = margin(7, 7, 3, 1.5))

#2 
ggplot(biorxiv_growth, aes(date, count)) +
  geom_line(size = 0.5, color = "#0072B2") + 
  geom_point(color = "white", fill = "#0072B2", shape = 21, size = 2) +
  scale_y_continuous(limits = c(0, 1600), expand = c(0, 0),
                     name = "preprints / month") + 
  scale_x_date(name = "year") +
  #theme_dviz_open() +
  theme(plot.margin = margin(7, 7, 3, 1.5))

################################################################################################################
#3
ggplot(biorxiv_growth, aes(date, count)) + geom_line(color = "#0072B2", size = 1) +
  scale_y_continuous(limits = c(0, 1600), expand = c(0, 0),
                     name = "preprints / month") + 
  scale_x_date(name = "year") +
  #theme_dviz_open() +
  theme(plot.margin = margin(7, 7, 3, 1.5))

#4
#option 1
ggplot(biorxiv_growth, aes(x=date, y=count)) + geom_area(color='#69b3a2', fill='#0072B240', alpha=0.4)

#option 2: with geom_ridgelines, requires library(ggridges) 
ggplot(biorxiv_growth, aes(date, height = count, y = 0)) + 
  geom_ridgeline(color = "#0072B2", fill = "#0072B240", size = 0.75) +
  scale_y_continuous(limits = c(0, 1600), expand = c(0, 0),
                     name = "preprints / month") + 
  scale_x_date(name = "year") +
  #theme_dviz_open() +
  theme(plot.margin = margin(7, 7, 3, 1.5))
################################################################################################################

#5 when you have multiple time series:

#bad one first:

preprint_growth %>% filter(archive %in% c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%
  filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))-> preprints

ggplot(preprints, aes(date, count, color = archive, fill = archive, shape = archive)) + 
  geom_point(color = "white", size = 2) +
  scale_shape_manual(values = c(21, 22, 23),
                     name = NULL) + 
  scale_y_continuous(limits = c(0, 600), expand = c(0, 0),
                     name = "preprints / month") + 
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  scale_fill_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                    name = NULL) +
  #theme_dviz_open() +
  theme(legend.title.align = 0.5,
        legend.position = c(0.1, .9),
        legend.just = c(0, 1),
        plot.margin = margin(14, 7, 3, 1.5))
################################################################################################################
#better one:

ggplot(preprints, aes(date, count, color = archive, fill = archive, shape = archive)) + 
  geom_line(size = 0.75) + 
  geom_point(color = "white", size = 2) +
  scale_y_continuous(limits = c(0, 600), expand = c(0, 0),
                     name = "preprints / month") + 
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  scale_fill_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                    name = NULL) +
  scale_shape_manual(values = c(21, 22, 23), #notice that we're assigning shapes to each series here
                     name = NULL) + 
  #theme_dviz_open() +
  theme(legend.title.align = 0.5,
        legend.position = c(0.1, .9),
        legend.just = c(0, 1),
        plot.margin = margin(14, 7, 3, 1.5))
################################################################################################################
#much better one with dots removed
preprints_final <- filter(preprints, date == ymd("2017-01-01"))
ggplot(preprints) +
  aes(date, count, color = archive, fill = archive, shape = archive) + 
  geom_line(size = 1) + 
  #geom_point(color = "white", size = 2) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis(
      breaks = preprints_final$count,
      labels = c("arXiv\nq-bio", "PeerJ\nPreprints", "bioRxiv"),
      name = NULL)
  ) + 
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01")),
               expand = expand_scale(mult = c(0.02, 0))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  scale_fill_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                    name = NULL) +
  scale_shape_manual(values = c(21, 22, 23),
                     name = NULL) + 
  coord_cartesian(clip = "off") +
  #theme_dviz_open() +
  theme(legend.position = "none") +
  theme(axis.line.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = margin(14, 7, 3, 1.5))
################################################################################################################
#Dose Response instead of time
MASS::oats %>% 
  # 1 long (UK) cwt == 112 lbs == 50.802345 kg
  mutate(N = 1*as.numeric(sub("cwt", "", N, fixed = TRUE))) %>%
  group_by(N, V) %>%
  summarize(mean = 20 * mean(Y)) %>% # factor 20 converts units to lbs/acre
  mutate(variety = ifelse(V == "Golden.rain", "Golden Rain", as.character(V))) ->
  oats_df

  head(oats_df)
  
  #turning variety values to factors
  oats_df$variety <- factor(oats_df$variety, levels = c("Marvellous", "Golden Rain", "Victory"))
  
  ggplot(oats_df,
         aes(N, mean, color = variety, shape = variety, fill = variety)) +
    geom_line(size = 0.75) + geom_point(color = "white", size = 2.5) +
    scale_y_continuous(name = "mean yield (lbs/acre)") + 
    scale_x_continuous(name = "manure treatment (cwt/acre)") +
    scale_shape_manual(values = c(21, 22, 23),
                       name = "oat variety") + 
    scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                       name = "oat variety") +
    scale_fill_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                      name = "oat variety") +
    coord_cartesian(clip = "off") +
    #theme_dviz_open() +
    theme(legend.title.align = 0.5)
  ################################################################################################################
  #House prices
  #load('/Volumes/GoogleDrive/My Drive/DATA/house_prices.rda')
  load('./house_prices.rda')
  
  #preparing the data. 
  CA_house_prices <- 
    filter(house_prices, state == "California", year(date) > 2000) %>%
    mutate(
      label = ifelse(
        date %in% c(ymd("2005-01-01"), ymd("2007-07-01"), 
                    ymd("2010-01-01"), ymd("2012-07-01"), ymd("2015-01-01")),
        format(date, "%b %Y"), ""),
      nudge_x = case_when(
        label == "Jan 2005" ~ -0.003,
        TRUE ~ 0.003
      ),
      nudge_y = case_when(
        label == "Jan 2005" ~ 0.01,
        label %in% c("Jul 2007", "Jul 2012") ~ 0.01,
        TRUE ~ -0.01
      ),
      hjust = case_when(
        label == "Jan 2005" ~ 1,
        TRUE ~ 0
      )
    )
  
  # 
  ggplot(CA_house_prices, aes(date, house_price_perc)) +
  geom_line(size = 1, color = "#0072b2") +
    scale_y_continuous(
      limits = c(-0.3, .32), expand = c(0, 0),
      breaks = c(-.3, -.15, 0, .15, .3),
      name = "12-month change\nin house prices", labels = scales::percent_format(accuracy = 1)
    ) + 
    scale_x_date(name = "", expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    #theme_dviz_grid() +
    theme(
      axis.line = element_blank(),
      plot.margin = margin(12, 1.5, 0, 1.5)
    )
  
  ggplot(CA_house_prices, aes(date, unemploy_perc/100)) +
    geom_line(size = 1, color = "#0072b2") +
    scale_y_continuous(
      limits = c(0.037, 0.143),
      name = "unemployment\nrate", labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0)
    ) +
    scale_x_date(name = "year", expand = c(0, 0)) +
    #theme_dviz_grid() +
    theme(
      axis.line = element_blank(),
      plot.margin = margin(6, 1.5, 3, 1.5)
    )

 
#connected plot by using geom_path.
  #library(ggrepel)
  
  ggplot(CA_house_prices) +
    aes(unemploy_perc/100, house_price_perc, colour = as.numeric(date)) + 
    geom_path(size = 1, lineend = "round") +
    geom_text_repel(
      aes(label = label), point.padding = .2, color = "black",
      min.segment.length = 0, size = 12/.pt,
      hjust = CA_house_prices$hjust,
      nudge_x = CA_house_prices$nudge_x,
      nudge_y = CA_house_prices$nudge_y,
      direction = "y",
      #family = dviz_font_family
    ) +
    scale_x_continuous(
      limits = c(0.037, 0.143),
      name = "unemployment rate", labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(-0.315, .315), expand = c(0, 0),
      breaks = c(-.3, -.15, 0, .15, .3),
      name = "12-month change in house prices", labels = scales::percent_format(accuracy = 1)
    ) + 
    scale_colour_gradient(low = "#E7F0FF", high = "#035B8F") + #"#0072b2") +
    guides(colour = "none") +
    coord_cartesian(clip = "off") +
    #theme_dviz_grid() +
    theme(
      axis.ticks.length = unit(0, "pt"),
      plot.margin = margin(21, 14, 3.5, 1.5))

  ################################################################################################################
  
  load('/Volumes/GoogleDrive/My Drive/DATA/fred_md.rda')
  load('./fred_md.rda')

  fred_md %>%
    select(-date, -sasdate) %>%
    scale() %>%
    prcomp() -> pca
  
  pca_data <- data.frame(date = fred_md$date, pca$x) %>%
    mutate(
      type = ifelse(
        (ymd("1990-07-01") <= date & date < ymd("1991-03-01")) |
          (ymd("2001-03-01") <= date & date < ymd("2001-11-01")) |
          (ymd("2007-12-01") <= date & date < ymd("2009-06-01")),
        "recession",
        "recovery"
      )
    )
  
  pca_labels <-
    mutate(pca_data,
           label = ifelse(
             date %in% c(ymd("1990-01-01"), ymd("1991-03-01"), ymd("2001-11-01"),
                         ymd("2009-06-01"), ymd("2017-12-01")),
             format(date, "%b %Y"), ""
           )
    ) %>%
    filter(label != "") %>%
    mutate(
      nudge_x = c(.2, -.2, -.2, -.2, .2),
      nudge_y = c(.2, -.2, -.2, -.2, .2),
      hjust = c(0, 1, 1, 1, 0),
      vjust = c(0, 1, 1, 1, 0),
      nudge_x2 = c(.2, .2, .2, -.2, .2),
      nudge_y2 = c(.2, -.2, -1, .2, .2),
      hjust2 = c(0, 0, .2, 1, 0),
      vjust2 = c(0, 1, 1, 1, 0)
    )
  colors = darken(c("#D55E00", "#009E73"), c(0.1, 0.1))
  
  ggplot(filter(pca_data, date >= ymd("1990-01-01"))) +
    aes(x=PC1, y=PC2, color=type, alpha = date, group = 1) +
    geom_path(size = 1, lineend = "butt") +
    geom_text_repel(
      data = pca_labels,
      aes(label = label),
      alpha = 1,
      point.padding = .2, color = "black",
      min.segment.length = 0, size = 12/.pt,
      #family = dviz_font_family,
      nudge_x = pca_labels$nudge_x,
      nudge_y = pca_labels$nudge_y,
      hjust = pca_labels$hjust,
      vjust = pca_labels$vjust
    ) +
    scale_color_manual(
      values = colors,
      name = NULL
    ) +
    scale_alpha_date(range = c(0.45, 1), guide = "none") +
    scale_x_continuous(limits = c(-2, 15.5), name = "PC 1") +
    scale_y_continuous(limits = c(-5, 5), name = "PC 2") +
    #theme_dviz_grid(14) +
    theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.box.background = element_rect(color = NA, fill = "white"),
      legend.box.margin = margin(6, 0, 6, 0),
      plot.margin = margin(3, 1.5, 6, 1.5)
    )
  
  
  ggplot(filter(pca_data, date >= ymd("1990-01-01"))) +
    aes(x=PC3, y=PC2, color=type, alpha = date, group = 1) +
    geom_path(size = 1, lineend = "butt") +
    geom_text_repel(
      data = pca_labels,
      aes(label = label),
      alpha = 1,
      point.padding = .2, color = "black",
      min.segment.length = 0, size = 12/.pt,
      #family = dviz_font_family,
      nudge_x = pca_labels$nudge_x2,
      nudge_y = pca_labels$nudge_y2,
      hjust = pca_labels$hjust2,
      vjust = pca_labels$vjust2
    ) +
    scale_color_manual(
      values = colors,
      name = NULL
    ) +
    scale_alpha_date(range = c(0.45, 1), guide = "none") +
    scale_x_continuous(limits = c(-6, 8.5), name = "PC 3") +
    scale_y_continuous(limits = c(-5, 5), name = "PC 2") +
    #theme_dviz_grid(14) +
    theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.box.background = element_rect(color = NA, fill = "white"),
      legend.box.margin = margin(6, 0, 6, 0),
      plot.margin = margin(6, 1.5, 3, 1.5)
    )

#Let's look down jones:
  #load('/Volumes/GoogleDrive/My Drive/DATA/dow_jones_industrial.rda')
  load('./dow_jones_industrial.rda')
  
  dow_jones_industrial %>% filter(date >= ymd("2008/12/31") & date <= ymd("2010/01/10")) %>%
    ggplot(aes(date, close)) + 
    geom_line(color = "grey20", size = 0.75) + 
    scale_x_date(limits = c(ymd("2008-12-31"), ymd("2010-01-10")), expand = c(0, 0)) +
    xlab(NULL) + ylab("Dow Jones Industrial Average") +
    #theme_dviz_grid() +
    theme(
      plot.margin = margin(3, 12, 3, 1.5)
    )

  #averages plotted at the end of the window
  dow_jones_industrial %>% filter(date >= ymd("2008/12/31") & date <= ymd("2010/01/10")) %>%
    mutate(
      close_20d_ave = moving_ave(date, close, 20, center = FALSE),
      close_50d_ave = moving_ave(date, close, 50, center = FALSE),
      close_100d_ave = moving_ave(date, close, 100, center = FALSE)
    ) %>%
    ggplot(aes(date, close)) + 
    geom_line(color = "grey20", size = .35) +
    geom_line(aes(date, close_20d_ave, color = "20d"), size = 1, na.rm = TRUE) +
    geom_line(aes(date, close_50d_ave, color = "50d"), size = 1, na.rm = TRUE) +
    geom_line(aes(date, close_100d_ave, color = "100d"), size = 1, na.rm = TRUE) +
    scale_color_manual(
      values = c(
        `20d` = "#009e73",
        `50d` = "#d55e00",
        `100d` = "#0072b2"
      ),
      breaks = c("20d", "50d", "100d"),
      labels = c("20-day average", "50-day average", "100-day average"),
      name = NULL
    ) + 
    scale_x_date(
      limits = c(ymd("2008-12-31"), ymd("2010-01-10")), expand = c(0, 0),
      labels = NULL
    ) +
    xlab(NULL) + ylab("Dow Jones Industrial Average") +
    #theme_dviz_grid() +
    theme(
      plot.margin = margin(3, 12, 3, 1.5),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      legend.box.background = element_rect(fill = "white", color = NA),
      legend.box.margin = margin(6, 12, 0, 12),
      axis.ticks.x = element_blank()
    )

  #centered averages
  dow_jones_industrial %>% filter(date >= ymd("2008/12/31") & date <= ymd("2010/01/10")) %>%
    mutate(
      close_20d_ave = moving_ave(date, close, 20, center = TRUE),
      close_50d_ave = moving_ave(date, close, 50, center = TRUE),
      close_100d_ave = moving_ave(date, close, 100, center = TRUE)
    ) %>%
    ggplot(aes(date, close)) + 
    geom_line(color = "grey20", size = .35) +
    geom_line(aes(date, close_20d_ave, color = "20d"), size = 1, na.rm = TRUE) +
    geom_line(aes(date, close_50d_ave, color = "50d"), size = 1, na.rm = TRUE) +
    geom_line(aes(date, close_100d_ave, color = "100d"), size = 1, na.rm = TRUE) +
    scale_color_manual(
      values = c(
        `20d` = "#009e73",
        `50d` = "#d55e00",
        `100d` = "#0072b2"
      ),
      breaks = c("20d", "50d", "100d"),
      labels = c("20-day average", "50-day average", "100-day average"),
      name = NULL,
      guide = "none"
    ) + 
    scale_x_date(limits = c(ymd("2008-12-31"), ymd("2010-01-10")), expand = c(0, 0)) +
    xlab(NULL) + ylab("Dow Jones Industrial Average") +
    #theme_dviz_grid() +
    theme(
      plot.margin = margin(3, 12, 3, 1.5)
    )

  #let's use LOESS to smooth
  #simple example
  ggplot(mtcars, aes(x=drat, y=wt)) + geom_point() + geom_smooth()
  
  #down jones example again
  dow_jones_industrial %>% filter(date >= ymd("2008/12/31") & date <= ymd("2010/01/10")) %>%
    mutate(
      close_100d_ave = moving_ave(date, close, 100)
    ) %>%
    ggplot(aes(date, close)) + 
    geom_line(color = "grey20", size = .35) +
    geom_line(aes(date, close_100d_ave, color = "100d"), size = 1, na.rm = TRUE) +
    geom_smooth(aes(color = "smooth"), size = 1, na.rm = TRUE, se = FALSE) +
    ###rest is for cosmetic
    scale_color_manual(
      values = c(
        `100d` = "#d55e00",
        smooth = "#0072b2"
      ),
      breaks = c("smooth", "100d"),
      labels = c("LOESS smoother", "100-day average"),
      name = NULL
    ) + 
    scale_x_date(limits = c(ymd("2008-12-31"), ymd("2010-01-10")), expand = c(0, 0)) +
    xlab(NULL) + ylab("Dow Jones Industrial Average") +
    #theme_dviz_grid() +
    theme(
      legend.position = c(1, 0.48),
      legend.justification = c(1, 0.5),
      legend.box.background = element_rect(fill = "white", color = NA),
      legend.box.margin = margin(0, 12, 6, 12),
      plot.margin = margin(3, 12, 3, 1.5)
    )
  
  #
  cars93 <- MASS::Cars93
  ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) + 
    geom_point(color = "grey60") + 
    geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
    scale_x_continuous(
      name = "price (USD)",
      breaks = c(20, 40, 60),
      labels = c("$20,000", "$40,000", "$60,000")
    ) +
    scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") 
 
  #Try different smoothing methods
  cars_base <- ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) + geom_point(color = "grey60") + 
    scale_x_continuous(
      name = "price (USD)",
      breaks = c(20, 40, 60),
      labels = c("$20,000", "$40,000", "$60,000")
    ) +
    scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") 
    #theme_dviz_grid(12)  
  
 cars_base + geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2")
 cars_base + geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 5, bs = 'cr'), color = "#0072B2")
 cars_base + geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 3), color = "#0072B2")
 cars_base + geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 6, bs = 'gp'), color = "#0072B2")

  #fit models with predefined functions. Let's usenon-linear least squares method
 fit.out <- nls(
   Fuel.tank.capacity ~ a*Price/(Price + b) + c,
   data = cars93,
   start = c(a = -45, b = -1, c = 70)
 )
 # second model
 fit.out <- nls(
   Fuel.tank.capacity ~ A1 - A0*exp(-m*Price),
   data = cars93,
   start = c(A0 = 29.249, A1 = 19.621, m = 0.149),
   control = nls.control(maxiter = 1000, warnOnly = TRUE)
 )
 fit.df <- data.frame(
   Price = 7:62,
   Fuel.tank.capacity = predict(fit.out, data.frame(Price = 7:62))
 )

 ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) + 
   geom_point(color = "grey60") + 
   geom_line(data = fit.df, size = 1, color = "#0072B2") +
   #stat_function(fun = function(x) -24.22+1.38*x  + 22) +
   scale_x_continuous(
     name = "price (USD)",
     breaks = c(20, 40, 60),
     labels = c("$20,000", "$40,000", "$60,000")
   ) +
   scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") 

 #Blue jay data
 load('./blue_jays.rda')
 #load('/Volumes/GoogleDrive/My Drive/DATA/blue_jays.rda')
 
 ggplot(blue_jays, aes(Mass, Head, color = KnownSex, fill = KnownSex)) + 
   geom_point(pch = 21, color = "white", size = 2.5) +
   geom_smooth(method = "lm", size = 0.75, se = FALSE, fullrange = TRUE) +
   scale_x_continuous(name = "body mass (g)") +
   scale_y_continuous(name = "head length (mm)") +
   scale_fill_manual(
     values = c(F = "#D55E00C0", M = "#0072B2C0"),
     breaks = c("F", "M"),
     labels = c("female birds   ", "male birds"),
     name = NULL,
     guide = guide_legend(
       direction = "horizontal",
       override.aes = list(size = 3, linetype = 0)
     )
   ) +
   scale_color_manual(
     values = c(F = "#D55E00", M = "#0072B2"),
     breaks = c("F", "M"),
     labels = c("female birds   ", "male birds"),
     name = NULL
   ) +
   theme(
     #legend.position = c(1, 0.01),
     #legend.justification = c(1, 0),
     legend.position = "top",
     legend.justification = "right",
     legend.box.spacing = unit(3.5, "pt"), # distance between legend and plot
     legend.text = element_text(vjust = 0.6),
     legend.spacing.x = unit(2, "pt"),
     legend.background = element_rect(fill = "white", color = NA),
     legend.key.width = unit(10, "pt")
   )
 
 #Detrending House price indexes
 #load('/Volumes/GoogleDrive/My Drive/DATA/house_prices.rda')
 load('./house_prices.rda')
 
 hpi_trends <- house_prices %>%
   filter(year(date) >= 1980) %>%
   filter(state %in% c("California", "Nevada", "West Virginia", "Texas")) %>%
   mutate(
     date_numeric = as.numeric(date),
     hpi = house_price_index,
     log_hpi = log(hpi)
   ) %>%
   group_by(state) %>%
   mutate(
     hpi_trend = {
       coefs <- coef(lm(log_hpi ~ date_numeric)) #linear model
       exp(coefs[1] + coefs[2]*date_numeric) #using coefficients to get a line
     },
     hpi_detrended = hpi/hpi_trend #dividing index by the trend line here
   )
 
 ggplot(hpi_trends, aes(date, hpi)) + #Notice that we're adding two line plots on top of each other
   geom_line(aes(y = hpi_trend), color = "grey50", size = 0.4) +
   geom_line(color = "#0072B2", size = 0.75) +
   scale_x_date(name = NULL) +
   scale_y_log10(name = "House Price Index (Dec. 2000 = 100)") +
   facet_wrap(~state, scales = "free_x") +
   #theme_dviz_hgrid() +
   theme(
     strip.text = element_text(size = 12),
     strip.background = element_rect(fill = "grey85"),
     axis.line.x = element_line(color = "grey50"),
     axis.ticks.x = element_line(color = "grey50"),
     axis.ticks.y = element_blank(),
     axis.text.y = element_text(margin = margin(0, 0, 0, 0))
   )

 #Plotting detrended
 ggplot(hpi_trends, aes(date, hpi_detrended)) +
   geom_hline(yintercept = 1, color = "grey50", size = 0.4) +
   geom_line(color = "#0072B2", size = 0.75) +
   scale_x_date(name = NULL) +
   scale_y_log10(
     name = "House Price Index (detrended)",
     breaks = c(0.752, 1, 1.33, 1.77),
     labels = c("0.75", "1.00", "1.33", "1.77")
   ) +
   facet_wrap(~state, scales = "free_x") +
   #theme_dviz_hgrid() +
   theme(
     strip.text = element_text(size = 12),
     strip.background = element_rect(fill = "grey85"),
     axis.line.x = element_line(color = "grey50"),
     axis.ticks.x = element_line(color = "grey50"),
     axis.ticks.y = element_blank(),
     axis.text.y = element_text(margin = margin(0, 0, 0, 0))
   ) 
 