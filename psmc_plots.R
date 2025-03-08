# psmc_plots.R 
# for clouded leopard genomics manuscript (figure 5)
# Madeleine A. Becker, updated 3/8/25


# load packages -----------------------------------------------------------

library(tidyverse)
library(ggpubr)


# load data ---------------------------------------------------------------

# text file with all raw PSMC file names
paths <- read_csv("Plotting_PSMCs/all_files.txt", col_names = FALSE)

# each of these is a separate csv output file from SMC++
gr1gr2 <- read_csv("unadmixed1_2_split.csv", col_names = TRUE)
CH_wild_US <- read_csv("CH_wild_US.csv", col_names = TRUE)
CH_captive_US <- read_csv("CH_captive_US.csv", col_names = TRUE)
CH_captive_wild <- read_csv("CH_wild_CH_captive.csv", col_names = TRUE)
  


# colors ------------------------------------------------------------------
CH <- "#f46e6f"
UK <- "#42c4c0"
US <- "#b36df2"
SU <- "black"

gr1 <- "red"
gr2 <- "orange"


# PSMC plots  --------------------------------------------------------------------

# function for getting data from each text file
df_psmc <- function (path){
  individual <- str_sub(path,-10,-7)
  region <- str_sub(individual,1,2)
  df <- read_tsv(path, col_names = FALSE) %>% 
    select("year" = X1, "pop" = X2) %>% 
    mutate("sample" = individual) %>% 
    mutate("country" = region)
}

# create dataframe of all text files using custom function
big_df <- tibble()
for (file in paths$X1){
  temp <- df_psmc(file)
  big_df <- rbind(big_df, temp)
}

# plot of all individuals including Sunda (not in ms)
big_df %>% 
  ggplot(aes(x = year, y = pop, group = sample, color = country)) +
  scale_color_manual(name = "Region", values = c(CH, SU, UK, US)) +
  geom_step() +
  scale_x_log10(limits = c(10000,1000000), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0,0)) +
  xlab("Years") + ylab("Effective population size (10E4)") + 
  labs (color = "Region")

# just a plot of group 1
group1 <- 
  big_df %>% 
  filter(!sample %in% c("UK06","UK07","UK08","UK10","UK12","UK17","UK18","UK19","SU21","SU38")) %>% 
  ggplot(aes(x = year, y = pop, group = sample, color = country)) +
  scale_color_manual(name = "Region", values = c(CH, UK, US)) +
  geom_step() +
  scale_x_log10(limits = c(10000,1000000), 
                expand = c(0,0),
                guide = "axis_logticks") +
  scale_y_continuous(limits = c(0,5), expand = c(0,0)) +
  xlab("Years") + 
  ylab("Effective population size (10E4)") +
  theme_classic()

# just a plot of group 2
group2 <- 
  big_df %>% 
  filter(sample %in% c("UK06","UK07","UK08","UK10","UK12","UK17","UK18","UK19")) %>% 
  ggplot(aes(x = year, y = pop, group = sample, color = country)) +
  scale_color_manual(name = "Region", values = UK) +
  geom_step() +
  scale_x_log10(limits = c(10000,1000000),
                expand = c(0,0),
                guide = "axis_logticks") +
  scale_y_continuous(limits = c(0,5), expand = c(0,0)) +
  xlab("Years") + 
  ylab("Effective population size (10E4)") +
  theme_classic()

# plot of just group 1 and 2 (not in ms)
both <- ggarrange(group1, group2, labels = c("A", "B"), ncol = 2, nrow = 1)


# SMC++ -------------------------------------------------------------------

# function for getting the year of inferred split from SMC++ text files
get_split <- function(df){
  min(max(select(filter(df, plot_num==0),x)),
      max(select(filter(df, plot_num==1),x)))}

# implement function for group 1 and 2 split; make a label for graph
g_split <- get_split(gr1gr2)
g_split_label <- paste(round(g_split), "ya")

# plot the group 1/group 2 SMC++ data, including split line
groups <-
  gr1gr2 %>% ggplot(aes(x = x, y = y, color = label)) +
  scale_color_manual(name = "Ecotype", 
                     values = c(gr1, gr2),
                     labels = c("Group 1", "Group 2")) +
  geom_step(size=1.25) +
  scale_x_log10(limits = c(100,100000), expand = c(0,0),guide = "axis_logticks") +
  scale_y_log10(guide = "axis_logticks") +
  geom_vline(xintercept = g_split) +
  xlab("Years") + 
  ylab("Effective population size") +
  geom_text(aes(x = 9000, label=g_split_label, y=9000),
            color = "black",
            hjust = 0,
            size = 4) +
  theme_classic()

# same thing as above but this uses 3 different populations: CH wild, CH captive, US (captive), so you have to pre-select which final split to use for the x-intercept line
cap_split <- get_split(CH_captive_US)
wild_split <- get_split(CH_wild_US)
CH_US_label <- paste("US/China: ",
                     round(cap_split),"-",
                     round(wild_split), " ya",
                     sep = "")

CH_split <- get_split(CH_captive_wild)
CH_label <- paste("China/China:",
                  round(CH_split),"ya")

US_CH <-
  CH_wild_US %>% 
  mutate(plot_num = plot_num + 2) %>% 
  filter(label == "CH_wild") %>%
  bind_rows(CH_captive_US %>%   
              filter(x <= wild_split)) %>% 
  mutate(label = factor(label,levels = c("CH_wild", "CH_captive", "US"))) %>% 
  ggplot(aes(x = x, y = y, color = label)) +
  scale_color_manual(name = "Group", 
                     values = c(CH, "#C13B3C", US),
                     labels = c("China wild",
                                "China captive",
                                "US captive")) +
  geom_step(size=1.25) + 
  scale_x_log10(limits = c(400,1000000), expand = c(0,0),guide = "axis_logticks") +
  scale_y_log10(guide = "axis_logticks") +
  geom_vline(xintercept = cap_split) +
  xlab("Years") + 
  ylab("Effective population size") + 
  geom_text(aes(x = 8500, label=CH_US_label, y=10000),
            color = "black", size = 3.5, hjust = 0) +
  geom_text(aes(x = 8500, label=CH_label, y=30000),
            color = "black", size = 3.5, hjust = 0) +
  theme_classic()

# this is the final plot used for the manuscript, with 2 PSMC and 2 SMC++ panels
all <- ggarrange(group1, group2, groups, US_CH, 
                 labels = c("A","B","C","D"), ncol = 2, nrow = 2)



# export as PNG: 1089 x 608