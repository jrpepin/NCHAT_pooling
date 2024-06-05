#------------------------------------------------------------------------------------
# NCHAT_Money
# 00_$$_dataviz.R
#------------------------------------------------------------------------------------

# PACKAGES --------------------------------------------------------------------------

## WARNING: Remove the leading # to install packages below one time. 
## Change filepaths below one time

# if (!require(remotes)) install.packages("remotes") 
# if (!require(devtools)) install.packages("devtools") 
# remotes::install_github("fsolt/icpsrdata")                      # download ICPSR data
# devtools::install_git("https://git.rud.is/hrbrmstr/waffle.git") # install waffle package not on CRAN
# install.packages("pacman")                                      # Install pacman package if not installed

# Installs and loads packages automatically
library("pacman")                  # Load pacman package

# Install packages not yet installed & load them
pacman::p_load(
  here,       # relative file paths
  foreign,    # read data
  dplyr,      # variable processing
  tidyr,      # reshaping data
  forcats,    # reverse factor variables
  srvyr,      # calc % with survey weights
  MESS,       # round prop & preserve sum to 100%
  gtsummary,  # pretty weighted tables
  ggplot2,    # graphing
  ggtext,     # automatically wraps the text inside figure
  colorspace) # color palettes   

library("waffle")                 
library("icpsrdata")

# FILEPATHS ----------------------------------------------------------------------

## Set the project directory to the current working directory.
## Change the filepath to where the data was downloaded.
projDir <- here::here()                        # File path to this project's directory
dataDir <- "./../../../Dropbox/Data/NCHAT"     # File path to where data will be downloaded
figDir  <- here("images")

# DATA ---------------------------------------------------------------------------
## The icpsr_download function will be ask for the login information required by ICPSR: the user’s email and password.
## An optional setup step is to add the login information ICPSR requires to your .Rprofile as in the following example:
## options("icpsr_email" = "juanita-herrera@uppermidwest.edu",
##         "icpsr_password" = "password123!")
## URL for manual download: https://www.icpsr.umich.edu/web/DSDR/studies/38417

# THIS WILL FAIL IF OPTIONS AREN'T SPECIFIED AS INSTRUCTED ABOVE
# ONLY NEEDS TO BE DOWNLOADED 1X
# icpsrdata::icpsr_download( file_id = 38417, download_dir = file.path(dataDir))

## Load the data and create a new dataframe containing only the variables of interest.  
load(file=file.path(dataDir, "ICPSR_38417/DS0001", '38417-0001-Data.rda'))

data <- da38417.0001 %>%
  select(RESPONSEID, WEIGHT_MAINRESPONDENT, 
         Q40,       # money treatment
         MARITAL_1, # legal marital status
         C18D_B,    # parent identifier using skip logic
         # {ends_with("HHR16"), starts_with("A1_HHR17_"), starts_with("A1_HHR18_") is masked by ICPSR]}
         D2, HHR5,  # couple type
         AGE_SD)

glimpse(data)

### creates a function so level order remains the same as argument order
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

## Variables & Sample
data <- data %>%
  mutate(
    # money treatment
    money = fct_case_when(
      Q40 == "(3) Put all of our money together"     ~ "Put all money together",
      Q40 == "(2) Put some money together"           ~ "Put some money together",
      Q40 == "(1) We keep all of our money separate" ~ "Keep all money separate"),
    # marital status
    marco = fct_case_when(
      MARITAL_1  == "(1) Legal marriage"             ~ "Married",
      TRUE                                           ~ "Cohabiting"),
    # parental status
    parent = fct_case_when(
      !is.na(C18D_B)                                 ~ "Parenting",
      TRUE                                           ~ "Not parenting"),
    # couple type
    couple = fct_case_when(
      (D2 == "(1) Man"   & HHR5 == "(2) Woman")      |
        (D2 == "(2) Woman" & HHR5 == "(1) Man")      ~ "Different-gender",
      D2 == "(2) Woman" & HHR5 == "(2) Woman"        ~ "Woman-woman",
      D2 == "(1) Man"   & HHR5 == "(1) Man"          ~ "Man-man"),
    # age
    age = fct_case_when(
      AGE_SD == "(1) 18-34"                          ~ "Age: 20-34",      # change bottom code to match codebook
      AGE_SD == "(2) 35-50"                          ~ "Age: 35-50",
      AGE_SD == "(3) 51-69"                          ~ "Age: 51-60")) %>% # change top code to match codebook
  select(money, marco, parent, couple, age, WEIGHT_MAINRESPONDENT, RESPONSEID) %>%
  drop_na() # drop respondents with any missing data

glimpse(data)

## Set as survey data
waffle_svy <- data %>%
  as_survey_design(id = RESPONSEID,
                   weights = WEIGHT_MAINRESPONDENT)

## Create a table of sample
waffle_svy %>%
  select(c(-RESPONSEID, -WEIGHT_MAINRESPONDENT)) %>%
  tbl_svysummary(statistic = all_categorical() ~ "{n_unweighted} ({p_unweighted}%)",
                 label = list(money    ~ "Money Treatment",
                              marco    ~ "Marital Status",
                              parent   ~ "Parental Status",
                              couple   ~ "Couple Type",
                              age      ~ "Age")) %>%
  modify_header(update = stat_0 ~ "**N = {N_unweighted}**") %>%
  modify_caption("**Table 1. Summary statistics of the analytic sample (unweighted)**") %>%
  modify_header(label = "**Variable**")

# CHI-SQUARED TESTS -----------------------------------------------------------------------
# the statistics below match the output from survey::svychisq using statistic = "Chisq"
# waffle_svy %>%  
#   filter(marco == "Married") %>%
#   survey:: svychisq(~money+parent,., statistic="Chisq")

## Within marital status
waffle_svy %>%
  tbl_strata(strata = marco,~.x %>%
               tbl_svysummary(by = money, include = c(parent, couple, age),
                              statistic = all_categorical() ~ "{n_unweighted} [{n}]") %>%
               add_p(test = everything() ~ "svy.adj.chisq.test")) %>%
  modify_header(update = list(
    stat_1_1 ~ "**Put all money together**",
    stat_1_2 ~ "**Put all money together**",
    stat_2_1 ~ "**Put some money together**",
    stat_2_2 ~ "**Put some money together**",
    stat_3_1 ~ "**Keep all money separate**",   
    stat_3_2 ~ "**Keep all money separate**")) %>%
  modify_caption("**Table 2. Chi-squared tests within marital groups** <br>unweighted n [weighted n]") %>%
  modify_header(label = "**Variable**")


## Between marital status
htest_svy <- data %>% # reshape data
  pivot_longer(
    cols = c(parent, couple, age),
    names_to = "subgroup",
    values_to = "level") %>%
  as_survey_design(id = RESPONSEID,
                   weights = WEIGHT_MAINRESPONDENT)

htest_svy %>%
  tbl_strata(strata = level,~.x %>%
               tbl_svysummary(by = money, include = c(marco),
                              statistic = all_categorical() ~ "{n_unweighted} [{n}]",
                              label = list(marco    ~ " ")) %>%
               add_p(test = everything() ~ "svy.adj.chisq.test"),
             .combine_with = "tbl_stack",
             .quiet = TRUE) %>%
  modify_header(update = list(
    stat_1 ~ "**Put all money together**",
    stat_2 ~ "**Put some money together**",
    stat_3 ~ "**Keep all money separate**")) %>%
  modify_caption("**Table 3.Chi-squared tests within demographic group by marital status** <br>unweighted n [weighted n]") %>%
  modify_header(label = "**Variable**")

# CREATE FIGURE DATA -------------------------------------------------------------
## Create bivariate statistics

# all -- just for reference
df0 <- waffle_svy %>%
  group_by( money) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = MESS::round_percent(vals), # round with preserving to 100%
         vals_low = MESS::round_percent(vals_low), 
         vals_upp = MESS::round_percent(vals_upp))

# marital status
df1 <- waffle_svy %>%
  group_by(marco, money) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>% # create summary proportions
  mutate(vals     = MESS::round_percent(vals), # round with preserving to 100%
         vals_low = MESS::round_percent(vals_low), 
         vals_upp = MESS::round_percent(vals_upp), 
         group    = "parent", # to graph on same line as parents
         subgroup = "All")

# parents
df2 <- waffle_svy %>%
  group_by(marco, parent, money) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>%
  mutate(vals     = MESS::round_percent(vals), # round with preserving to 100%
         vals_low = MESS::round_percent(vals_low), 
         vals_upp = MESS::round_percent(vals_upp), 
         group    = "parent") %>%
  rename(subgroup = parent)

# couple type
df3 <- waffle_svy %>%
  group_by(marco, couple, money) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>%
  mutate(vals     = MESS::round_percent(vals), # round with preserving to 100%
         vals_low = if_else(vals_low < 0, 0, vals_low), # round negative value to zero
         vals_low = MESS::round_percent(vals_low), 
         vals_upp = MESS::round_percent(vals_upp), 
         group    = "couple") %>%
  rename(subgroup = couple)

# age groups
df4 <- waffle_svy %>%
  group_by(marco, age, money) %>%
  summarize(vals  = survey_mean(na.rm = TRUE, vartype = "ci")) %>%
  mutate(vals     = MESS::round_percent(vals), # round with preserving to 100%
         vals_low = MESS::round_percent(vals_low), 
         vals_upp = MESS::round_percent(vals_upp), 
         group    = "age") %>%
  rename(subgroup = age)

# Combine datasets
waffle_data <- df1 %>%
  full_join(df2) %>%
  full_join(df3) %>%
  full_join(df4) %>%
  mutate(group = as_factor(group),
         subgroup = as_factor(subgroup))

head(waffle_data, n = 10) # to check results

# VISUALIZE ------------------------------------------------------------------
## Prep the data for plotting

## Create a combo variable because geom_waffle doesn't work with facet_grid()
waffle_data$c<-paste0(as.character(waffle_data$marco),"\n", as.character(waffle_data$subgroup))

waffle_data$c <- factor(waffle_data$c, 
                        levels = c("Married\nAll", "Married\nParenting", "Married\nNot parenting",
                                   "Cohabiting\nAll", "Cohabiting\nParenting", "Cohabiting\nNot parenting",
                                   "Married\nDifferent-gender", "Married\nMan-man", "Married\nWoman-woman", 
                                   "Cohabiting\nDifferent-gender", "Cohabiting\nMan-man", "Cohabiting\nWoman-woman", 
                                   "Married\nAge: 20-34", "Married\nAge: 35-50", "Married\nAge: 51-60",
                                   "Cohabiting\nAge: 20-34", "Cohabiting\nAge: 35-50", "Cohabiting\nAge: 51-60"), 
                        ordered = FALSE)

waffle_data$subgroup <- factor(waffle_data$subgroup, 
                        levels = c("All", "Parenting", "Not parenting",
                                   "Different-gender", "Man-man", "Woman-woman", 
                                   "Age: 20-34", "Age: 35-50", "Age: 51-60"), 
                        ordered = FALSE)

## Create a variable for plotting the values & correct top rows
waffle_data$yaxis <- round(waffle_data$vals/10) 
waffle_data$yaxis[waffle_data$c == "Cohabiting\nParenting" & 
                    waffle_data$money == "Keep all money separate"] <- 4 # correction re: rounded off chart
waffle_data$yaxis[waffle_data$c == "Cohabiting\nNot parenting" & 
                    waffle_data$money == "Keep all money separate"] <- 6 # move up a row 
waffle_data$yaxis[waffle_data$c == "Married\nWoman-woman" & 
                    waffle_data$money == "Keep all money separate"] <- 3 # move up a row 
waffle_data$yaxis[waffle_data$c == "Cohabiting\nDifferent-gender" & 
                    waffle_data$money == "Keep all money separate"] <- 6 # move up a row 
waffle_data$yaxis[waffle_data$c == "Cohabiting\nAge: 20-34" & 
                    waffle_data$money == "Keep all money separate"] <- 6 # move up a row 
waffle_data$yaxis[waffle_data$c == "Cohabiting\nAge: 51-60" & 
                    waffle_data$money == "Keep all money separate"] <- 6 # move up a row 

## CREATE THE OG PLOT
p1<- waffle_data %>%
  ggplot(aes(fill = money, values = vals)) +
  geom_waffle(color = "white", size = .2, n_rows = 10, flip = TRUE) +
  geom_text(aes(x=1.1, y=yaxis, label=vals), 
            color    = "black", 
            size     = 3,
            fontface ="bold",
            position = position_stack(reverse = TRUE)) +
  facet_wrap(vars(c), ncol = 6) +
  scale_fill_manual(values = sequential_hcl(3, palette = "OrYel")) +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(panel.grid      = element_blank(),
        axis.title      = element_blank(),
        axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        legend.title    = element_blank(),
        plot.title.position   = "plot",
        plot.caption.position =  "plot",
        plot.caption          = element_textbox_simple(),
        panel.spacing         = unit(0, "lines"),
        plot.subtitle         = element_text(face = "italic"),
        legend.position       = "top") +
  labs(title = "U.S. couples' money arrangements by demographic groups (n = 3,396)",
       subtitle = "Percentage of U.S. couples (aged 20-60) who selected _____________",
       caption = "**Figure 1.** Data are from the National Couples' Health and Time Study (U.S.), 2020-2021. 
       Sampling weights are used to adjust for the complex sampling process of the survey, 
       such as the oversample of sexual and gender diverse families.
       Chi-squared tests confirm all demographic group comparisons between married and cohabiting couples are statistically significant (p<.05).
       Among married couples, parents and different-gender couples are more likely than their counterparts to report they 'put all money together' (p<.05).
       There are no statistically significant differences by demographic group among cohabiting couples. 
       Additional details about data access, variable construction, cell sizes and distribution, and chi-squared tests 
       between and within marital groups are available at: https:&#47;&#47;github.com&#47;jrpepin&#47;NCHAT_Money.") 

p1 # view the plot


## CREATE THE COUPLE-TYPE PLOT
# Define color palette
c_palette <- c("#18BC9C",
               "#3498DB",
               "#F39C12",
               "#E74C3C")


p2 <- waffle_data %>%
  filter(group == "couple" & marco == "Married") %>%
  ggplot(aes(fill = money, values = vals)) +
  geom_waffle(color = "white", size = .2, n_rows = 10, flip = TRUE) +
  geom_text(aes(x=1.1, y=yaxis, label=vals), 
            color    = "black", 
            size     = 3,
            fontface ="bold",
            position = position_stack(reverse = TRUE)) +
  facet_wrap(vars(subgroup), ncol = 3) +
  scale_fill_manual(values = c_palette) +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(panel.grid      = element_blank(),
        axis.title      = element_blank(),
        axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        legend.title    = element_blank(),
        plot.title.position   = "plot",
        plot.caption.position =  "plot",
        plot.caption          = element_textbox_simple(),
        panel.spacing         = unit(0, "lines"),
        plot.subtitle         = element_text(face = "italic"),
        legend.position       = "right") + 
  guides(fill = guide_legend(reverse=T))

p2 # view the plot

# save the image as png file in the project directory
ggsave(filename = file.path(figDir, "waffle.png"), p2, width=6, height=2, bg = "white", units="in", dpi=300)

