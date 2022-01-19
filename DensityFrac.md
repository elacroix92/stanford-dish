MAOC\_POC
================
Emily Lacroix
Last update: 1/19/2022

-   [Setup](#setup)
    -   [Load libraries](#load-libraries)
    -   [Files](#files)
    -   [Constants](#constants)
    -   [Figure theme](#figure-theme)
    -   [Figure labels](#figure-labels)
-   [Import and Combine Data](#import-and-combine-data)
    -   [Import data](#import-data)
    -   [Combine Data](#combine-data)
-   [Figures, Tables, Calculations](#figures-tables-calculations)
    -   [Percent Recovery](#percent-recovery)
    -   [Table 1: MAOC & POC](#table-1-maoc--poc)
-   [Statistics](#statistics)
    -   [POC](#poc)
        -   [Check for normality](#check-for-normality)
        -   [Check for equal variances](#check-for-equal-variances)
        -   [ANOVA](#anova)
    -   [MAOC](#maoc)
        -   [Check for normality](#check-for-normality-1)
        -   [Check for equal variances](#check-for-equal-variances-1)
        -   [ANOVA](#anova-1)

This script analyzes the Density Fractionation data from the soils used
in both the Disturbance and Oxygen Enrichment Incubations.

# Setup

## Load libraries

    library(readxl)
    library(car)

    ## Loading required package: carData

    library(tidyverse)

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x dplyr::recode() masks car::recode()
    ## x purrr::some()   masks car::some()

    options(dplyr.summarise.inform = FALSE)

## Files

    data_file <- "StanfordDish_AllData.xlsx"

## Constants

    control_cores <- c(1, 5, 9, 13)

## Figure theme

    my_theme <- function(base_size = 12, base_family = ""){ ## Control base font face and size. use `rel()` for lelative font size.
      theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
          panel.border = element_rect(colour = "black", fill = "transparent"),
          panel.background  = element_blank(),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.position="top",
          legend.title = element_blank(),
          strip.text = element_text(size = 13)
        )
    }

## Figure labels

    site_labels <- c(
      "DISHC" = "Clayey\nLoam",
      "DISHT" = "Loam",
      "DISHR" = "Sandy\n Loam"
    )

# Import and Combine Data

## Import data

    dens_frac <- 
      data_file %>% 
      read_xlsx(sheet = "DensityFractionation")

    tc_tn <-
      data_file %>% 
      read_xlsx(sheet = "TotalCarbon")

## Combine Data

    maoc_poc <-
      dens_frac %>% 
      left_join(
        tc_tn %>% 
          group_by(site) %>% 
          summarise(avg_perc_c = mean(perc_c, na.rm = TRUE)),
        by = "site"
      ) %>% 
      mutate(
        estimated_total_mg_c = 1000 * sample_mass * (avg_perc_c / 100),
        prop_maoc = mg_c_hf / estimated_total_mg_c,
        prop_poc = mg_c_lf / estimated_total_mg_c,
        across("site", factor, levels = c("DISHC", "DISHT", "DISHR")),
        percent_recovery = 100* (total_g_lf + mass_hf) / sample_mass
      ) 

# Figures, Tables, Calculations

## Percent Recovery

    maoc_poc %>% pull(percent_recovery) %>% summary()

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   94.28   96.60   98.78   98.12   99.88  101.39

## Table 1: MAOC & POC

    maoc_poc %>% 
      group_by(site) %>% 
      summarise(
        avg_perc_poc = 100 * mean(prop_poc),
        se_perc_poc = 100 * sd(prop_poc) / sqrt(n()),
        avg_perc_maoc = 100 * mean(prop_maoc),
        se_perc_maoc = 100 * sd(prop_maoc) / sqrt(n())
      ) 

    ## # A tibble: 3 × 5
    ##   site  avg_perc_poc se_perc_poc avg_perc_maoc se_perc_maoc
    ##   <fct>        <dbl>       <dbl>         <dbl>        <dbl>
    ## 1 DISHC         5.48        1.36          81.9         4.23
    ## 2 DISHT        11.4         2.82          82.4         6.32
    ## 3 DISHR        17.3         1.84          76.2         2.41

# Statistics

## POC

### Check for normality

    maoc_poc %>% 
      group_by(site) %>% 
      summarise(
        normality_stat = shapiro.test(prop_poc)$statistic,
        normality_p_value = shapiro.test(prop_poc)$p.value
      )

    ## # A tibble: 3 × 3
    ##   site  normality_stat normality_p_value
    ##   <fct>          <dbl>             <dbl>
    ## 1 DISHC          0.945             0.684
    ## 2 DISHT          0.933             0.611
    ## 3 DISHR          0.913             0.497

### Check for equal variances

    leveneTest(
      prop_poc ~ site, 
      data = maoc_poc 
    )

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  2  1.8533 0.2118
    ##        9

The sites proportion of POC are normally distributed with equal
variances.

### ANOVA

    summary(aov(prop_poc ~ site, data = maoc_poc)) 

    ##             Df  Sum Sq Mean Sq F value Pr(>F)  
    ## site         2 0.02807 0.01403   7.976 0.0102 *
    ## Residuals    9 0.01584 0.00176                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

They are significantly different! Now do Tukey’s HSD

    TukeyHSD(aov(prop_poc ~ site, data = maoc_poc))

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = prop_poc ~ site, data = maoc_poc)
    ## 
    ## $site
    ##                   diff         lwr       upr     p adj
    ## DISHT-DISHC 0.05912207 -0.02369098 0.1419351 0.1694759
    ## DISHR-DISHC 0.11846510  0.03565205 0.2012782 0.0079318
    ## DISHR-DISHT 0.05934303 -0.02347003 0.1421561 0.1676347

Only DISHR is different from DISHC, otherwise there is slight overlap it
appears

## MAOC

### Check for normality

    maoc_poc %>% 
      group_by(site) %>% 
      summarise(
        normality_stat = shapiro.test(prop_maoc)$statistic,
        normality_p_value = shapiro.test(prop_maoc)$p.value
      )

    ## # A tibble: 3 × 3
    ##   site  normality_stat normality_p_value
    ##   <fct>          <dbl>             <dbl>
    ## 1 DISHC          0.996             0.985
    ## 2 DISHT          0.953             0.735
    ## 3 DISHR          0.848             0.221

### Check for equal variances

    leveneTest(
      prop_maoc ~ site, 
      data = maoc_poc 
    )

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  2   1.694 0.2375
    ##        9

Proportion MAOC is also normally distributed with equal variances.

### ANOVA

    summary(aov(prop_maoc ~ site, data = maoc_poc)) 

    ##             Df  Sum Sq  Mean Sq F value Pr(>F)
    ## site         2 0.00967 0.004837    0.57  0.585
    ## Residuals    9 0.07644 0.008493
