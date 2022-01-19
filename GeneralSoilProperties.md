General Soil Properties
================
Emily Lacroix
Last update: 1/14/2022

-   [Setup](#setup)
    -   [Load libraries](#load-libraries)
    -   [Files](#files)
    -   [Constants](#constants)
    -   [Useful Functions](#useful-functions)
-   [Texture](#texture)
    -   [Import and wrangle data](#import-and-wrangle-data)
    -   [Calculate texture](#calculate-texture)
        -   [Write function to calculate texture from hydrometer
            readings](#write-function-to-calculate-texture-from-hydrometer-readings)
        -   [Calculate texture for each
            sample](#calculate-texture-for-each-sample)
        -   [Summarize texture data](#summarize-texture-data)
-   [Total Carbon and Nitrogen](#total-carbon-and-nitrogen)
    -   [Import and summarise data](#import-and-summarise-data)
    -   [Statistics - total C](#statistics---total-c)
        -   [Check for normality](#check-for-normality)
        -   [Check for equal variances](#check-for-equal-variances)
        -   [ANOVA](#anova)
    -   [Statistics - C/N](#statistics---cn)
        -   [Check normality](#check-normality)
        -   [Check for equal variances](#check-for-equal-variances-1)
        -   [Wilcoxon rank sum](#wilcoxon-rank-sum)

# Setup

## Load libraries

    library(multcompView)
    library(car)
    library(tidyverse)
    library(readxl)

    options(dplyr.summarise.inform = FALSE)

## Files

    data_file <- "StanfordDish_AllData.xlsx"

## Constants

For texture analysis, all solutions were at 23 degrees C.

    concentration_hmp_g_mL <- 0.005
    viscosity_hmp_g_cm_s <- 0.9533 #value from table in Gee and Bauder 1986
    density_water_g_cm3 <- 1.0035 #specific to temperature 
    particle_density_g_cm3 <- 2.65
    g_cm_s2 <- 980.665
    hmp_density <- density_water_g_cm3 * (1 + (0.63 * concentration_hmp_g_mL))
    beta_calc <- 30 * viscosity_hmp_g_cm_s / (g_cm_s2 * (particle_density_g_cm3 - hmp_density))

## Useful Functions

    tri.to.squ<-function(x)
    {
      rn <- row.names(x)
      cn <- colnames(x)
      an <- unique(c(cn,rn))
      myval <- x[!is.na(x)]
      mymat <- 
        matrix(1, nrow = length(an), ncol = length(an), dimnames = list(an,an))
      for(ext in 1:length(cn))
      {
        for(int in 1:length(rn))
        {
          if(is.na(x[row.names(x) == rn[int], colnames(x) == cn[ext]])) next
          mymat[row.names(mymat) == rn[int], colnames(mymat) == cn[ext]] <- 
            x[row.names(x) == rn[int], colnames(x) == cn[ext]]
          mymat[row.names(mymat) == cn[ext], colnames(mymat) == rn[int]] <- 
            x[row.names(x) == rn[int], colnames(x) == cn[ext]]
        }
      }
      return(mymat)
    }

# Texture

This code calculates texture based on hydrometer readings.

## Import and wrangle data

    data <- 
      data_file %>% 
      read_xlsx(sheet = "Texture") %>% 
      mutate(
        p = 100* (hydrometer_reading_g_L - blank) / mass_g,
        h_prime = -0.164 * hydrometer_reading_g_L + 16.3,
        theta = 1000 * (beta_calc * h_prime)^0.5,
        x_um = theta * (time_min)^-0.5,
        ln_x = log(x_um) #log is the natural logarithm in R
      ) %>% 
      pivot_wider(
        id_cols = 
          c("site", "experiment", "core_num", "blank", "temperature_c", "mass_g"),
        names_from = time_min,
        values_from = c(p, h_prime, theta, x_um, ln_x)
      ) 

## Calculate texture

### Write function to calculate texture from hydrometer readings

    calculate_texture <-
      function(p_90, p_1440, ln_x_90, ln_x_1440, p_half, p_1, ln_x_half, ln_x_1){
        m_clay <- (p_90 - p_1440) / (ln_x_90 - ln_x_1440)
        p_2um <- m_clay*log(2) + p_1440
        
        m_sand <- (p_half - p_1) / (ln_x_half - ln_x_1)
        p_50um <- m_sand*log(50) + p_1440
        
        sand <- 100 - p_50um
        silt <- p_50um - p_2um
        clay <- p_2um
        
        print(paste(sand, silt, clay))
      }

### Calculate texture for each sample

    texture <- 
      data %>% 
      mutate(
        sand_silt_clay = 
          calculate_texture(
            p_90 = p_90, 
            p_1440 = p_1440, 
            ln_x_90 = ln_x_90, 
            ln_x_1440 = ln_x_1440, 
            p_half = p_0.5, 
            p_1 = p_1,
            ln_x_half = ln_x_0.5,
            ln_x_1 = ln_x_1
          )
      ) %>% 
      separate(sand_silt_clay, sep = " ", into = c("sand", "silt", "clay")) %>% 
      mutate(across(c(sand, silt, clay), as.numeric)) %>% 
      select(site, experiment, core_num, sand, silt, clay)

    ## [1] "37.6658101707024 41.0627632332954 21.2714265960022"
    ## [2] "28.8430105450218 39.8561419212061 31.3008475337722"
    ## [3] "46.1986758377631 25.0011060081928 28.8002181540441"
    ## [4] "43.8894652690132 39.1708390634719 16.9396956675149"
    ## [5] "46.4410937612074 39.7621597599921 13.7967464788006"
    ## [6] "28.8306677234205 54.8720409109024 16.297291365677" 
    ## [7] "54.0467734853181 40.9333361065826 5.01989040809932"
    ## [8] "52.7967734853181 40.9332258023854 6.27000071229652"
    ## [9] "51.5536313635488 41.5601097366285 6.88625889982279"

### Summarize texture data

    texture %>% 
      group_by(site) %>% 
      summarise(
        across(c(sand, silt, clay), list(mean = mean, se = ~sd(.)/sqrt(n())))
      )

    ## # A tibble: 3 × 7
    ##   site  sand_mean sand_se silt_mean silt_se clay_mean clay_se
    ##   <chr>     <dbl>   <dbl>     <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 DISHC      37.6   5.01       35.3   5.16      27.1    3.01 
    ## 2 DISHR      52.8   0.720      41.1   0.209      6.06   0.549
    ## 3 DISHT      39.7   5.49       44.6   5.14      15.7    0.959

DISHC texture = clay loam DISHR texture = sandy loam DISHT texture =
loam

# Total Carbon and Nitrogen

## Import and summarise data

    carbon_nitrogen <- 
      data_file %>% 
      read_xlsx(sheet = "TotalCarbon") %>% 
      group_by(site) %>% 
      mutate(
        n = n()
      ) %>% 
      group_by(site, n) %>% 
      summarise(across(c(perc_c, perc_n), list(mean = mean, sd = sd))) %>% 
      mutate(
        se_perc_c = perc_c_sd / sqrt(n),
        se_perc_n = perc_n_sd / sqrt(n),
      ) %>% 
      select(site, perc_c_mean, se_perc_c, perc_n_mean, se_perc_n)

    carbon_nitrogen %>% knitr::kable()

| site  | perc\_c\_mean | se\_perc\_c | perc\_n\_mean | se\_perc\_n |
|:------|--------------:|------------:|--------------:|------------:|
| DISHC |      1.788752 |   0.1385081 |     0.1158103 |   0.0071870 |
| DISHR |      1.069381 |   0.0732260 |     0.0880442 |   0.0072843 |
| DISHT |      1.261754 |   0.0412646 |     0.0901348 |   0.0029735 |

## Statistics - total C

### Check for normality

    data_file %>% 
      read_xlsx(sheet = "TotalCarbon") %>% 
      group_by(site) %>% 
      summarise(
        p_normality = shapiro.test(perc_c)$p.value
      )

    ## # A tibble: 3 × 2
    ##   site  p_normality
    ##   <chr>       <dbl>
    ## 1 DISHC       0.695
    ## 2 DISHR       0.969
    ## 3 DISHT       0.293

Percent C is normally distributed

### Check for equal variances

    leveneTest(
      perc_c ~ site, 
      data = data_file %>% 
        read_xlsx(sheet = "TotalCarbon")
    )

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  2  1.0026 0.3746
    ##       47

Variances are equal.

### ANOVA

    perc_c_aov <- 
      aov(
      perc_c ~ site, 
      data = data_file %>% 
        read_xlsx(sheet = "TotalCarbon")
    )

    summary(perc_c_aov)

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## site         2  1.467  0.7335   10.64 0.000154 ***
    ## Residuals   47  3.241  0.0689                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Significant differences. Do TukeyHSD as post-hoc test.

    TukeyHSD(perc_c_aov)

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = perc_c ~ site, data = data_file %>% read_xlsx(sheet = "TotalCarbon"))
    ## 
    ## $site
    ##                   diff        lwr        upr     p adj
    ## DISHR-DISHC -0.7193712 -1.1456618 -0.2930806 0.0004937
    ## DISHT-DISHC -0.5269974 -0.8280216 -0.2259731 0.0003042
    ## DISHT-DISHR  0.1923738 -0.1405032  0.5252508 0.3498271

## Statistics - C/N

    c_to_n <- 
      data_file %>% 
      read_xlsx(sheet = "TotalCarbon") %>% 
      mutate(c_to_n = perc_c / perc_n)

### Check normality

    c_to_n %>% 
      group_by(site) %>% 
      summarise(
        avg_c_to_n = mean(c_to_n, na.rm = TRUE),
        se_c_to_n = sd(c_to_n) / sqrt(n()),
        p_normality = shapiro.test(c_to_n)$p.value,
        p_normality2 = shapiro.test(c_to_n^2)$p.value,
      )

    ## # A tibble: 3 × 5
    ##   site  avg_c_to_n se_c_to_n p_normality p_normality2
    ##   <chr>      <dbl>     <dbl>       <dbl>        <dbl>
    ## 1 DISHC       15.4    0.733       0.127        0.0839
    ## 2 DISHR       12.2    0.206       0.312        0.304 
    ## 3 DISHT       14.0    0.0757      0.0475       0.0922

    c_to_n %>% 
      ggplot(aes(x = c_to_n^2)) + 
      geom_histogram() + 
      facet_grid(cols = vars(site))

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](GeneralSoilProperties_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Will try a squared transformation

### Check for equal variances

    leveneTest(c_to_n^2 ~ site, data = c_to_n)

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value  Pr(>F)  
    ## group  2  4.1455 0.02198 *
    ##       47                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

With a transformation, the C to N ratio can be normally distributed but
no equal variances. Will need to use pairwise Wilcoxon rank sum

### Wilcoxon rank sum

    c_to_n_wilcox <- 
      pairwise.wilcox.test(
      x = c_to_n$c_to_n,
      g = c_to_n$site,
      p.adjust.method = "BH",
      paired = FALSE
    )

    c_to_n_wilcox

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum exact test 
    ## 
    ## data:  c_to_n$c_to_n and c_to_n$site 
    ## 
    ##       DISHC  DISHR  
    ## DISHR 0.0159 -      
    ## DISHT 0.0066 8.1e-05
    ## 
    ## P value adjustment method: BH

    c_to_n_matrix_site <- tri.to.squ(c_to_n_wilcox$p.value)
    c_to_n_letters_site <- multcompLetters(c_to_n_matrix_site, compare ="<=", threshold = 0.05, Letters = letters)

    c_to_n_letters_site

    ## DISHC DISHR DISHT 
    ##   "a"   "b"   "c"
