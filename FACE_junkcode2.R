> summary(manova(cbind(unlist(LiCOR_df.tfm[,c(7:9)]))~CO2Tmt*H2OTmt, LiCOR_df.tfm_pmeans))
Error in h(simpleError(msg, call)) : 
  error in evaluating the argument 'object' in selecting a method for function 'summary': variable lengths differ (found for 'CO2Tmt')
> summary(manova(cbind(unlist(LiCOR_df.tfm_pmeans[,c(7:9)]))~CO2Tmt*H2OTmt, LiCOR_df.tfm_pmeans))
Error in h(simpleError(msg, call)) : 
  error in evaluating the argument 'object' in selecting a method for function 'summary': variable lengths differ (found for 'CO2Tmt')
> cbind(unlist(LiCOR_df.tfm_pmeans[,c(7:9)]))
[,1]
Photo.y1  1.220708e+01
Photo.y2  1.907423e+01
Photo.y3  1.828734e+01
Photo.y4  2.747313e+01
Photo.y5  1.999624e+01
Photo.y6  2.639835e+01
Photo.y7  2.059042e+00
Photo.y8  1.185316e+01
Photo.y9  3.827339e+00
Photo.y10 1.633889e+01
Photo.y11 2.002858e+01
Photo.y12 2.153460e+01
Photo.y13 1.200584e+01
Photo.y14 2.651463e+01
Photo.y15 9.636393e+00
Photo.y16 7.579988e+00
Photo.y17 1.291027e+01
Photo.y18 1.195568e+00
Photo.y19 6.532156e+00
Photo.y20 9.708780e+00
Photo.y21 2.231206e+01
Photo.y22 1.174427e+01
Photo.y23 2.736396e+01
Photo.y24 5.508523e+00
Photo.y25 7.713445e+00
Photo.y26 1.320927e+01
Photo.y27 8.916216e+00
Photo.y28 1.088497e+01
Photo.y29 5.412202e+00
Photo.y30 1.469685e+01
Cond.y1   7.247547e-02
Cond.y2   1.689130e-01
Cond.y3   9.601842e-02
Cond.y4   1.600390e-01
Cond.y5   1.497048e-01
Cond.y6   2.248685e-01
Cond.y7   2.130750e-02
Cond.y8   4.959208e-02
Cond.y9   2.025423e-02
Cond.y10  9.648108e-02
Cond.y11  8.086238e-02
Cond.y12  2.083571e-01
Cond.y13  6.695611e-02
Cond.y14  1.387672e-01
Cond.y15  4.043208e-02
Cond.y16  5.583415e-02
Cond.y17  1.327766e-01
Cond.y18  8.511576e-03
Cond.y19  5.487746e-02
Cond.y20  1.101187e-01
Cond.y21  2.279851e-01
Cond.y22  1.137716e-01
Cond.y23  3.356466e-01
Cond.y24  3.762715e-02
Cond.y25  1.046596e-01
Cond.y26  1.204099e-01
Cond.y27  2.240319e-01
Cond.y28  3.474006e-02
Cond.y29  3.742617e-02
Cond.y30  8.764209e-02
WUE.3501  1.553801e+02
WUE.3502  1.127405e+02
WUE.3503  1.909407e+02
WUE.3504  1.680344e+02
WUE.3505  1.451780e+02
WUE.3506  1.231487e+02
WUE.3507  9.854775e+01
WUE.3508  2.157253e+02
WUE.3509  2.090313e+02
WUE.35010 1.712797e+02
WUE.35011 2.506258e+02
WUE.35012 1.182361e+02
WUE.35013 1.582102e+02
WUE.35014 1.895847e+02
WUE.35015 2.336916e+02
WUE.35016 1.364363e+02
WUE.35017 1.051822e+02
WUE.35018 1.404638e+02
WUE.35019 1.267235e+02
WUE.35020 1.063775e+02
WUE.35021 1.003088e+02
WUE.35022 1.002941e+02
WUE.35023 8.129848e+01
WUE.35024 1.366291e+02
WUE.35025 6.873539e+01
WUE.35026 1.085645e+02
WUE.35027 3.495237e+01
WUE.35028 2.972958e+02
WUE.35029 1.505252e+02
WUE.35030 1.741422e+02
> summary(manova(cbind(unlist(LiCOR_df.tfm_pmeans$Photo.y),unlist(LiCOR_df.tfm_pmeans$Cond.y),unlist(LiCOR_df.tfm_pmeans$WUE.350tfm)))~CO2Tmt*H2OTmt, LiCOR_df.tfm_pmeans)
Length   Class    Mode 
3 formula    call 
> summary(manova(cbind(unlist(LiCOR_df.tfm_pmeans$Photo.y),unlist(LiCOR_df.tfm_pmeans$Cond.y),unlist(LiCOR_df.tfm_pmeans$WUE.350tfm))~CO2Tmt*H2OTmt, LiCOR_df.tfm_pmeans))
Df  Pillai approx F num Df den Df   Pr(>F)    
CO2Tmt         1 0.08587   0.7515      3     24 0.532248    
H2OTmt         1 0.56798  10.5175      3     24 0.000132 ***
  CO2Tmt:H2OTmt  1 0.06482   0.5545      3     24 0.650074    
Residuals     26                                            
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> manova.df <- biomass2. %>% 
  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  +   left_join(LiCOR_df.tfm[,c("Code","Photo.y","Cond.y", "WUE.350tfm")], by = "Code") %>% 
  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  +   # filter(!is.na(Photo.y)) %>% 
  +   # filter(!is.na(rootshoot)) %>% 
  +   left_join(SIF.[,c("Code","d13C")]) %>% 
  +   mutate(logCond.y = log(Cond.y), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  +   mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  +   left_join(lookup, by = "Plot") 
Joining with `by = join_by(Code)`
> biomass2. %>% 
  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y")], by = "Code") %>% 
  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  +   # filter(!is.na(Photo.y)) %>% 
  +   # filter(!is.na(rootshoot)) %>% 
  +   left_join(SIF.[,c("Code","d13C")]) %>% 
  +   group_by(Plot) %>% 
  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  +   mutate(across(where(is.numeric), ~ mean(.x))) %>% 
  +   left_join(lookup, by = "Plot") %>% View()
Joining with `by = join_by(Code)`
Error in View : In argument: `sqrt_WUE.350 = sqrt(WUE.350)`.
> biomass2. %>% 
  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  +   # filter(!is.na(Photo.y)) %>% 
  +   # filter(!is.na(rootshoot)) %>% 
  +   left_join(SIF.[,c("Code","d13C")]) %>% 
  +   group_by(Plot) %>% 
  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  +   mutate(across(where(is.numeric), ~ mean(.x))) %>% 
  +   left_join(lookup, by = "Plot") %>% View()
Joining with `by = join_by(Code)`
> biomass2. %>% 
  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  +   # filter(!is.na(Photo.y)) %>% 
  +   # filter(!is.na(rootshoot)) %>% 
  +   left_join(SIF.[,c("Code","d13C")]) %>% 
  +   group_by(Plot, Spp) %>% 
  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  +   mutate(across(where(is.numeric), ~ mean(.x))) %>% 
  +   left_join(lookup, by = "Plot") %>% View()
Joining with `by = join_by(Code)`
`summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
> manova.df <- biomass2. %>% 
  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  +   # filter(!is.na(Photo.y)) %>% 
  +   # filter(!is.na(rootshoot)) %>% 
  +   left_join(SIF.[,c("Code","d13C")]) %>% 
  +   group_by(Plot, Spp) %>% 
  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  +   mutate(across(where(is.numeric), ~ mean(.x))) %>% 
  +   left_join(lookup, by = "Plot") 
Joining with `by = join_by(Code)`
`summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
> manova.cbind <- cbind(unlist(manova.df$Photo.y),unlist(manova.df$logCond.y),unlist(manova.df$sqrt_WUE.350),unlist(manova.df$sqrt_totmass),unlist(manova.df$rootshoot),unlist(manova.df$lwc),unlist(manova.df$logRootRatio),unlist(manova.df$logFineRoot))
> summary(manova(manova.cbind ~ CO2Tmt*H2OTmt, manova.df))
Error in h(simpleError(msg, call)) : 
  error in evaluating the argument 'object' in selecting a method for function 'summary': object 'CO2Tmt' not found
> manova.df <- biomass2. %>% 
  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  +   left_join(SIF.[,c("Code","d13C")]) %>% 
  +   group_by(Plot, Spp) %>% 
  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  +   mutate(across(where(is.numeric), ~ mean(.x))) %>% 
  +   left_join(lookup, by = "Plot") %>% 
  +   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))
Joining with `by = join_by(Code)`
`summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
> manova.cbind <- cbind(unlist(manova.df$Photo.y),unlist(manova.df$logCond.y),unlist(manova.df$sqrt_WUE.350),unlist(manova.df$sqrt_totmass),unlist(manova.df$rootshoot),unlist(manova.df$lwc),unlist(manova.df$logRootRatio),unlist(manova.df$logFineRoot))
> summary(manova(manova.cbind ~ CO2Tmt*H2OTmt, manova.df))
Error in summary.manova(manova(manova.cbind ~ CO2Tmt * H2OTmt, manova.df)) : 
  residuals have rank 7 < 8
> manova(manova.cbind ~ CO2Tmt*H2OTmt, manova.df)
Call:
  manova(manova.cbind ~ CO2Tmt * H2OTmt, manova.df)

Terms:
  CO2Tmt   H2OTmt CO2Tmt:H2OTmt Residuals
resp 1            0.7651 783.1341       22.6456  154.4738
resp 2            0.2872   8.3836        0.5480    1.2368
resp 3            2.2393   0.0135        9.2109  146.3712
resp 4            1.3280   8.3634        0.1058    4.3342
resp 5            0.4395   3.6185        0.2689    1.8377
resp 6          157.9662 278.7687       28.0795  175.7468
resp 7            0.4403   0.1063        0.6055    1.8309
resp 8            0.2675   4.8825        0.0802    2.4481
Deg. of Freedom        1        1             1        18

Residual standard errors: 2.929484 0.2621256 2.851619 0.4907029 0.319526 3.124694 0.3189312 0.3687874
Estimated effects may be unbalanced
10 observations deleted due to missingness
> cor(manova.cbind)
[,1] [,2] [,3]       [,4]       [,5] [,6]       [,7]       [,8]
[1,]    1   NA   NA         NA         NA   NA         NA         NA
[2,]   NA    1   NA         NA         NA   NA         NA         NA
[3,]   NA   NA    1         NA         NA   NA         NA         NA
[4,]   NA   NA   NA  1.0000000 -0.3690450   NA  0.1044968  0.7126097
[5,]   NA   NA   NA -0.3690450  1.0000000   NA  0.2690113 -0.2868325
[6,]   NA   NA   NA         NA         NA    1         NA         NA
[7,]   NA   NA   NA  0.1044968  0.2690113   NA  1.0000000 -0.3455456
[8,]   NA   NA   NA  0.7126097 -0.2868325   NA -0.3455456  1.0000000
> View(manova.cbind)
> manova.cbind[,c(1,2,4,5)]
[,1]      [,2]     [,3]      [,4]
[1,] 15.640655 -2.201439 3.477787 1.7307957
[2,] 15.640655 -2.201439 3.477787 1.7307957
[3,] 24.031712 -2.250609 2.878799 1.1814774
[4,] 24.031712 -2.250609 2.878799 1.1814774
[5,] 23.197293 -1.695665 3.716840 1.3764519
[6,] 23.197293 -1.695665 3.716840 1.3764519
[7,]  6.956102 -3.426310 2.144066 2.2220802
[8,]  6.956102 -3.426310 2.144066 2.2220802
[9,] 11.169483 -3.132057 2.422252 2.0891881
[10,] 11.169483 -3.132057 2.422252 2.0891881
[11,] 20.781590 -2.041754 3.570363 0.7863844
[12,] 20.781590 -2.041754 3.570363 0.7863844
[13,] 19.260237 -2.339338 4.399942 1.1222879
[14,] 19.260237 -2.339338 4.399942 1.1222879
[15,]       NaN       NaN 2.401207 1.1888705
[16,]       NaN       NaN 2.401207 1.1888705
[17,] 10.245127 -2.452228 2.743877 1.4532533
[18,] 10.245127 -2.452228 2.743877 1.4532533
[19,]  3.863862 -3.834490 1.260904 1.6076633
[20,]  3.863862 -3.834490 1.260904 1.6076633
[21,] 16.010419 -1.842336 2.671648 1.5457914
[22,] 16.010419 -1.842336 2.671648 1.5457914
[23,] 19.554111 -1.632630 2.953741 1.1249840
[24,] 19.554111 -1.632630 2.953741 1.1249840
[25,]  6.610984 -2.768536 2.355289 2.8922363
[26,]  6.610984 -2.768536 2.355289 2.8922363
[27,] 11.062744 -1.806410 2.704262 0.6345233
[28,] 11.062744 -1.806410 2.704262 0.6345233
[29,]       NaN       NaN 2.329394 1.1138453
[30,]       NaN       NaN 2.329394 1.1138453
[31,] 10.054527 -2.859939 2.652911 2.1175883
[32,] 10.054527 -2.859939 2.652911 2.1175883
> manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df)
Call:
  manova(manova.cbind[, c(1, 2, 4, 5)] ~ CO2Tmt * H2OTmt, manova.df)

Terms:
  CO2Tmt   H2OTmt CO2Tmt:H2OTmt Residuals
resp 1           19.5072 742.6357       72.4556  264.0072
resp 2            0.2811   9.1084        0.8215    1.6005
resp 3            0.4086   8.1377        0.7576    5.9614
resp 4            0.3073   4.9832        0.1076    4.3216
Deg. of Freedom        1        1             1        24

Residual standard errors: 3.31667 0.2582387 0.4983887 0.4243436
Estimated effects may be unbalanced
4 observations deleted due to missingness
> summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
Df  Pillai approx F num Df den Df    Pr(>F)    
CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
  H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
  CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
  Residuals     24                                             
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> manova.cbindL <- manova.cbind %>% 
  +   filter(Spp=="L")
Error in UseMethod("filter") : 
  no applicable method for 'filter' applied to an object of class "c('matrix', 'array', 'double', 'numeric')"
> manova.df %>% 
  +   filter(Spp=="L") %>% 
  +   select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  +   lapply(unlist()) %>% 
  +   cbind()
Error in unlist() : argument "x" is missing, with no default
> manova.df %>% 
  +   filter(Spp=="L") %>% 
  +   select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  +   lapply(unlist(.x)) %>% 
  +   cbind()
Error: object '.x' not found
> source("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/FACE_junkcode.R", echo=TRUE)

> # FACE_junkcode
  > within_4.9.24 <- left_join(within_4.9.24, points_within, by = "position")
Error in eval(ei, envir) : object 'within_4.9.24' not found
> manova.df %>% 
  +   filter(Spp=="L") %>% 
  +   select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  +   lapply(unlist(.)) %>% 
  +   cbind()
Error in select(., Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot,  : 
                  unused arguments (Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot)
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   lapply(unlist(.)) %>% 
                  +   cbind()
                Adding missing grouping variables: `Plot`
                Error in match.fun(FUN) : 
                  'unlist(.)' is not a function, character or symbol
                > source("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/FACE_junkcode.R", echo=TRUE)
                
                > # FACE_junkcode
                  > within_4.9.24 <- left_join(within_4.9.24, points_within, by = "position")
                Error in eval(ei, envir) : object 'within_4.9.24' not found
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   lapply(unlist) %>% 
                  +   cbind()
                Adding missing grouping variables: `Plot`
                .           
                Plot         character,16
                Photo.y      numeric,16  
                logCond.y    numeric,16  
                sqrt_WUE.350 numeric,16  
                sqrt_totmass numeric,16  
                rootshoot    numeric,16  
                lwc          numeric,16  
                logRootRatio numeric,16  
                logFineRoot  numeric,16  
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   lapply(unlist) %>% 
                  +   cbind() %>% View()
                Adding missing grouping variables: `Plot`
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   cbind() %>% 
                  +   lapply(unlist) %>% View()
                Adding missing grouping variables: `Plot`
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   bind_rows() %>% View()
                Adding missing grouping variables: `Plot`
                > View(manova.cbind)
                > manova.cbindL <- manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   bind_rows()
                Adding missing grouping variables: `Plot`
                > summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                Error in h(simpleError(msg, call)) : 
                  error in evaluating the argument 'object' in selecting a method for function 'summary': invalid type (list) for variable 'manova.cbindL[, c(1, 2, 4, 5)]'
                > bind_rows(manova.cbindL[,c(1,2,4,5)])
                # A tibble: 16 × 4
                # Groups:   Plot [16]
                Plot  Photo.y sqrt_WUE.350 sqrt_totmass
                <chr>   <dbl>        <dbl>        <dbl>
                  1 1       15.6         11.5          3.48
                2 10      24.0         19.3          2.88
                3 11      23.2         11.6          3.72
                4 12       6.96        12.3          2.14
                5 13      11.2         15.5          2.42
                6 14      20.8         13.4          3.57
                7 15      19.3         13.2          4.40
                8 16     NaN          NaN            2.40
                9 2       10.2         11.0          2.74
                10 3        3.86        11.6          1.26
                11 4       16.0         10.2          2.67
                12 5       19.6          9.52         2.95
                13 6        6.61         9.99         2.36
                14 7       11.1          8.17         2.70
                15 8      NaN          NaN            2.33
                16 9       10.1         12.7          2.65
                > summary(manova(bind_rows(manova.cbindL[,c(1,2,4,5)]) ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                Error in h(simpleError(msg, call)) : 
                  error in evaluating the argument 'object' in selecting a method for function 'summary': invalid type (list) for variable 'bind_rows(manova.cbindL[, c(1, 2, 4, 5)])'
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate(across(vars(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot), unlist)) %>% View()
                Adding missing grouping variables: `Plot`
                Error in View : In argument: `across(...)`.
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate(across(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), unlist)) %>% View()
                Adding missing grouping variables: `Plot`
                Error in View : In argument: `across(...)`.
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), funs(unlist)) %>% View()
                Adding missing grouping variables: `Plot`
                Warning in View :
                  `funs()` was deprecated in dplyr 0.8.0.Please use a list of either functions or lambdas: 
                  
                  # Simple named list: 
                  list(mean = mean, median = median)
                
                # Auto named with `tibble::lst()`: 
                tibble::lst(mean, median)
                
                # Using lambdas
                list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) %>% View()
                Adding missing grouping variables: `Plot`
                > manova.cbindL <- manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist))
                Adding missing grouping variables: `Plot`
                > summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                Error in h(simpleError(msg, call)) : 
                  error in evaluating the argument 'object' in selecting a method for function 'summary': invalid type (list) for variable 'manova.cbindL[, c(1, 2, 4, 5)]'
                > str(manova.cbindL)
                gropd_df [16 × 9] (S3: grouped_df/tbl_df/tbl/data.frame)
                $ Plot        : chr [1:16] "1" "10" "11" "12" ...
                $ Photo.y     : num [1:16] 15.64 24.03 23.2 6.96 11.17 ...
                $ logCond.y   : num [1:16] -2.2 -2.25 -1.7 -3.43 -3.13 ...
                $ sqrt_WUE.350: num [1:16] 11.5 19.3 11.6 12.3 15.5 ...
                $ sqrt_totmass: num [1:16] 3.48 2.88 3.72 2.14 2.42 ...
                $ rootshoot   : num [1:16] 1.73 1.18 1.38 2.22 2.09 ...
                $ lwc         : num [1:16] 34.7 36.6 41.1 29.2 31.8 ...
                $ logRootRatio: num [1:16] -0.779 -1.281 -1.444 -0.935 -0.295 ...
                $ logFineRoot : num [1:16] 6.98 7.35 7.86 6.66 6.18 ...
                - attr(*, "groups")= tibble [16 × 2] (S3: tbl_df/tbl/data.frame)
                ..$ Plot : chr [1:16] "1" "10" "11" "12" ...
                ..$ .rows: list<int> [1:16] 
                .. ..$ : int 1
                .. ..$ : int 2
                .. ..$ : int 3
                .. ..$ : int 4
                .. ..$ : int 5
                .. ..$ : int 6
                .. ..$ : int 7
                .. ..$ : int 8
                .. ..$ : int 9
                .. ..$ : int 10
                .. ..$ : int 11
                .. ..$ : int 12
                .. ..$ : int 13
                .. ..$ : int 14
                .. ..$ : int 15
                .. ..$ : int 16
                .. ..@ ptype: int(0) 
                ..- attr(*, ".drop")= logi TRUE
                > manova.df <- biomass2. %>% 
                  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                  +   left_join(SIF.[,c("Code","d13C")]) %>% 
                  +   group_by(Plot, Spp) %>% 
                  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
                  +   ungroup() %>% 
                  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
                  +   mutate(across(where(is.numeric), ~ mean(.x))) %>% 
                  +   left_join(lookup, by = "Plot") %>% 
                  +   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))
                Joining with `by = join_by(Code)`
                `summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
                > manova.cbindL <- manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                > str(manova.cbindL)
                tibble [16 × 8] (S3: tbl_df/tbl/data.frame)
                $ Photo.y     : num [1:16] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
                $ logCond.y   : num [1:16] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
                $ sqrt_WUE.350: num [1:16] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
                $ sqrt_totmass: num [1:16] 2.79 2.79 2.79 2.79 2.79 ...
                $ rootshoot   : num [1:16] 1.51 1.51 1.51 1.51 1.51 ...
                $ lwc         : num [1:16] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
                $ logRootRatio: num [1:16] -0.927 -0.927 -0.927 -0.927 -0.927 ...
                $ logFineRoot : num [1:16] 6.88 6.88 6.88 6.88 6.88 ...
                > View(manova.cbindL)
                > manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% View()
                > View(manova.df)
                > biomass2. %>% 
                  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% View()
                > biomass2. %>% 
                  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                  +   left_join(SIF.[,c("Code","d13C")]) %>% 
                  +   group_by(Plot, Spp) %>% 
                  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% View()
                Joining with `by = join_by(Code)`
                `summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
                > biomass2. %>% 
                  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                  +   left_join(SIF.[,c("Code","d13C")]) %>% 
                  +   group_by(Plot, Spp) %>% 
                  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
                  +   ungroup() %>% 
                  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% View()
                Joining with `by = join_by(Code)`
                `summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
                > biomass2. %>% 
                  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                  +   left_join(SIF.[,c("Code","d13C")]) %>% 
                  +   group_by(Plot, Spp) %>% 
                  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
                  +   ungroup() %>% 
                  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
                  +   left_join(lookup, by = "Plot") %>% View()
                Joining with `by = join_by(Code)`
                `summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
                > biomass2. %>% 
                  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                  +   left_join(SIF.[,c("Code","d13C")]) %>% 
                  +   group_by(Plot, Spp) %>% 
                  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
                  +   ungroup() %>% 
                  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
                  +   left_join(lookup, by = "Plot") %>% 
                  +   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% View()
                Joining with `by = join_by(Code)`
                `summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
                > manova.df <- biomass2. %>% 
                  +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                  +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                  +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                  +   left_join(SIF.[,c("Code","d13C")]) %>% 
                  +   group_by(Plot, Spp) %>% 
                  +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
                  +   ungroup() %>% 
                  +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
                  +   left_join(lookup, by = "Plot") %>% 
                  +   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
                Joining with `by = join_by(Code)`
                `summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
                > manova.cbindL <- manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                > manova.cbindV <- manova.df %>% 
                  +   filter(Spp=="V") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                Df  Pillai approx F num Df den Df    Pr(>F)    
                CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
                  H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
                  CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
                  Residuals     24                                             
                ---
                  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                > summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                Error in h(simpleError(msg, call)) : 
                  error in evaluating the argument 'object' in selecting a method for function 'summary': invalid type (list) for variable 'manova.cbindL[, c(1, 2, 4, 5)]'
                > str(manova.cbindL)
                tibble [16 × 8] (S3: tbl_df/tbl/data.frame)
                $ Photo.y     : num [1:16] 12.21 20.59 20 2.06 6 ...
                $ logCond.y   : num [1:16] -2.62 -2.67 -1.9 -3.85 -3.93 ...
                $ sqrt_WUE.350: num [1:16] 12.47 25.57 12.05 9.93 18 ...
                $ sqrt_totmass: num [1:16] 4.01 2.84 4.15 2.28 2.83 ...
                $ rootshoot   : num [1:16] 0.644 0.498 0.725 0.742 0.901 ...
                $ lwc         : num [1:16] 36.6 35.2 40.1 35.7 38.5 ...
                $ logRootRatio: num [1:16] -0.793 -1.719 -1.737 -1.072 -0.328 ...
                $ logFineRoot : num [1:16] 6.85 7.38 8.16 6.59 6.34 ...
                > manova.dfL <- manova.df %>% 
                  +   filter(Spp=="L") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                > manova.dfV <- manova.df %>% 
                  +   filter(Spp=="V") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                > manova.cbindL <- cbind(unlist(manova.dfL$Photo.y),unlist(manova.dfL$logCond.y),unlist(manova.dfL$sqrt_WUE.350),unlist(manova.dfL$sqrt_totmass),unlist(manova.dfL$rootshoot),unlist(manova.dfL$lwc),unlist(manova.dfL$logRootRatio),unlist(manova.dfL$logFineRoot))
                > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                Df  Pillai approx F num Df den Df    Pr(>F)    
                CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
                  H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
                  CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
                  Residuals     24                                             
                ---
                  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                > summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                Df  Pillai approx F num Df den Df   Pr(>F)   
                CO2Tmt         1 0.32831   1.0998      4      9 0.413288   
                H2OTmt         1 0.76886   7.4842      4      9 0.006121 **
                  CO2Tmt:H2OTmt  1 0.05822   0.1391      4      9 0.963449   
                Residuals     12                                           
                ---
                  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                > summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                Error in h(simpleError(msg, call)) : 
                  error in evaluating the argument 'object' in selecting a method for function 'summary': invalid type (list) for variable 'manova.cbindV[, c(1, 2, 4, 5)]'
                > manova.cbindV <- cbind(unlist(manova.dfV$Photo.y),unlist(manova.dfV$logCond.y),unlist(manova.dfV$sqrt_WUE.350),unlist(manova.dfV$sqrt_totmass),unlist(manova.dfV$rootshoot),unlist(manova.dfV$lwc),unlist(manova.dfV$logRootRatio),unlist(manova.dfV$logFineRoot))
                > manova.dfV <- manova.df %>% 
                  +   filter(Spp=="V") %>% 
                  +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                  +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                Error in (function (bibtype, textVersion, header = NULL, footer = NULL,  : 
                                      reached elapsed time limit
                                    > manova.cbindV <- cbind(unlist(manova.dfV$Photo.y),unlist(manova.dfV$logCond.y),unlist(manova.dfV$sqrt_WUE.350),unlist(manova.dfV$sqrt_totmass),unlist(manova.dfV$rootshoot),unlist(manova.dfV$lwc),unlist(manova.dfV$logRootRatio),unlist(manova.dfV$logFineRoot))
                                    > View(manova.cbindV)
                                    > summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.52879   1.9638      4      7 0.2047469    
                                    H2OTmt         1 0.94341  29.1720      4      7 0.0001855 ***
                                      CO2Tmt:H2OTmt  1 0.60328   2.6612      4      7 0.1223626    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.52879   1.9638      4      7 0.2047469    
                                    H2OTmt         1 0.94341  29.1720      4      7 0.0001855 ***
                                      CO2Tmt:H2OTmt  1 0.60328   2.6612      4      7 0.1223626    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.52879   1.9638      4      7 0.2047469    
                                    H2OTmt         1 0.94341  29.1720      4      7 0.0001855 ***
                                      CO2Tmt:H2OTmt  1 0.60328   2.6612      4      7 0.1223626    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
                                      H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
                                      CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
                                      Residuals     24                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # kitchen sink
                                      > summary(manova(manova.cbind ~ CO2Tmt*H2OTmt, manova.df))
                                    Error in summary.manova(manova(manova.cbind ~ CO2Tmt * H2OTmt, manova.df)) : 
                                      residuals have rank 7 < 8
                                    > # kitchen sink oh wait this did not work bc too many variables
                                      > summary(manova(manova.cbind[,c(1,2,4:6)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df   Pr(>F)    
                                    CO2Tmt         1 0.61249    4.426      5     14  0.01257 *  
                                      H2OTmt         1 0.96237   71.603      5     14 1.81e-09 ***
                                      CO2Tmt:H2OTmt  1 0.44985    2.290      5     14  0.10194    
                                    Residuals     18                                            
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindL[,c(1,2,4:6)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.33113   0.7921      5      8 0.583997   
                                    H2OTmt         1 0.82732   7.6656      5      8 0.006422 **
                                      CO2Tmt:H2OTmt  1 0.05840   0.0992      5      8 0.989524   
                                    Residuals     12                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4:6)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df  Pr(>F)  
                                    CO2Tmt         1 0.67188   1.2286      5      3 0.46127  
                                    H2OTmt         1 0.97584  24.2371      5      3 0.01247 *
                                      CO2Tmt:H2OTmt  1 0.61569   0.9612      5      3 0.54943  
                                    Residuals      7                                         
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # kitchen sink oh wait this did not work bc too many variables
                                      > summary(manova(manova.cbind[,c(1,2,4,5,7)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df   Pr(>F)    
                                    CO2Tmt         1 0.45013    3.274      5     20 0.025432 *  
                                      H2OTmt         1 0.91867   45.185      5     20  3.2e-10 ***
                                      CO2Tmt:H2OTmt  1 0.60708    6.180      5     20 0.001283 ** 
                                      Residuals     24                                            
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindL[,c(1,2,4,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.35795   0.8920      5      8 0.5288897    
                                    H2OTmt         1 0.90967  16.1129      5      8 0.0005369 ***
                                      CO2Tmt:H2OTmt  1 0.32889   0.7841      5      8 0.5885976    
                                    Residuals     12                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.56588   1.5642      5      6 0.2990610    
                                    H2OTmt         1 0.94829  22.0061      5      6 0.0008552 ***
                                      CO2Tmt:H2OTmt  1 0.66754   2.4095      5      6 0.1572625    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.56588   1.5642      5      6 0.2990610    
                                    H2OTmt         1 0.94829  22.0061      5      6 0.0008552 ***
                                      CO2Tmt:H2OTmt  1 0.66754   2.4095      5      6 0.1572625    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # kitchen sink oh wait this did not work bc too many variables
                                      > summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.44200    4.159      4     21 0.0123314 *  
                                      H2OTmt         1 0.90467   49.825      4     21 2.008e-10 ***
                                      CO2Tmt:H2OTmt  1 0.60159    7.928      4     21 0.0004653 ***
                                      Residuals     24                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindL[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df  Pr(>F)    
                                    CO2Tmt         1 0.35762   1.2526      4      9 0.35612    
                                    H2OTmt         1 0.88290  16.9641      4      9 0.00032 ***
                                      CO2Tmt:H2OTmt  1 0.26780   0.8229      4      9 0.54232    
                                    Residuals     12                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.31751   0.8142      4      7 0.554470   
                                    H2OTmt         1 0.88584  13.5791      4      7 0.002061 **
                                      CO2Tmt:H2OTmt  1 0.62334   2.8961      4      7 0.104350   
                                    Residuals     10                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.31751   0.8142      4      7 0.554470   
                                    H2OTmt         1 0.88584  13.5791      4      7 0.002061 **
                                      CO2Tmt:H2OTmt  1 0.62334   2.8961      4      7 0.104350   
                                    Residuals     10                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > manova.df2 <- biomass2. %>% 
                                      +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                                      +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                                      +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                                      +   left_join(SIF.[,c("Code","d13C")]) %>% 
                                      +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
                                      +   left_join(lookup, by = "Plot") %>% 
                                      +   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
                                    Joining with `by = join_by(Code)`
                                    > manova.dfL2 <- manova.df2 %>% 
                                      +   filter(Spp=="L") %>% 
                                      +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                                      +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                                    > manova.dfV2 <- manova.df2 %>% 
                                      +   filter(Spp=="V") %>% 
                                      +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                                      +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                                    > manova.df2L <- manova.df2 %>% 
                                      +   filter(Spp=="L") %>% 
                                      +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                                      +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                                    > 
                                      > manova.df2V <- manova.df2 %>% 
                                      +   filter(Spp=="V") %>% 
                                      +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                                      +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                                    > 
                                      > manova.cbind <- cbind(unlist(manova.df2$Photo.y),unlist(manova.df2$logCond.y),unlist(manova.df2$sqrt_WUE.350),unlist(manova.df2$sqrt_totmass),unlist(manova.df2$rootshoot),unlist(manova.df2$lwc),unlist(manova.df2$logRootRatio),unlist(manova.df2$logFineRoot))
                                    > manova.cbind2L <- cbind(unlist(manova.df2L$Photo.y),unlist(manova.df2L$logCond.y),unlist(manova.df2L$sqrt_WUE.350),unlist(manova.df2L$sqrt_totmass),unlist(manova.df2L$rootshoot),unlist(manova.df2L$lwc),unlist(manova.df2L$logRootRatio),unlist(manova.df2L$logFineRoot))
                                    > manova.cbind2V <- cbind(unlist(manova.df2V$Photo.y),unlist(manova.df2V$logCond.y),unlist(manova.df2V$sqrt_WUE.350),unlist(manova.df2V$sqrt_totmass),unlist(manova.df2V$rootshoot),unlist(manova.df2V$lwc),unlist(manova.df2V$logRootRatio),unlist(manova.df2V$logFineRoot))
                                    > colnames(manova.df[,c(1,2,4,5)])
                                    [1] "Plot"             "Spp"              "StemWet_expanded" "rootmass_g"      
                                    > # reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
                                      > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df2))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.16505   3.2617      4     66   0.01675 *  
                                      H2OTmt         1 0.61117  25.9346      4     66 6.136e-13 ***
                                      CO2Tmt:H2OTmt  1 0.03497   0.5979      4     66   0.66540    
                                    Residuals     69                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbind2L[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.26596   3.0798      4     34   0.02879 *  
                                      H2OTmt         1 0.56013  10.8237      4     34 9.093e-06 ***
                                      CO2Tmt:H2OTmt  1 0.02064   0.1791      4     34   0.94763    
                                    Residuals     37                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbind2V[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)    
                                    CO2Tmt         1 0.27057   2.3183      4     25  0.08492 .  
                                    H2OTmt         1 0.77614  21.6693      4     25 8.02e-08 ***
                                      CO2Tmt:H2OTmt  1 0.22514   1.8160      4     25  0.15729    
                                    Residuals     28                                            
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df2))
                                    Df  Pillai approx F num Df den Df   Pr(>F)    
                                    CO2Tmt         1 0.13375   2.5477      4     66  0.04739 *  
                                      H2OTmt         1 0.63326  28.4909      4     66 9.21e-14 ***
                                      CO2Tmt:H2OTmt  1 0.02843   0.4828      4     66  0.74824    
                                    Residuals     69                                            
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbind2L[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.20810   2.2337      4     34   0.08594 .  
                                    H2OTmt         1 0.63628  14.8696      4     34 4.032e-07 ***
                                      CO2Tmt:H2OTmt  1 0.03375   0.2969      4     34   0.87791    
                                    Residuals     37                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbind2V[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.20908   1.6521      4     25    0.1926    
                                    H2OTmt         1 0.77161  21.1154      4     25 1.025e-07 ***
                                      CO2Tmt:H2OTmt  1 0.21447   1.7064      4     25    0.1801    
                                    Residuals     28                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
                                      > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Error in h(simpleError(msg, call)) : 
                                      error in evaluating the argument 'object' in selecting a method for function 'summary': variable lengths differ (found for 'CO2Tmt')
                                    > summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.32831   1.0998      4      9 0.413288   
                                    H2OTmt         1 0.76886   7.4842      4      9 0.006121 **
                                      CO2Tmt:H2OTmt  1 0.05822   0.1391      4      9 0.963449   
                                    Residuals     12                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.52879   1.9638      4      7 0.2047469    
                                    H2OTmt         1 0.94341  29.1720      4      7 0.0001855 ***
                                      CO2Tmt:H2OTmt  1 0.60328   2.6612      4      7 0.1223626    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
                                      > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Error in h(simpleError(msg, call)) : 
                                      error in evaluating the argument 'object' in selecting a method for function 'summary': variable lengths differ (found for 'CO2Tmt')
                                    > manova.cbind <- cbind(unlist(manova.df$Photo.y),unlist(manova.df$logCond.y),unlist(manova.df$sqrt_WUE.350),unlist(manova.df$sqrt_totmass),unlist(manova.df$rootshoot),unlist(manova.df$lwc),unlist(manova.df$logRootRatio),unlist(manova.df$logFineRoot))
                                    > # reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
                                      > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.20511   1.4837      4     23    0.2397    
                                    H2OTmt         1 0.76723  18.9525      4     23 5.151e-07 ***
                                      CO2Tmt:H2OTmt  1 0.09278   0.5880      4     23    0.6746    
                                    Residuals     26                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
                                      > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.20511   1.4837      4     23    0.2397    
                                    H2OTmt         1 0.76723  18.9525      4     23 5.151e-07 ***
                                      CO2Tmt:H2OTmt  1 0.09278   0.5880      4     23    0.6746    
                                    Residuals     26                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.32831   1.0998      4      9 0.413288   
                                    H2OTmt         1 0.76886   7.4842      4      9 0.006121 **
                                      CO2Tmt:H2OTmt  1 0.05822   0.1391      4      9 0.963449   
                                    Residuals     12                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.52879   1.9638      4      7 0.2047469    
                                    H2OTmt         1 0.94341  29.1720      4      7 0.0001855 ***
                                      CO2Tmt:H2OTmt  1 0.60328   2.6612      4      7 0.1223626    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # kitchen sink oh wait this did not work bc too many variables
                                      > summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.19034   1.3518      4     23    0.2813    
                                    H2OTmt         1 0.83338  28.7603      4     23 1.187e-08 ***
                                      CO2Tmt:H2OTmt  1 0.11973   0.7821      4     23    0.5484    
                                    Residuals     26                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindL[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df  Pr(>F)    
                                    CO2Tmt         1 0.35762   1.2526      4      9 0.35612    
                                    H2OTmt         1 0.88290  16.9641      4      9 0.00032 ***
                                      CO2Tmt:H2OTmt  1 0.26780   0.8229      4      9 0.54232    
                                    Residuals     12                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.31751   0.8142      4      7 0.554470   
                                    H2OTmt         1 0.88584  13.5791      4      7 0.002061 **
                                      CO2Tmt:H2OTmt  1 0.62334   2.8961      4      7 0.104350   
                                    Residuals     10                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
                                      > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.20511   1.4837      4     23    0.2397    
                                    H2OTmt         1 0.76723  18.9525      4     23 5.151e-07 ***
                                      CO2Tmt:H2OTmt  1 0.09278   0.5880      4     23    0.6746    
                                    Residuals     26                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > manova.df <- biomass2. %>% 
                                      +   dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
                                      +   left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
                                      +   left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
                                      +   left_join(SIF.[,c("Code","d13C")]) %>% 
                                      +   group_by(Plot, Spp) %>% 
                                      +   summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
                                      +   ungroup() %>% 
                                      +   mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
                                      +   left_join(lookup, by = "Plot") %>% 
                                      +   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
                                    Joining with `by = join_by(Code)`
                                    `summarise()` has grouped output by 'Plot'. You can override using the `.groups` argument.
                                    > 
                                      > manova.dfL <- manova.df %>% 
                                      +   filter(Spp=="L") %>% 
                                      +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                                      +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                                    > 
                                      > manova.dfV <- manova.df %>% 
                                      +   filter(Spp=="V") %>% 
                                      +   dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
                                      +   mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 
                                    > manova.cbind <- cbind(unlist(manova.df$Photo.y),unlist(manova.df$logCond.y),unlist(manova.df$sqrt_WUE.350),unlist(manova.df$sqrt_totmass),unlist(manova.df$rootshoot),unlist(manova.df$lwc),unlist(manova.df$logRootRatio),unlist(manova.df$logFineRoot))
                                    > manova.cbindL <- cbind(unlist(manova.dfL$Photo.y),unlist(manova.dfL$logCond.y),unlist(manova.dfL$sqrt_WUE.350),unlist(manova.dfL$sqrt_totmass),unlist(manova.dfL$rootshoot),unlist(manova.dfL$lwc),unlist(manova.dfL$logRootRatio),unlist(manova.dfL$logFineRoot))
                                    > manova.cbindV <- cbind(unlist(manova.dfV$Photo.y),unlist(manova.dfV$logCond.y),unlist(manova.dfV$sqrt_WUE.350),unlist(manova.dfV$sqrt_totmass),unlist(manova.dfV$rootshoot),unlist(manova.dfV$lwc),unlist(manova.dfV$logRootRatio),unlist(manova.dfV$logFineRoot))
                                    > # reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
                                      > summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.20511   1.4837      4     23    0.2397    
                                    H2OTmt         1 0.76723  18.9525      4     23 5.151e-07 ***
                                      CO2Tmt:H2OTmt  1 0.09278   0.5880      4     23    0.6746    
                                    Residuals     26                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.32831   1.0998      4      9 0.413288   
                                    H2OTmt         1 0.76886   7.4842      4      9 0.006121 **
                                      CO2Tmt:H2OTmt  1 0.05822   0.1391      4      9 0.963449   
                                    Residuals     12                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.52879   1.9638      4      7 0.2047469    
                                    H2OTmt         1 0.94341  29.1720      4      7 0.0001855 ***
                                      CO2Tmt:H2OTmt  1 0.60328   2.6612      4      7 0.1223626    
                                    Residuals     10                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > # kitchen sink oh wait this did not work bc too many variables
                                      > summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df))
                                    Df  Pillai approx F num Df den Df    Pr(>F)    
                                    CO2Tmt         1 0.19034   1.3518      4     23    0.2813    
                                    H2OTmt         1 0.83338  28.7603      4     23 1.187e-08 ***
                                      CO2Tmt:H2OTmt  1 0.11973   0.7821      4     23    0.5484    
                                    Residuals     26                                             
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindL[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
                                    Df  Pillai approx F num Df den Df  Pr(>F)    
                                    CO2Tmt         1 0.35762   1.2526      4      9 0.35612    
                                    H2OTmt         1 0.88290  16.9641      4      9 0.00032 ***
                                      CO2Tmt:H2OTmt  1 0.26780   0.8229      4      9 0.54232    
                                    Residuals     12                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                                    > summary(manova(manova.cbindV[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))
                                    Df  Pillai approx F num Df den Df   Pr(>F)   
                                    CO2Tmt         1 0.31751   0.8142      4      7 0.554470   
                                    H2OTmt         1 0.88584  13.5791      4      7 0.002061 **
                                      CO2Tmt:H2OTmt  1 0.62334   2.8961      4      7 0.104350   
                                    Residuals     10                                           
                                    ---
                                      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

                                    
                                                                      
## ended up doing outliers via Cook's distance en route to models
                                    
# Step 1: check data for outliers caused by *error* or *extreme values*
# LiCOR_df. responses Photo.y, Cond.y, WUE.350
# biomass2. responses totmass, rootshoot, LWC
# rootimage. responses ratio_2to1, dia_range_1 or w/e, # branchpoints
# SIF. response d13C
boxplot.stats(LiCOR_df.$Photo.y)$out
boxplot.stats(LiCOR_df.$Cond.y)$out
LiCOR_df.[which(LiCOR_df.$Cond.y %in% boxplot.stats(LiCOR_df.$Cond.y)$out),c(1,2,85:97)]
# these 4 have cond values 3x the average
boxplot.stats(LiCOR_df.$WUE.350)$out
LiCOR_df.[which(LiCOR_df.$WUE.350 %in% boxplot.stats(LiCOR_df.$WUE.350)$out),c(1,2,85:97)] %>% View()
# these 4 have really low cond; 10L6b has WUE of 1579

boxplot.stats(biomass2.$totmass)$out # I feel ok about these, think they're just big
biomass2.[which(biomass2.$totmass %in% boxplot.stats(biomass2.$totmass)$out),-c(8,9,11:18)] %>% View()
boxplot.stats(biomass2.$rootshoot)$out
biomass2.[which(biomass2.$rootshoot %in% boxplot.stats(biomass2.$rootshoot)$out),-c(8,9,11:18)] %>% View()
boxplot.stats(biomass2.$lwc)$out
biomass2.[which(biomass2.$lwc %in% boxplot.stats(biomass2.$lwc)$out),-c(8,9,11:18)] %>% View()


## cook's d with weights?

m1 <- lm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC), data = trans_dfwV, weights = sqrtTotmass_w)
ggpredict(m1, terms=c("CO2","meanSWC [4,42]")) %>% 
  plot(add.data = TRUE, ci = T)

cooksd1 <- cooks.distance(m1)
sample_size <- nobs(m1)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4/sample_size, names(cooksd1),""), col="red")  # add labels

influential1 <- as.numeric(names(cooksd1)[(cooksd1 > (4/sample_size))])
ggpredict(lm(sqrtTotmass ~ CO2*H2OTmt , data = trans_dfwV %>% 
               # filter(Spp == "V") %>% 
               filter(!row_number() %in% influential1), weights = sqrtTotmass_w ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = T)
summary(lm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC), data = trans_dfwV %>% 
             #  filter(Spp == "V") %>% 
             filter(!row_number() %in% influential1), weights = sqrtTotmass_w ))


# cook's d with categorical water

m1 <- lm(Photo.y ~ CO2*H2OTmt, data = trans_dfwV)
ggpredict(m1, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = T)

cooksd1 <- cooks.distance(m1)
sample_size <- nobs(m1)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4/sample_size, names(cooksd1),""), col="red")  # add labels

influential1 <- as.numeric(names(cooksd1)[(cooksd1 > (4/sample_size))])
ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV %>% 
               # filter(Spp == "V") %>% 
               filter(!row_number() %in% influential1)), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = T)
summary(lm(Photo.y ~ CO2*H2OTmt, data = trans_dfwV %>% 
             #  filter(Spp == "V") %>% 
             filter(!row_number() %in% influential1) ))

### what about brms
library(brms)
oaks1 <- brms::brm(Photo.y ~ rescale(CO2)*rescale(meanSWC),
                       data = trans_dfwV, chains = 3, # per plot means for Valleys
                       iter = 3000, warmup = 1000)
summary(oaks1)
plot(oaks1) # caterpillars
pp_check(oaks1)

oaks2 <- brms::brm(Photo.y ~ rescale(CO2)*rescale(meanSWC) + (1|Plot) + (1|Spp), # Plot, Spp as re
                       data = trans_df_full, chains = 3, # full dataset
                       iter = 3000, warmup = 1000)
summary(oaks2)
pairs(oaks2)
plot(oaks2)
pp_check(oaks2)

oaks3 <- brms::brm(Photo.y ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), # Plot as random effect
                            data = trans_df_full %>% filter(Spp=="V"), chains = 3, # all obs for Spp == V
                            iter = 3000, warmup = 1000)
plot(oaks3)
summary(oaks3)

# loo(oaks1, oaks2, oaks3, compare = TRUE) 
# AIC(oaks1, oaks2, oaks3)
oaks1_sqrtTotmass <- brms::brm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC),
                   data = trans_dfwV, chains = 3, # per plot means for Valleys
                   iter = 3000, warmup = 1000)
oaks2_sqrtTotmass <- brms::brm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC) + (1|Plot) + (1|Spp), # Plot, Spp as re
                   data = trans_df_full, chains = 3, # full dataset
                   iter = 3000, warmup = 1000)
oaks3_sqrtTotmass <- brms::brm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), # Plot as random effect
                   data = trans_df_full %>% filter(Spp=="V"), chains = 3, # all obs for Spp == V
                   iter = 3000, warmup = 1000)
summary(oaks1_sqrtTotmass)
summary(oaks2_sqrtTotmass)
summary(oaks3_sqrtTotmass)

oaks1_rootshoot <- brms::brm(rootshoot ~ rescale(CO2)*rescale(meanSWC),
                               data = trans_dfwV, chains = 3, # per plot means for Valleys
                               iter = 3000, warmup = 1000)
oaks2_rootshoot <- brms::brm(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot) + (1|Spp), # Plot, Spp as re
                               data = trans_df_full, chains = 3, # full dataset
                               iter = 3000, warmup = 1000)
oaks3_rootshoot <- brms::brm(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), # Plot as random effect
                               data = trans_df_full %>% filter(Spp=="V"), chains = 3, # all obs for Spp == V
                               iter = 3000, warmup = 1000)
summary(oaks1_rootshoot)
summary(oaks2_rootshoot)
summary(oaks3_rootshoot)

oaks1_rootshoot <- brms::brm(rootshoot ~ rescale(CO2)*rescale(meanSWC),
                             data = trans_dfwV, chains = 3, # per plot means for Valleys
                             iter = 3000, warmup = 1000)
oaks2_rootshoot <- brms::brm(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot) + (1|Spp), # Plot, Spp as re
                             data = trans_df_full, chains = 3, # full dataset
                             iter = 3000, warmup = 1000)
oaks3_rootshoot <- brms::brm(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), # Plot as random effect
                             data = trans_df_full %>% filter(Spp=="V"), chains = 3, # all obs for Spp == V
                             iter = 3000, warmup = 1000)
summary(oaks1_rootshoot)
summary(oaks2_rootshoot)
summary(oaks3_rootshoot)

oaks1_logAvg_area <- brms::brm(logAvg_area ~ rescale(CO2)*rescale(meanSWC),
                             data = trans_dfwV, chains = 3, # per plot means for Valleys
                             iter = 3000, warmup = 1000)
oaks2_logAvg_area <- brms::brm(logAvg_area ~ rescale(CO2)*rescale(meanSWC) + (1|Plot) + (1|Spp), # Plot, Spp as re
                             data = trans_df_full, chains = 3, # full dataset
                             iter = 3000, warmup = 1000)
oaks3_logAvg_area <- brms::brm(logAvg_area ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), # Plot as random effect
                             data = trans_df_full %>% filter(Spp=="V"), chains = 3, # all obs for Spp == V
                             iter = 3000, warmup = 1000)
summary(oaks1_logAvg_area)
summary(oaks2_logAvg_area)
summary(oaks3_logAvg_area)

oaks1_logCond.y <- brms::brm(logCond.y ~ rescale(CO2)*rescale(meanSWC),
                               data = trans_dfwV, chains = 3, # per plot means for Valleys
                               iter = 3000, warmup = 1000)
oaks2_logCond.y <- brms::brm(logCond.y ~ rescale(CO2)*rescale(meanSWC) + (1|Plot) + (1|Spp), # Plot, Spp as re
                               data = trans_df_full, chains = 3, # full dataset
                               iter = 3000, warmup = 1000)
oaks3_logCond.y <- brms::brm(logCond.y ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), # Plot as random effect
                               data = trans_df_full %>% filter(Spp=="V"), chains = 3, # all obs for Spp == V
                               iter = 3000, warmup = 1000)
summary(oaks1_logCond.y)
summary(oaks2_logCond.y)
summary(oaks3_logCond.y)
