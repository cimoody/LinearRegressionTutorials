# Linear Regression Tutorials
# Done by Cristina Moody
# 17 July 2015

# http://www.princeton.edu/~otorres/Regression101R.pdf
# Linear regression
install.packages("car");
library(car);
str(Prestige);
# 'data.frame':	102 obs. of  6 variables:
#     $ education: num  13.1 12.3 12.8 11.4 14.6 ...
# $ income   : int  12351 25879 9271 8865 8403 11030 8258 14163 11377 11023 ...
# $ women    : num  11.16 4.02 15.7 9.11 11.68 ...
# $ prestige : num  68.8 69.1 63.4 56.8 73.5 77.6 72.6 78.1 73.1 68.8 ...
# $ census   : int  1113 1130 1171 1175 2111 2113 2133 2141 2143 2153 ...
# $ type     : Factor w/ 3 levels "bc","prof","wc": 2 2 2 2 2 2 2 2 2 2 ...
reg1 <- lm(prestige ~ education + log2(income) + women, data = Prestige);
summary(reg1);
#
# Call:
#     lm(formula = prestige ~ education + log2(income) + women, data = Prestige)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -17.364  -4.429  -0.101   4.316  19.179
#
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -110.9658    14.8429  -7.476 3.27e-11 ***
#     education       3.7305     0.3544  10.527  < 2e-16 ***
#     log2(income)    9.3147     1.3265   7.022 2.90e-10 ***
#     women           0.0469     0.0299   1.568     0.12
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 7.093 on 98 degrees of freedom
# Multiple R-squared:  0.8351,	Adjusted R-squared:   0.83
# F-statistic: 165.4 on 3 and 98 DF,  p-value: < 2.2e-16
#
#
# Linear regression (heteroskedasticity-robust standard errors)
library(lmtest);
library(sandwich);
reg1$robse <- vcovHC(reg1, type = "HC1");
coeftest(reg1, reg1$robse);
#
# t test of coefficients:
#
#     Estimate  Std. Error t value  Pr(>|t|)
# (Intercept)  -110.965824   15.275221 -7.2644 9.074e-11 ***
#     education       3.730508    0.388808  9.5947 9.176e-16 ***
#     log2(income)    9.314666    1.382326  6.7384 1.107e-09 ***
#     women           0.046895    0.031484  1.4895    0.1396
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#
# Predict values/Residuals
# After running the regression
prestige_hat <- fitted(reg1); # predicted values
as.data.frame(prestige_hat);
# prestige_hat
# gov.administrators            65.07260
# general.managers              71.50702
# accountants                   60.16243
# purchasing.officers           54.21544
# chemists                      65.55434
# physicists                    72.70790
# biologists                    67.72890
# architects                    75.20712
# civil.engineers               68.75371
# mining.engineers              68.77237
# surveyors                     52.02945
# draughtsmen                   54.37693
# computer.programers           62.81355
# economists                    66.44428
# psychologists                 64.60173
# social.workers                62.25138
# lawyers                       80.68558
# librarians                    62.59103
# vocational.counsellors        70.66091
# ministers                     56.90504
# university.teachers           76.27680
# primary.school.teachers       59.86614
# secondary.school.teachers     68.31387
# physicians                    85.31677
# veterinarians                 77.51847
# osteopaths.chiropractors      75.52331
# nurses                        53.40014
# nursing.aides                 37.46322
# physio.therapsts              58.42100
# pharmacists                   71.27293
# medical.technicians           55.24449
# commercial.artists            48.73187
# radio.tv.announcers           56.98712
# athletes                      53.20605
# secretaries                   48.42120
# typists                       44.63671
# bookkeepers                   47.04198
# tellers.cashiers              37.88869
# computer.operators            47.49560
# shipping.clerks               37.57387
# file.clerks                   45.70006
# receptionsts                  41.71407
# mail.carriers                 39.55039
# postal.clerks                 39.60213
# telephone.operators           41.04416
# collectors                    46.76391
# claim.adjustors               47.78052
# travel.clerks                 50.98469
# office.clerks                 44.74186
# sales.supervisors             46.41385
# commercial.travellers         52.72510
# sales.clerks                  35.34349
# newsboys                      16.92814
# service.station.attendant     30.67483
# insurance.agents              53.91215
# real.estate.salesmen          50.51379
# buyers                        51.99937
# firefighters                  46.55913
# policemen                     52.07701
# cooks                         28.44802
# bartenders                    32.69087
# funeral.directors             49.29757
# babysitters                   15.05907
# launderers                    27.22045
# janitors                      26.68728
# elevator.operators            28.69610
# farmers                       24.92066
# farm.workers                  22.02415
# rotary.well.drillers          40.86713
# bakers                        30.83356
# slaughterers.1                33.15601
# slaughterers.2                33.15601
# canners                       21.48467
# textile.weavers               28.33068
# textile.labourers             25.63437
# tool.die.makers               47.58934
# machinists                    40.46145
# sheet.metal.workers           38.59367
# welders                       36.75626
# auto.workers                  37.59697
# aircraft.workers              40.19082
# electronic.workers            36.47000
# radio.tv.repairmen            43.16959
# sewing.mach.operators         23.97471
# auto.repairmen                35.72813
# aircraft.repairmen            47.03511
# railway.sectionmen            27.52967
# electrical.linemen            44.15066
# electricians                  45.38136
# construction.foremen          41.97841
# carpenters                    30.11199
# masons                        30.49379
# house.painters                31.47042
# plumbers                      38.97651
# construction.labourers        28.29019
# pilots                        63.15758
# train.engineers               42.82749
# bus.drivers                   33.64290
# taxi.drivers                  30.97497
# longshoremen                  34.03366
# typesetters                   44.87895
# bookbinders                   34.35822
prestige_resid <- residuals(reg1); # residuals
as.data.frame(prestige_resid);
# prestige_resid
# gov.administrators             3.7274014
# general.managers              -2.4070193
# accountants                    3.2375678
# purchasing.officers            2.5845601
# chemists                       7.9456572
# physicists                     4.8921019
# biologists                     4.8711040
# architects                     2.8928824
# civil.engineers                4.3462926
# mining.engineers               0.0276314
# surveyors                      9.9705459
# draughtsmen                    5.6230675
# computer.programers           -9.0135457
# economists                    -4.2442830
# psychologists                 10.2982666
# social.workers                -7.1513830
# lawyers                        1.6144218
# librarians                    -4.4910309
# vocational.counsellors       -12.3609075
# ministers                     15.8949567
# university.teachers            8.3231955
# primary.school.teachers       -0.2661392
# secondary.school.teachers     -2.2138734
# physicians                     1.8832315
# veterinarians                -10.8184657
# osteopaths.chiropractors      -7.1233058
# nurses                        11.2998577
# nursing.aides                 -2.5632191
# physio.therapsts              13.6789974
# pharmacists                   -1.9729288
# medical.technicians           12.2555091
# commercial.artists             8.4681348
# radio.tv.announcers            0.6128836
# athletes                       0.8939474
# secretaries                   -2.4211987
# typists                       -2.7367087
# bookkeepers                    2.3580239
# tellers.cashiers               4.4113103
# computer.operators             0.2043964
# shipping.clerks               -6.6738658
# file.clerks                  -13.0000555
# receptionsts                  -3.0140746
# mail.carriers                 -3.4503901
# postal.clerks                 -2.4021280
# telephone.operators           -2.9441635
# collectors                   -17.3639140
# claim.adjustors                3.3194770
# travel.clerks                -15.2846949
# office.clerks                 -9.1418644
# sales.supervisors             -4.9138482
# commercial.travellers        -12.5250956
# sales.clerks                  -8.8434930
# newsboys                      -2.1281375
# service.station.attendant     -7.3748257
# insurance.agents              -6.6121483
# real.estate.salesmen          -3.4137891
# buyers                        -0.8993745
# firefighters                  -3.0591344
# policemen                     -0.4770084
# cooks                          1.2519759
# bartenders                   -12.4908743
# funeral.directors              5.6024334
# babysitters                   10.8409285
# launderers                    -6.4204536
# janitors                      -9.3872828
# elevator.operators            -8.5961024
# farmers                       19.1793359
# farm.workers                  -0.5241473
# rotary.well.drillers          -5.5671279
# bakers                         8.0664446
# slaughterers.1                -7.9560108
# slaughterers.2                 1.6439892
# canners                        1.7153335
# textile.weavers                4.9693160
# textile.labourers              3.1656329
# tool.die.makers               -5.0893352
# machinists                     3.7385466
# sheet.metal.workers           -2.6936668
# welders                        5.0437377
# auto.workers                  -1.6969740
# aircraft.workers               3.5091796
# electronic.workers            14.3300035
# radio.tv.repairmen            -5.9695864
# sewing.mach.operators          4.2252924
# auto.repairmen                 2.3718722
# aircraft.repairmen             3.2648918
# railway.sectionmen            -0.2296680
# electrical.linemen            -3.2506613
# electricians                   4.8186438
# construction.foremen           9.1215890
# carpenters                     8.7880118
# masons                         5.7062102
# house.painters                -1.5704244
# plumbers                       3.9234944
# construction.labourers        -1.7901862
# pilots                         2.9424156
# train.engineers                6.0725145
# bus.drivers                    2.2570984
# taxi.drivers                  -5.8749701
# longshoremen                  -7.9336623
# typesetters                   -2.6789492
# bookbinders                    0.8417838
#
#
# Dummy regression with no interactions
# (analysis of covariance, fixed effects)
reg2 <- lm(prestige ~ education + log2(income) + type, data = Prestige);
summary(reg2);
#
# Call:
#     lm(formula = prestige ~ education + log2(income) + type, data = Prestige)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -13.511  -3.746   1.011   4.356  18.438
#
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -81.2019    13.7431  -5.909 5.63e-08 ***
#     education      3.2845     0.6081   5.401 5.06e-07 ***
#     log2(income)   7.2694     1.1900   6.109 2.31e-08 ***
#     typeprof       6.7509     3.6185   1.866   0.0652 .
# typewc        -1.4394     2.3780  -0.605   0.5465
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6.637 on 93 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.8555,	Adjusted R-squared:  0.8493
# F-statistic: 137.6 on 4 and 93 DF,  p-value: < 2.2e-16
#
# Re-ordering factor variables
Prestige$type <- with(Prestige, factor(type, levels = c("bc", "wc", "prof")));
summary(Prestige);
# education          income          women           prestige
# Min.   : 6.380   Min.   :  611   Min.   : 0.000   Min.   :14.80
# 1st Qu.: 8.445   1st Qu.: 4106   1st Qu.: 3.592   1st Qu.:35.23
# Median :10.540   Median : 5930   Median :13.600   Median :43.60
# Mean   :10.738   Mean   : 6798   Mean   :28.979   Mean   :46.83
# 3rd Qu.:12.648   3rd Qu.: 8187   3rd Qu.:52.203   3rd Qu.:59.27
# Max.   :15.970   Max.   :25879   Max.   :97.510   Max.   :87.20
# census       type
# Min.   :1113   bc  :44
# 1st Qu.:3120   wc  :23
# Median :5135   prof:31
# Mean   :5402   NA's: 4
# 3rd Qu.:8312
# Max.   :9517
#
#
# Dummy regression with interactions
reg3 <- lm(prestige ~ type*(education + log2(income)), data = Prestige);
summary(reg3);
#
# Call:
#     lm(formula = prestige ~ type * (education + log2(income)), data = Prestige)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -13.970  -4.124   1.206   3.829  18.059
#
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)
# (Intercept)           -120.0459    20.1576  -5.955 5.07e-08 ***
#     typewc                  30.2412    37.9788   0.796  0.42800
# typeprof                85.1601    31.1810   2.731  0.00761 **
#     education                2.3357     0.9277   2.518  0.01360 *
#     log2(income)            11.0782     1.8063   6.133 2.32e-08 ***
#     typewc:education         3.6400     1.7589   2.069  0.04140 *
#     typeprof:education       0.6974     1.2895   0.541  0.58998
# typewc:log2(income)     -5.6530     3.0519  -1.852  0.06730 .
# typeprof:log2(income)   -6.5356     2.6167  -2.498  0.01434 *
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6.409 on 89 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.871,	Adjusted R-squared:  0.8595
# F-statistic: 75.15 on 8 and 89 DF,  p-value: < 2.2e-16
#
#
# Diagnostics for linear regression
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
# dev.off();
residualPlots(reg1);
#       Test stat Pr(>|t|)
# education     -0.684    0.496
# income        -2.886    0.005
# type              NA       NA
# Tukey test    -2.610    0.009
#
# Influential variables
# library(car);
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
avPlots(reg1, id.n = 2, id.cex = 0.7);
#
# Outliers: QQ-Plots
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
qqPlot(reg1, id.n = 3);
# service.station.attendant
# 1
# electronic.workers
# 97
# medical.technicians
# 98
#
# Outliers: Bonferonni test
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
outlierTest(reg1);
#
# No Studentized residuals with Bonferonni p < 0.05
# Largest |rstudent|:
#     rstudent unadjusted p-value Bonferonni p
# medical.technicians 2.821091          0.0058632      0.57459
#
# High leverage (hat) points
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
influenceIndexPlot(reg1, id.n = 3);
# Cook's distance measures how much an observation influences the overall model or predicted values
# Studentizided residuals are the residuals divided by their estimated standard deviation as a way to standardized
# Bonferroni test to identify outliers
# Hat-points identify influential observations (have a high impact on the predictor variables)
#
# Influence Plots
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
influencePlot(reg1, id.n=3);
# Creates a bubble-plot combining the display of Studentized residuals, hat-values, and Cook's
# distance (represented in the circles).
#
# Testing for normality
reg1 <- lm(prestige ~ education + income +type, data = Prestige);
qqPlot(reg1);
# Look for the tails, points should be close to the line or within the confidence intervals.
# Quantile plots compare the Studentized residuals vs a t-distribution
# Other tests: shapiro.test(), mshapiro.test() in library(mvnormtest)-library(ts)
#
# Testing for heteroskedasticity
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
ncvTest(reg1);
# Non-constant Variance Score Test
# Variance formula: ~ fitted.values
# Chisquare = 0.09830307    Df = 1     p = 0.7538756
# Breush/Pagan and Cook/Weisberg score test for non-constant error variance. Null is constant variance
# See also residualPlots(reg1).
residualPlots(reg1);
#
#
# Testing for multicolinearity
reg1 <- lm(prestige ~ education + income + type, data = Prestige);
vif(reg1);
#           GVIF Df GVIF^(1/(2*Df))
# education 5.973932  1        2.444163
# income    1.681325  1        1.296659
# type      6.102131  2        1.571703
# A gvif> 4 suggests collinearity.
# “When there are strong linear relationships among the predictors in a regression analysis, the
# precision of the estimated regression coefficients in linear models declines compared to what it
# would have been were the predictors uncorrelated with each other” (Fox:359)
