library(here)
library(data.table)
library(summarytools)

setwd(here())
devtools::load_all("../flexMissing") # flexMissing package
setwd(here('Real-data/FSD3185/Study/Data'))


d <- read.csv2(file = "daF3185_eng.csv")

# X = q4_1 How have you funded your studies? Student loan
# Y = q4_5 How have you funded your studies? Personal income through work
# 1 = Completely, 2 = Partially, 3 = Not at all
# 1 = "Mainly" would be a better translation
data <- data.frame(
  X1 = as.factor(d$q4_1),
  X2 = as.factor(d$q4_5),
  R1 = as.numeric(is.finite(d$q4_1)),
  R2 = as.numeric(is.finite(d$q4_5))
)

dfSummary(data)

# create mDAG
G <- make.graph(obs_variables = c(),
                missing_variables = c('X1','X2'),
                missing_indicators = c('R1','R2'),
                di_edges = list(c('X1','X2'),
                                c('X1','R2'), c('R1','R2'))
)

ID <- f.ID_algorithm(G,T)

plot(G, vertex.size = 15, save=T,file='mDAG.png', width = 12, height = 9, units = "in", res = 300)
plot(ID,vertex.size = 15, save=T,file='ID.png', width = 12, height = 9, units = "in", res = 300)

f.summarize_ID(ID)

## propensity scores
set.seed(7)
propensitys <- f.propensity(graph=G,
                            data=data,
                            ID=ID,
                            law='target', # 'target' or 'full'
                            link.R="logit",
                            formula.R=list(R2='R2 ~ 1 + I(X1==1) + I(X1==2) + R1'),
                            crossfit=F,
                            K=5,
                            superlearner=F,
                            lib.SL =c("SL.glm","SL.earth","SL.ranger","SL.mean"),
                            vocal=T)

summary(propensitys[['R1']]$Rk_fit)
summary(propensitys[['R2']]$Rk_fit)


## with M-estimation ====
# p(X1)
est_FUN <- function(data, theta){
  with(data, c( (X1==1)-theta[1], (X1==2)-theta[2], 1- theta[1]-theta[2]- theta[3]) ) # theta[1]=p(X1=1), theta[2]=p(X1=2), theta[3]=p(X1=3)
}

est1 <- f.Mestimation(data=data,
              graph=G,
              propensitys=propensitys,
              ID=ID,
              law='target',
              est_FUN=est_FUN,
              variables=list(c('X1'), c('X1'), NULL),
              start=c(0.2, 0.2, 0.6),
              vocal=T)

# the estimates
cat("Estimates: ", est1$results@estimates, "\n")
cat("SD:", sqrt(diag(est1$results@vcov)), "\n")

# p(X2|X1)
# Note: inferece from f.Mestimation is valid in this case as E((\partial{I(R1=1,R2=1)/p(R1=1,R2=1|X1,X2)}/\partial)*est_FUN)=0, where beta is the parameter for the propensity scores
est_FUN <- function(data, theta){
  with(data, c( (X1==1)*{(X2==1)-theta[1]}, 
                (X1==1)*{(X2==2)-theta[2]},
                1 - theta[1]- theta[2]-theta[3],
                (X1==2)*{(X2==1)-theta[4]},
                (X1==2)*{(X2==2)-theta[5]},
                1 - theta[4]- theta[5]-theta[6],
                (X1==3)*{(X2==1)-theta[7]},
                (X1==3)*{(X2==2)-theta[8]},
                1 - theta[7]- theta[8]-theta[9]
                ) 
       )
}


est2 <- f.Mestimation(data=data,
                     graph=G,
                     propensitys=propensitys,
                     ID=ID,
                     law='target',
                     est_FUN=est_FUN,
                     variables=list(c('X1','X2'), c('X1','X2'), NULL,
                                    c('X1','X2'), c('X1','X2'), NULL,
                                    c('X1','X2'), c('X1','X2'), NULL),
                     start=rep(c(0.2, 0.2, 0.6),3),
                     vocal=T)

# the estimates
cat("Estimates: ", est2$results@estimates, "\n")
cat("SD:", sqrt(diag(est2$results@vcov)), "\n")

save(list=c('propensitys','est1','est2'), file='results.Rdata')

## Organize results into a table ====

library(huxtable)

## ---- helper: format estimate + 95% CI ----
make_ci <- function(est, variance, level = 0.95, digits = 3) {
  se <- sqrt(variance)
  z  <- qnorm(1 - (1 - level)/2)
  
  lower <- est - z * se
  upper <- est + z * se
  
  paste0(
    round(est, digits), " (",
    round(lower, digits), ", ",
    round(upper, digits), ")"
  )
}

## p(X1)
est_px1  <- est1$results@estimates
variance_px1 <- diag(est1$results@vcov)

param_px1 <- c(
  "\\(p(X_1 = 1)\\)",
  "\\(p(X_1 = 2)\\)",
  "\\(p(X_1 = 3)\\)"
)

tab_px1 <- data.frame(
  Parameter = param_px1,
  `Estimate (95% CI)` = make_ci(est_px1, variance_px1),
  stringsAsFactors = FALSE
)


## p(X2 | X1)
est_px2  <- est2$results@estimates
variance_px2 <- diag(est2$results@vcov)

param_px2 <- c(
  "\\(p(X_2 = 1 \\mid X_1 = 1)\\)",
  "\\(p(X_2 = 2 \\mid X_1 = 1)\\)",
  "\\(p(X_2 = 3 \\mid X_1 = 1)\\)",
  "\\(p(X_2 = 1 \\mid X_1 = 2)\\)",
  "\\(p(X_2 = 2 \\mid X_1 = 2)\\)",
  "\\(p(X_2 = 3 \\mid X_1 = 2)\\)",
  "\\(p(X_2 = 1 \\mid X_1 = 3)\\)",
  "\\(p(X_2 = 2 \\mid X_1 = 3)\\)",
  "\\(p(X_2 = 3 \\mid X_1 = 3)\\)"
)

tab_px2 <- data.frame(
  Parameter = param_px2,
  `Estimate (95% CI)` = make_ci(est_px2, variance_px2),
  stringsAsFactors = FALSE
)

## p(R2=1 | X1, R1=1)
est3 <- predict(
  propensitys[['R2']]$Rk_fit,
  newdata = data.frame(X1 = c(1, 2, 3),R1=rep(1,3)),
  type = "response",
  se.fit = TRUE
)

eta_r2 <- est3$fit
variance_r2  <- est3$se.fit^2

param_r2 <- c(
  "\\(p(R_2 = 1 \\mid X_1 = 1, R_1=1)\\)",
  "\\(p(R_2 = 1 \\mid X_1 = 2, R_1=1)\\)",
  "\\(p(R_2 = 1 \\mid X_1 = 3, R_1=1)\\)"
)

tab_r2 <- data.frame(
  Parameter = param_r2,
  `Estimate (95% CI)` = make_ci(eta_r2, variance_r2),
  stringsAsFactors = FALSE
)

## p(R1=1)
est4 <- predict(
  propensitys[['R1']]$Rk_fit,
  type = "response",
  se.fit = TRUE
)


eta_r1 <- est4$fit[1]
variance_r1  <- est4$se.fit[1]^2

param_r1 <- c(
  "\\(p(R_1 = 1)\\)"
)

tab_r1 <- data.frame(
  Parameter = param_r1,
  `Estimate (95% CI)` = make_ci(eta_r1, variance_r1),
  stringsAsFactors = FALSE
)



## ===============================
## Combine + huxtable formatting
## ===============================

## table in long format
dat <- rbind(tab_px1, tab_px2, tab_r1, tab_r2)
names(dat)[names(dat) == "Estimate..95..CI."] <- "Estimate (95\\% CI)"

table1 <- as_hux(dat) %>%
  set_align(col = 1, everywhere, "left") %>%
  set_align(col = 2, everywhere, "left") %>%
  set_bold(1, everywhere) %>%
  set_font_size(7) %>%
  set_all_padding(-1) %>%
  set_left_padding(col = 2, everywhere, 10) %>%
  set_escape_contents(everywhere, everywhere, FALSE) %>%
  set_top_border(row = 1, col = everywhere, brdr(0.8, "solid")) %>%
  set_bottom_border(row = 1, col = everywhere, brdr(0.4, "solid")) %>%
  set_bottom_border(row = nrow(.), col = everywhere, brdr(0.8, "solid")) %>%
  set_caption("Parameter estimates with 95\\% confidence intervals.")

table1

## table in wide format
dat <- rbind(t(tab_px1), t(tab_px2[1:3,]),t(tab_px2[4:6,]),t(tab_px2[7:9,]), cbind(t(tab_r1),c('',''),c('','')), t(tab_r2))
dat[1,] <- c("\\(x_1=1\\)","\\(x_1=2\\)","\\(x_1=3\\)") # header for p(X1)
dat[3,] <- c("\\(x_2=1\\)","\\(x_2=2\\)","\\(x_2=3\\)") # header for p(X2|X1=1)
dat[9,] <- c("\\(r_1=1\\)","","") # header for p(R1=1)
dat[11,] <- c("\\(x_1=1\\)","\\(x_1=2\\)","\\(x_1=3\\)") # header for p(R2=1|X1,R1=1)
dat <- dat[-c(5,7),] # remove header rows for p(X2|X1=2) and p(X2|X1=3)

table1 <- as_hux(dat) %>%
  insert_column(c("\\(p(X_1=x_1)\\)","",
                  "\\(p(X_2=x_2 \\mid X_1=x_1)\\)","\\(x_1=1\\)","\\(x_1=2\\)","\\(x_1=3\\)",
                  "\\(p(R_1=r_1)\\)","",
                  "\\(p(R_2=1 \\mid X_1=x_1, R_1=1)\\)",""), after = 0) %>%
  insert_row(c("Parameter","Estimate (95\\% CI)","",""), after = 0) %>%
  # merge estimates
  merge_cells(1, 2:4) %>%
  set_align(col = 1, everywhere, "left") %>%
  set_align(col = 2, everywhere, "center") %>%
  # merge parameters
  # merge_cells(2:3, 1) %>% 
  # merge_cells(8:9, 1) %>%
  # merge_cells(10:11, 1) %>%
  # set_valign(c(2,8,10), col=1,"middle") %>%
  set_bold(1, everywhere) %>%
  set_font_size(7) %>%
  set_all_padding(-2) %>%
  # set_left_padding(col = 2, everywhere, 10) %>%
  set_escape_contents(everywhere, everywhere, FALSE) %>%
  set_top_border(row = 1, col = everywhere, brdr(0.8, "solid")) %>%
  set_bottom_border(row = 1, col = everywhere, brdr(0.4, "solid")) %>%
  # set_top_border(row = c(3,7,9), col = everywhere, brdr(0.4, "dashed")) %>%
  set_bottom_border(row = nrow(.), col = everywhere, brdr(0.8, "solid")) %>%
  set_caption("Parameter estimates with 95\\% confidence intervals.")

## export
quick_latex(table1, file = "table.tex")

