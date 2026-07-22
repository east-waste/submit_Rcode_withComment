#データを読み込む　
#空欄は欠損値NAにする　
dat <- read.csv("hl_qol.csv", stringsAsFactors = FALSE)
dat[dat == ""] <- NA

#表を作る（カテゴリが順序通りになるように）
dat$Gender <- factor(dat$Gender, levels = c("Male", "Female"))
dat$agegroup3 <- factor(dat$agegroup3, levels = c("38 to 69 years", "70 to 79 years", "80 years above"))
dat$ms2g <- factor(dat$ms2g, levels = c("married", "unmarried"))
dat$RA06ur <- factor(dat$RA06ur, levels = c("Urban Area", "Rural Australia"))
dat$Stateno <- factor(dat$Stateno, levels = c("SA practice", "QLD practice"))
dat$educatn4g <- factor(dat$educatn4g, levels = c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"))
dat$workst3g <- factor(dat$workst3g, levels = c("employed", "retired", "pensioner"))
dat$econ3g <- factor(dat$econ3g, levels = c("very high to higher", "the same", "lower to very low"))
dat$irsd_quint <- factor(dat$irsd_quint, levels = c("highest", "high", "middle", "low"))
dat$ischprocedure <- factor(dat$ischprocedure, levels = c("no", "yes"))
dat$yearsIHD3g <- factor(dat$yearsIHD3g, levels = c("0-5 years", "6-10 years", ">10 years"))
dat$CVDcomorb3g <- factor(dat$CVDcomorb3g, levels = c("none", "just 1", "2 or more"))
dat$CVDriskf3g <- factor(dat$CVDriskf3g, levels = c("0-1 risk factors", "2 risk factors", "3 or more"))

#平均と95%信頼区間を計算する
mean_ci <- function(x) {
  x <- x[!is.na(x)]     #欠損を取り除く
  n <- length(x)        #人数
  m <- mean(x)          #平均
  se <- sd(x) / sqrt(n) #標準偏差
  lcl <- m - 1.96 * se  #信頼区間の下限
  ucl <- m + 1.96 * se  #信頼区間の上限
  c(n = n, mean = round(m, 1), lcl = round(lcl, 1), ucl = round(ucl, 1))
}

#論文と同じように、p値を少数第三位までの出力にそろえる　0.001より小さい時は <0.001 と出力する
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}
#1つの変数についてPCS・MCSを表に追加する関数
#forループの中で全てのカテゴリについて回している
add_variable <- function(result, var, var_label, levs, labs, p_pcs, p_mcs) {
  for (i in seq_along(levs)) {
    sub <- dat[!is.na(dat[[var]]) & dat[[var]] == levs[i], ]  #そのカテゴリの人だけ取り出す
    pcs <- mean_ci(sub$pcs)   #PCSの平均値と95％信頼区間を計算
    mcs <- mean_ci(sub$mcs)   #MCSの平均値と95％信頼区間を計算
    
    one_row <- data.frame(
      Variable = ifelse(i == 1, var_label, ""),
      Category = labs[i],
      N = pcs["n"],
      PCS_mean = pcs["mean"],
      PCS_lower = pcs["lcl"],          #PCSの95%信頼区間の下限
      PCS_upper = pcs["ucl"],          #PCSの95%信頼区間の上限
      PCS_p = ifelse(i == 1, format_p(p_pcs), ""),
      MCS_mean = mcs["mean"],
      MCS_lower = mcs["lcl"],           #MCSの95%信頼区間の下限
      MCS_upper = mcs["ucl"],           #MCSの95%信頼区間の上限
      MCS_p = ifelse(i == 1, format_p(p_mcs), "")
    )
    
    result <- rbind(result, one_row)
  }
  result
}

#結果を保存する空の表を作成
table3 <- data.frame()

#性別：t検定
table3 <- add_variable(table3, "Gender", "Gender", c("Male", "Female"), c("Male", "Female"), t.test(dat$pcs ~ dat$Gender)$p.value, t.test(dat$mcs ~ dat$Gender)$p.value)

#年齢：傾向検定
table3 <- add_variable(table3, "agegroup3", "Age group", c("38 to 69 years", "70 to 79 years", "80 years above"), c("<70 years", "70 to 79 years", ">=80 years"), summary(lm(dat$pcs ~ as.numeric(dat$agegroup3)))$coefficients[2, 4], summary(lm(dat$mcs ~ as.numeric(dat$agegroup3)))$coefficients[2, 4])

#結婚しているかどうか：t検定
table3 <- add_variable(table3, "ms2g", "Marital status", c("married", "unmarried"), c("Married", "Unmarried"), t.test(dat$pcs ~ dat$ms2g)$p.value, t.test(dat$mcs ~ dat$ms2g)$p.value)

#住んでる場所：t検定
table3 <- add_variable(table3, "RA06ur", "Residence area", c("Urban Area", "Rural Australia"), c("Urban", "Rural"), t.test(dat$pcs ~ dat$RA06ur)$p.value, t.test(dat$mcs ~ dat$RA06ur)$p.value)

#診療所がある場所：t検定
table3 <- add_variable(table3, "Stateno", "State of general practice", c("SA practice", "QLD practice"), c("South Australia", "Queensland"), t.test(dat$pcs ~ dat$Stateno)$p.value, t.test(dat$mcs ~ dat$Stateno)$p.value)

#学歴：傾向検定
table3 <- add_variable(table3, "educatn4g", "Attained educational level", c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"), c("Degree or higher", "Certificate to advanced diploma", "Secondary", "No schooling to primary"), summary(lm(dat$pcs ~ as.numeric(dat$educatn4g)))$coefficients[2, 4], summary(lm(dat$mcs ~ as.numeric(dat$educatn4g)))$coefficients[2, 4])

#働いているかどうか：ANOVA検定
table3 <- add_variable(table3, "workst3g", "Working status", c("employed", "retired", "pensioner"), c("Employed full or part time", "Retired", "Pensioner"), summary(aov(dat$pcs ~ dat$workst3g))[[1]][["Pr(>F)"]][1], summary(aov(dat$mcs ~ dat$workst3g))[[1]][["Pr(>F)"]][1])

#経済状況：傾向検定
table3 <- add_variable(table3, "econ3g", "Perceived economic situation", c("very high to higher", "the same", "lower to very low"), c("Very high to higher", "Similar to other people", "Lower to very low"), summary(lm(dat$pcs ~ as.numeric(dat$econ3g)))$coefficients[2, 4], summary(lm(dat$mcs ~ as.numeric(dat$econ3g)))$coefficients[2, 4])

#地域の経済状況：傾向検定
table3 <- add_variable(table3, "irsd_quint", "Economic position (quintiles)", c("highest", "high", "middle", "low"), c("Highest", "High", "Middle", "Low"), summary(lm(dat$pcs ~ as.numeric(dat$irsd_quint)))$coefficients[2, 4], summary(lm(dat$mcs ~ as.numeric(dat$irsd_quint)))$coefficients[2, 4])

#手術歴：t検定
table3 <- add_variable(table3, "ischprocedure", "History of revascularization procedure", c("no", "yes"), c("No", "Yes"), t.test(dat$pcs ~ dat$ischprocedure)$p.value, t.test(dat$mcs ~ dat$ischprocedure)$p.value)

#病気になってからの年数：傾向検定
table3 <- add_variable(table3, "yearsIHD3g", "Time since first ischaemic episode", c("0-5 years", "6-10 years", ">10 years"), c("Up to 5 years", "6-10 years", "10+ years"), summary(lm(dat$pcs ~ as.numeric(dat$yearsIHD3g)))$coefficients[2, 4], summary(lm(dat$mcs ~ as.numeric(dat$yearsIHD3g)))$coefficients[2, 4])

#他の心血管疾患の数：傾向検定
table3 <- add_variable(table3, "CVDcomorb3g", "Number of other CVD conditions", c("none", "just 1", "2 or more"), c("0", "1", "2+"), summary(lm(dat$pcs ~ as.numeric(dat$CVDcomorb3g)))$coefficients[2, 4], summary(lm(dat$mcs ~ as.numeric(dat$CVDcomorb3g)))$coefficients[2, 4])

#リスク因子数：傾向検定
table3 <- add_variable(table3, "CVDriskf3g", "Number of clinical CVD risk factors", c("0-1 risk factors", "2 risk factors", "3 or more"), c("0-1", "2", "3+"), summary(lm(dat$pcs ~ as.numeric(dat$CVDriskf3g)))$coefficients[2, 4], summary(lm(dat$mcs ~ as.numeric(dat$CVDriskf3g)))$coefficients[2, 4])

#table3を表示・CSVファイルで保存
print(table3)
write.csv(table3, "table3_result.csv", row.names = FALSE)
