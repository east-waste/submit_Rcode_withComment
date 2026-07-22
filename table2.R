#データを読み込む　
#空欄は欠損値NAにする　
dat <- read.csv("hl_qol.csv", stringsAsFactors = FALSE)
dat[dat == ""] <- NA

#hltcatnewが欠損の人を除外する
dat <- dat[!is.na(dat$hltcatnew), ]

#hltcatnewがinadequateなら１，それ以外なら０
dat$inadequate <- 0
dat$inadequate[dat$hltcatnew == "inadequate"] <- 1

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

#論文と同じように、p値を少数第三位までの出力にそろえる　0.001より小さい時は <0.001 と出力する
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}

#1つの変数について表の行をまとめて作る関数
#forループの中で全てのカテゴリについて回している
add_variable <- function(result, var, var_label, levs, labs, p_value) {
  for (i in seq_along(levs)) {
    sub <- dat[!is.na(dat[[var]]) & dat[[var]] == levs[i], ]  #そのカテゴリの人だけ取り出す
    n <- nrow(sub)                    #そのカテゴリの人数
    percent <- n / nrow(dat) * 100    #そのカテゴリが全体に占める割合
    x <- sum(sub$inadequate == 1)     #inadequateの人数
    prevalence <- x / n * 100         #その割合
    ci <- prop.test(x, n, correct = FALSE)$conf.int * 100   #95%信頼区間を計算
    
    #データの列を定義(１行分)
    one_row <- data.frame(
      Variable = ifelse(i == 1, var_label, ""),
      Category = labs[i],
      N = n,
      Percent = round(percent, 1),
      Prevalence = round(prevalence, 1),
      Lower = round(ci[1], 1),    #95%信頼区間の下限値を小数第一位まで
      Upper = round(ci[2], 1),    #95%信頼区間の上限値を小数第一位まで
      P_value = ifelse(i == 1, format_p(p_value), "")   #format_pでP値を整形
    )
    #作成した一行を結果の表に追加する
    result <- rbind(result, one_row)
  }
  #完成した表を返す
  result
}

table2 <- data.frame()

#性別：カイ二乗検定（chisq.test）の結果をadd_variable関数によって表に追加する
tab <- table(dat$Gender, dat$inadequate)
table2 <- add_variable(table2, "Gender", "Gender", c("Male", "Female"), c("Male", "Female"), chisq.test(tab)$p.value)

#年齢：傾向検定（prop.ternd.test）
tab <- table(dat$agegroup3, dat$inadequate)
table2 <- add_variable(table2, "agegroup3", "Age group", c("38 to 69 years", "70 to 79 years", "80 years above"), c("<70 years", "70 to 79 years", ">=80 years"), prop.trend.test(tab[, "1"], rowSums(tab))$p.value)

#結婚しているかどうか：カイ二乗検定
tab <- table(dat$ms2g, dat$inadequate)
table2 <- add_variable(table2, "ms2g", "Marital status", c("married", "unmarried"), c("Married", "Unmarried"), chisq.test(tab)$p.value)

#住んでいる場所：カイ二乗検定
tab <- table(dat$RA06ur, dat$inadequate)
table2 <- add_variable(table2, "RA06ur", "Residence area", c("Urban Area", "Rural Australia"), c("Urban", "Rural"), chisq.test(tab)$p.value)

#診療所がある場所：カイ二乗検定
tab <- table(dat$Stateno, dat$inadequate)
table2 <- add_variable(table2, "Stateno", "State of general practice", c("SA practice", "QLD practice"), c("South Australia", "Queensland"), chisq.test(tab)$p.value)

#学歴：傾向検定
tab <- table(dat$educatn4g, dat$inadequate)
table2 <- add_variable(table2, "educatn4g", "Attained educational level", c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"), c("Degree or higher", "Certificate to advanced diploma", "Secondary", "No schooling to primary"), prop.trend.test(tab[, "1"], rowSums(tab))$p.value)

#働いているかどうか：カイ二乗検定
tab <- table(dat$workst3g, dat$inadequate)
table2 <- add_variable(table2, "workst3g", "Working status", c("employed", "retired", "pensioner"), c("Employed full or part time", "Retired", "Pensioner"), chisq.test(tab)$p.value)

#経済状況：傾向検定
tab <- table(dat$econ3g, dat$inadequate)
table2 <- add_variable(table2, "econ3g", "Perceived economic situation", c("very high to higher", "the same", "lower to very low"), c("Very high to higher", "Similar to other people", "Lower to very low"), prop.trend.test(tab[, "1"], rowSums(tab))$p.value)

#地域の経済状況：傾向検定
tab <- table(dat$irsd_quint, dat$inadequate)
table2 <- add_variable(table2, "irsd_quint", "Economic position (quintiles)", c("highest", "high", "middle", "low"), c("Highest", "High", "Middle", "Low"), prop.trend.test(tab[, "1"], rowSums(tab))$p.value)

#手術歴：カイ二乗検定
tab <- table(dat$ischprocedure, dat$inadequate)
table2 <- add_variable(table2, "ischprocedure", "History of revascularization procedure", c("no", "yes"), c("No", "Yes"), chisq.test(tab)$p.value)

#病気になってからの年数：傾向検定
tab <- table(dat$yearsIHD3g, dat$inadequate)
table2 <- add_variable(table2, "yearsIHD3g", "Time since first ischaemic episode", c("0-5 years", "6-10 years", ">10 years"), c("Up to 5 years", "6-10 years", "10+ years"), prop.trend.test(tab[, "1"], rowSums(tab))$p.value)

#他の心血管疾患の数：傾向検定
tab <- table(dat$CVDcomorb3g, dat$inadequate)
table2 <- add_variable(table2, "CVDcomorb3g", "Number of other CVD conditions", c("none", "just 1", "2 or more"), c("0", "1", "2+"), prop.trend.test(tab[, "1"], rowSums(tab))$p.value)

#リスク因子数：傾向検定
tab <- table(dat$CVDriskf3g, dat$inadequate)
table2 <- add_variable(table2, "CVDriskf3g", "Number of clinical CVD risk factors", c("0-1 risk factors", "2 risk factors", "3 or more"), c("0-1", "2", "3+"), prop.trend.test(tab[, "1"], rowSums(tab))$p.value)

#table2を表示・CSVファイルで保存
print(table2)
write.csv(table2, "table2_result.csv", row.names = FALSE)
