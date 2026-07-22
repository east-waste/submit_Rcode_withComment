# =========================================================
# ※ このファイルは SakanaAI が作成したコードです（makeTable2sakana.R）
# =========================================================
# Table 2 の再現
#
# 目的変数：inadequate health literacy かどうか
# 説明変数：属性・臨床のカテゴリ変数
#
# 各カテゴリごとに
#   ・人数 N
#   ・全体に占める割合 Percent
#   ・inadequate health literacy の割合 Prevalence
#   ・その95%信頼区間 Lower / Upper
#   ・p値
# を出す。
#
# 検定の選び方：
#   ・順序のないカテゴリ変数 → カイ二乗検定
#   ・順序のあるカテゴリ変数 → 傾向性の検定
# =========================================================


# --- データの読み込み ---
dat <- read.csv("hl_qol.csv", stringsAsFactors = FALSE)

# 空欄 "" を欠損 NA にする
dat[dat == ""] <- NA

# Table 2 は、課題文の指示どおり
# ヘルスリテラシー hltcatnew が欠損の人を除外する
dat <- dat[!is.na(dat$hltcatnew), ]

# inadequate health literacy かどうかの変数を作る
# inadequate なら1、それ以外（adequate, marginal）は0
dat$inadequate <- 0
dat$inadequate[dat$hltcatnew == "inadequate"] <- 1


# --- カテゴリの並び順を表と同じになるように指定する ---
dat$Gender        <- factor(dat$Gender,        levels = c("Male", "Female"))
dat$agegroup3     <- factor(dat$agegroup3,     levels = c("38 to 69 years", "70 to 79 years", "80 years above"))
dat$ms2g          <- factor(dat$ms2g,          levels = c("married", "unmarried"))
dat$RA06ur        <- factor(dat$RA06ur,        levels = c("Urban Area", "Rural Australia"))
dat$Stateno       <- factor(dat$Stateno,       levels = c("SA practice", "QLD practice"))
dat$educatn4g     <- factor(dat$educatn4g,     levels = c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"))
dat$workst3g      <- factor(dat$workst3g,      levels = c("employed", "retired", "pensioner"))
dat$econ3g        <- factor(dat$econ3g,        levels = c("very high to higher", "the same", "lower to very low"))
dat$irsd_quint    <- factor(dat$irsd_quint,    levels = c("highest", "high", "middle", "low"))
dat$ischprocedure <- factor(dat$ischprocedure, levels = c("no", "yes"))
dat$yearsIHD3g    <- factor(dat$yearsIHD3g,    levels = c("0-5 years", "6-10 years", ">10 years"))
dat$CVDcomorb3g   <- factor(dat$CVDcomorb3g,   levels = c("none", "just 1", "2 or more"))
dat$CVDriskf3g    <- factor(dat$CVDriskf3g,    levels = c("0-1 risk factors", "2 risk factors", "3 or more"))


# --- p値を見やすい文字にする小さな関数 ---
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}


# --- 1つの変数について、表の行をまとめて作る関数 ---
# Table 3 と同じように、
# Variable は1行目だけ、Category は各カテゴリ名を書く。
add_variable <- function(result, var, var_label, levs, labs, p_value) {
  for (i in seq_along(levs)) {
    
    # そのカテゴリの人だけ取り出す
    sub <- dat[!is.na(dat[[var]]) & dat[[var]] == levs[i], ]
    
    # そのカテゴリの人数
    n <- nrow(sub)
    
    # そのカテゴリが全体に占める割合
    percent <- n / nrow(dat) * 100
    
    # inadequate health literacy の人数
    x <- sum(sub$inadequate == 1)
    
    # inadequate health literacy の割合
    prevalence <- x / n * 100
    
    # 95%信頼区間
    ci <- prop.test(x, n, correct = FALSE)$conf.int * 100
    
    one_row <- data.frame(
      Variable   = ifelse(i == 1, var_label, ""),
      Category   = labs[i],
      N          = n,
      Percent    = round(percent, 1),
      Prevalence = round(prevalence, 1),
      Lower      = round(ci[1], 1),
      Upper      = round(ci[2], 1),
      P_value    = ifelse(i == 1, format_p(p_value), ""),
      stringsAsFactors = FALSE
    )
    
    result <- rbind(result, one_row)
  }
  result
}


# =========================================================
# 変数を1つずつ処理して表を作る
# （検定は変数ごとにカイ二乗検定または傾向性の検定を手で書く）
# =========================================================

table2 <- data.frame()


# --- Gender：順序のない2群 → カイ二乗検定 ---
tab <- table(dat$Gender, dat$inadequate)
table2 <- add_variable(table2, "Gender", "Gender",
  c("Male", "Female"), c("Male", "Female"),
  p_value = chisq.test(tab)$p.value)


# --- Age group：順序のある3群 → 傾向性の検定 ---
tab <- table(dat$agegroup3, dat$inadequate)
table2 <- add_variable(table2, "agegroup3", "Age group",
  c("38 to 69 years", "70 to 79 years", "80 years above"),
  c("<70 years", "70 to 79 years", ">=80 years"),
  p_value = prop.trend.test(tab[, "1"], rowSums(tab))$p.value)


# --- Marital status：順序のない2群 → カイ二乗検定 ---
tab <- table(dat$ms2g, dat$inadequate)
table2 <- add_variable(table2, "ms2g", "Marital status",
  c("married", "unmarried"), c("Married", "Unmarried"),
  p_value = chisq.test(tab)$p.value)


# --- Residence area：順序のない2群 → カイ二乗検定 ---
tab <- table(dat$RA06ur, dat$inadequate)
table2 <- add_variable(table2, "RA06ur", "Residence area",
  c("Urban Area", "Rural Australia"), c("Urban", "Rural"),
  p_value = chisq.test(tab)$p.value)


# --- State of general practice：順序のない2群 → カイ二乗検定 ---
tab <- table(dat$Stateno, dat$inadequate)
table2 <- add_variable(table2, "Stateno", "State of general practice",
  c("SA practice", "QLD practice"), c("South Australia", "Queensland"),
  p_value = chisq.test(tab)$p.value)


# --- Attained educational level：順序のある4群 → 傾向性の検定 ---
tab <- table(dat$educatn4g, dat$inadequate)
table2 <- add_variable(table2, "educatn4g", "Attained educational level",
  c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"),
  c("Degree or higher", "Certificate to advanced diploma", "Secondary", "No schooling to primary"),
  p_value = prop.trend.test(tab[, "1"], rowSums(tab))$p.value)


# --- Working status：順序のない3群 → カイ二乗検定 ---
tab <- table(dat$workst3g, dat$inadequate)
table2 <- add_variable(table2, "workst3g", "Working status",
  c("employed", "retired", "pensioner"),
  c("Employed full or part time", "Retired", "Pensioner"),
  p_value = chisq.test(tab)$p.value)


# --- Perceived economic situation：順序のある3群 → 傾向性の検定 ---
tab <- table(dat$econ3g, dat$inadequate)
table2 <- add_variable(table2, "econ3g", "Perceived economic situation",
  c("very high to higher", "the same", "lower to very low"),
  c("Very high to higher", "Similar to other people", "Lower to very low"),
  p_value = prop.trend.test(tab[, "1"], rowSums(tab))$p.value)


# --- Economic position (quintiles)：順序のある4群 → 傾向性の検定 ---
tab <- table(dat$irsd_quint, dat$inadequate)
table2 <- add_variable(table2, "irsd_quint", "Economic position (quintiles)",
  c("highest", "high", "middle", "low"),
  c("Highest", "High", "Middle", "Low"),
  p_value = prop.trend.test(tab[, "1"], rowSums(tab))$p.value)


# --- History of revascularization procedure：順序のない2群 → カイ二乗検定 ---
tab <- table(dat$ischprocedure, dat$inadequate)
table2 <- add_variable(table2, "ischprocedure", "History of revascularization procedure",
  c("no", "yes"), c("No", "Yes"),
  p_value = chisq.test(tab)$p.value)


# --- Time since first ischaemic episode：順序のある3群 → 傾向性の検定 ---
tab <- table(dat$yearsIHD3g, dat$inadequate)
table2 <- add_variable(table2, "yearsIHD3g", "Time since first ischaemic episode",
  c("0-5 years", "6-10 years", ">10 years"),
  c("Up to 5 years", "6-10 years", "10+ years"),
  p_value = prop.trend.test(tab[, "1"], rowSums(tab))$p.value)


# --- Number of other CVD conditions：順序のある3群 → 傾向性の検定 ---
tab <- table(dat$CVDcomorb3g, dat$inadequate)
table2 <- add_variable(table2, "CVDcomorb3g", "Number of other CVD conditions",
  c("none", "just 1", "2 or more"), c("0", "1", "2+"),
  p_value = prop.trend.test(tab[, "1"], rowSums(tab))$p.value)


# --- Number of clinical CVD risk factors：順序のある3群 → 傾向性の検定 ---
tab <- table(dat$CVDriskf3g, dat$inadequate)
table2 <- add_variable(table2, "CVDriskf3g", "Number of clinical CVD risk factors",
  c("0-1 risk factors", "2 risk factors", "3 or more"), c("0-1", "2", "3+"),
  p_value = prop.trend.test(tab[, "1"], rowSums(tab))$p.value)


# 行番号をきれいにする
rownames(table2) <- NULL


# =========================================================
# 結果を表示して保存する
# =========================================================

print(table2)

dir.create("outputs", showWarnings = FALSE)
write.csv(table2, "outputs/table2_result.csv", row.names = FALSE, fileEncoding = "UTF-8")

# RStudio で開いている場合は表としても確認できる
if (interactive()) View(table2)
