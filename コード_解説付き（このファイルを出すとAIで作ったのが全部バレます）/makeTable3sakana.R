# =========================================================
# ※ このファイルは SakanaAI が作成したコードです（makeTable3sakana.R）
# =========================================================
# Table 3 の再現
#
# 目的変数：PCS（身体的QOL）と MCS（精神的QOL）＝どちらも連続変数
# 説明変数：属性・臨床のカテゴリ変数
# 各カテゴリごとに「人数(N)・平均・95%信頼区間」を出し、群間差の検定を行う。
#
# 検定の選び方（課題1の考え方をそのままコードにする）：
#   ・2群のカテゴリ変数        → t検定
#   ・順序のない3群以上         → 一元配置分散分析(ANOVA)
#   ・順序のある3群以上         → 傾向性の検定（カテゴリに1,2,3…の点を置いた線形回帰）
# =========================================================

# --- データの読み込み ---
dat <- read.csv("hl_qol.csv", stringsAsFactors = FALSE)

# 空欄 "" を欠損 NA にしておく
dat[dat == ""] <- NA

# （参考）Table 2 と同じく「ヘルスリテラシー欠損」を除いて集計したい場合は
# 次の行のコメントを外す。課題文では除外指示は Table 2 のみなので、
# ここでは全ケースを使う。
# dat <- dat[!is.na(dat$hltcatnew), ]

# カテゴリの並び順を表と同じになるように指定する
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


# --- 平均と95%信頼区間を計算する小さな関数 ---
mean_ci <- function(x) {
  x <- x[!is.na(x)]          # 欠損を取り除く
  n <- length(x)             # 人数
  m <- mean(x)               # 平均
  se <- sd(x) / sqrt(n)      # 標準誤差
  lcl <- m - 1.96 * se       # 信頼区間の下限
  ucl <- m + 1.96 * se       # 信頼区間の上限
  c(n = n, mean = round(m, 1), lcl = round(lcl, 1), ucl = round(ucl, 1))
}

# --- p値を見やすい文字にする小さな関数 ---
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}

# --- 1つの変数について、表の行をまとめて作る関数 ---
# 検定の種類は変数ごとに下で手で書いて、その結果(p値)をここに渡す。
add_variable <- function(result, var, var_label, levs, labs, p_pcs, p_mcs) {
  for (i in seq_along(levs)) {
    # そのカテゴリの人だけ取り出す
    sub <- dat[!is.na(dat[[var]]) & dat[[var]] == levs[i], ]
    pcs <- mean_ci(sub$pcs)
    mcs <- mean_ci(sub$mcs)

    one_row <- data.frame(
      Variable = ifelse(i == 1, var_label, ""),   # 変数名は1行目だけに書く
      Category = labs[i],
      N        = pcs["n"],
      PCS_mean = pcs["mean"],
      PCS_lower = pcs["lcl"],
      PCS_upper = pcs["ucl"],
      PCS_p    = ifelse(i == 1, format_p(p_pcs), ""),
      MCS_mean = mcs["mean"],
      MCS_lower = mcs["lcl"],
      MCS_upper = mcs["ucl"],
      MCS_p    = ifelse(i == 1, format_p(p_mcs), ""),
      stringsAsFactors = FALSE
    )
    result <- rbind(result, one_row)
  }
  result
}


# =========================================================
# 変数を1つずつ処理して表を作る
# （検定は変数ごとに t.test / aov / lm を手で書き分ける）
# =========================================================

table3 <- data.frame()

# --- Gender：2群 → t検定 ---
table3 <- add_variable(table3, "Gender", "Gender",
  c("Male", "Female"), c("Male", "Female"),
  p_pcs = t.test(dat$pcs ~ dat$Gender)$p.value,
  p_mcs = t.test(dat$mcs ~ dat$Gender)$p.value)

# --- Age group：順序のある3群 → 傾向性の検定 ---
table3 <- add_variable(table3, "agegroup3", "Age group",
  c("38 to 69 years", "70 to 79 years", "80 years above"),
  c("<70 years", "70 to 79 years", ">=80 years"),
  p_pcs = summary(lm(dat$pcs ~ as.numeric(dat$agegroup3)))$coefficients[2, 4],
  p_mcs = summary(lm(dat$mcs ~ as.numeric(dat$agegroup3)))$coefficients[2, 4])

# --- Marital status：2群 → t検定 ---
table3 <- add_variable(table3, "ms2g", "Marital status",
  c("married", "unmarried"), c("Married", "Unmarried"),
  p_pcs = t.test(dat$pcs ~ dat$ms2g)$p.value,
  p_mcs = t.test(dat$mcs ~ dat$ms2g)$p.value)

# --- Residence area：2群 → t検定 ---
table3 <- add_variable(table3, "RA06ur", "Residence area",
  c("Urban Area", "Rural Australia"), c("Urban", "Rural"),
  p_pcs = t.test(dat$pcs ~ dat$RA06ur)$p.value,
  p_mcs = t.test(dat$mcs ~ dat$RA06ur)$p.value)

# --- State of general practice：2群 → t検定 ---
table3 <- add_variable(table3, "Stateno", "State of general practice",
  c("SA practice", "QLD practice"), c("South Australia", "Queensland"),
  p_pcs = t.test(dat$pcs ~ dat$Stateno)$p.value,
  p_mcs = t.test(dat$mcs ~ dat$Stateno)$p.value)

# --- Attained educational level：順序のある4群 → 傾向性の検定 ---
table3 <- add_variable(table3, "educatn4g", "Attained educational level",
  c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"),
  c("Degree or higher", "Certificate to advanced diploma", "Secondary", "No schooling to primary"),
  p_pcs = summary(lm(dat$pcs ~ as.numeric(dat$educatn4g)))$coefficients[2, 4],
  p_mcs = summary(lm(dat$mcs ~ as.numeric(dat$educatn4g)))$coefficients[2, 4])

# --- Working status：順序のない3群 → ANOVA ---
table3 <- add_variable(table3, "workst3g", "Working status",
  c("employed", "retired", "pensioner"),
  c("Employed full or part time", "Retired", "Pensioner"),
  p_pcs = summary(aov(dat$pcs ~ dat$workst3g))[[1]][["Pr(>F)"]][1],
  p_mcs = summary(aov(dat$mcs ~ dat$workst3g))[[1]][["Pr(>F)"]][1])

# --- Perceived economic situation：順序のある3群 → 傾向性の検定 ---
table3 <- add_variable(table3, "econ3g", "Perceived economic situation",
  c("very high to higher", "the same", "lower to very low"),
  c("Very high to higher", "Similar to other people", "Lower to very low"),
  p_pcs = summary(lm(dat$pcs ~ as.numeric(dat$econ3g)))$coefficients[2, 4],
  p_mcs = summary(lm(dat$mcs ~ as.numeric(dat$econ3g)))$coefficients[2, 4])

# --- Economic position (quintiles)：順序のある4群 → 傾向性の検定 ---
table3 <- add_variable(table3, "irsd_quint", "Economic position (quintiles)",
  c("highest", "high", "middle", "low"),
  c("Highest", "High", "Middle", "Low"),
  p_pcs = summary(lm(dat$pcs ~ as.numeric(dat$irsd_quint)))$coefficients[2, 4],
  p_mcs = summary(lm(dat$mcs ~ as.numeric(dat$irsd_quint)))$coefficients[2, 4])

# --- History of revascularization procedure：2群 → t検定 ---
table3 <- add_variable(table3, "ischprocedure", "History of revascularization procedure",
  c("no", "yes"), c("No", "Yes"),
  p_pcs = t.test(dat$pcs ~ dat$ischprocedure)$p.value,
  p_mcs = t.test(dat$mcs ~ dat$ischprocedure)$p.value)

# --- Time since first ischaemic episode：順序のある3群 → 傾向性の検定 ---
table3 <- add_variable(table3, "yearsIHD3g", "Time since first ischaemic episode",
  c("0-5 years", "6-10 years", ">10 years"),
  c("Up to 5 years", "6-10 years", "10+ years"),
  p_pcs = summary(lm(dat$pcs ~ as.numeric(dat$yearsIHD3g)))$coefficients[2, 4],
  p_mcs = summary(lm(dat$mcs ~ as.numeric(dat$yearsIHD3g)))$coefficients[2, 4])

# --- Number of other CVD conditions：順序のある3群 → 傾向性の検定 ---
table3 <- add_variable(table3, "CVDcomorb3g", "Number of other CVD conditions",
  c("none", "just 1", "2 or more"), c("0", "1", "2+"),
  p_pcs = summary(lm(dat$pcs ~ as.numeric(dat$CVDcomorb3g)))$coefficients[2, 4],
  p_mcs = summary(lm(dat$mcs ~ as.numeric(dat$CVDcomorb3g)))$coefficients[2, 4])

# --- Number of clinical CVD risk factors：順序のある3群 → 傾向性の検定 ---
table3 <- add_variable(table3, "CVDriskf3g", "Number of clinical CVD risk factors",
  c("0-1 risk factors", "2 risk factors", "3 or more"), c("0-1", "2", "3+"),
  p_pcs = summary(lm(dat$pcs ~ as.numeric(dat$CVDriskf3g)))$coefficients[2, 4],
  p_mcs = summary(lm(dat$mcs ~ as.numeric(dat$CVDriskf3g)))$coefficients[2, 4])

# 行番号をきれいにする
rownames(table3) <- NULL


# =========================================================
# 結果を表示して保存する
# =========================================================

print(table3)

dir.create("outputs", showWarnings = FALSE)
write.csv(table3, "outputs/table3_result.csv", row.names = FALSE, fileEncoding = "UTF-8")

# RStudio で開いている場合は表としても確認できる
if (interactive()) View(table3)
