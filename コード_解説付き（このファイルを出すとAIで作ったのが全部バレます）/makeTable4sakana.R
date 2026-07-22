# =========================================================
# ※ このファイルは SakanaAI が作成したコードです（makeTable4sakana.R）
# =========================================================
# Table 4 の再現
#
# 論文のTable 4に近い形で、Physical component と Mental component について、
#   Mean
#   β crude
#   β adjusted
# を出す。
#
# ただし、95%信頼区間は1つのセルにまとめず、
# Inadequate, Marginal, Adequate それぞれについて
# Estimate / Lower / Upper を別々の列に分ける。
# =========================================================


# --- データの読み込み ---
dat <- read.csv("hl_qol.csv", stringsAsFactors = FALSE)

# 空欄 "" を欠損 NA にする
dat[dat == ""] <- NA

# Table 4ではヘルスリテラシーが欠損の人を除外する
dat <- dat[!is.na(dat$hltcatnew), ]


# --- カテゴリの順番を指定する ---
dat$hltcatnew <- factor(dat$hltcatnew, levels = c("inadequate", "marginal", "adequate"))
dat$hltcatnew_ref <- factor(dat$hltcatnew, levels = c("adequate", "inadequate", "marginal"))

# 傾向性のp値用に、カテゴリに点数を置いた変数を作る
dat$hl_score <- as.numeric(dat$hltcatnew)

# 年齢の2乗を作る
dat$age2 <- dat$age^2


# --- 他のカテゴリ変数も並び順（基準カテゴリ）を指定する ---
dat$Gender        <- factor(dat$Gender,        levels = c("Male", "Female"))
dat$ms2g          <- factor(dat$ms2g,          levels = c("married", "unmarried"))
dat$educatn4g     <- factor(dat$educatn4g,     levels = c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"))
dat$workst3g      <- factor(dat$workst3g,      levels = c("employed", "retired", "pensioner"))
dat$econ3g        <- factor(dat$econ3g,        levels = c("very high to higher", "the same", "lower to very low"))
dat$irsd_quint    <- factor(dat$irsd_quint,    levels = c("highest", "high", "middle", "low"))
dat$ischprocedure <- factor(dat$ischprocedure, levels = c("no", "yes"))
dat$CVDcomorb3g   <- factor(dat$CVDcomorb3g,   levels = c("none", "just 1", "2 or more"))
dat$CVDriskf3g    <- factor(dat$CVDriskf3g,    levels = c("0-1 risk factors", "2 risk factors", "3 or more"))


# --- p値を見やすい文字にする小さな関数 ---
format_p <- function(p) {
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}


# --- 平均と95%信頼区間を計算する関数 ---
mean_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  lcl <- m - 1.96 * se
  ucl <- m + 1.96 * se
  c(estimate = round(m, 1), lower = round(lcl, 1), upper = round(ucl, 1))
}


# --- 回帰係数と95%信頼区間を取り出す関数 ---
beta_ci <- function(model, term) {
  est <- summary(model)$coefficients
  ci <- confint(model)
  c(estimate = round(est[term, "Estimate"], 1),
    lower = round(ci[term, 1], 1),
    upper = round(ci[term, 2], 1))
}


# --- 1行を作る関数 ---
make_row <- function(variable, category, inad, marg, adeq, p_value) {
  data.frame(
    Variable = variable,
    Category = category,
    Inadequate = inad["estimate"],
    Inadequate_lower = inad["lower"],
    Inadequate_upper = inad["upper"],
    Marginal = marg["estimate"],
    Marginal_lower = marg["lower"],
    Marginal_upper = marg["upper"],
    Adequate = adeq["estimate"],
    Adequate_lower = adeq["lower"],
    Adequate_upper = adeq["upper"],
    P_value = format_p(p_value),
    stringsAsFactors = FALSE
  )
}


# =========================================================
# モデルを作る
# =========================================================

pcs_crude <- lm(pcs ~ hltcatnew_ref, data = dat)
pcs_trend <- lm(pcs ~ hl_score, data = dat)
pcs_adj <- lm(pcs ~ hltcatnew_ref + Gender + age + age2 + ms2g + educatn4g +
                workst3g + econ3g + irsd_quint + ischprocedure +
                CVDcomorb3g + CVDriskf3g,
              data = dat)
pcs_adj_trend <- lm(pcs ~ hl_score + Gender + age + age2 + ms2g + educatn4g +
                      workst3g + econ3g + irsd_quint + ischprocedure +
                      CVDcomorb3g + CVDriskf3g,
                    data = dat)

mcs_crude <- lm(mcs ~ hltcatnew_ref, data = dat)
mcs_trend <- lm(mcs ~ hl_score, data = dat)
mcs_adj <- lm(mcs ~ hltcatnew_ref + Gender + age + age2 + ms2g + educatn4g +
                econ3g + irsd_quint,
              data = dat)
mcs_adj_trend <- lm(mcs ~ hl_score + Gender + age + age2 + ms2g + educatn4g +
                      econ3g + irsd_quint,
                    data = dat)


# =========================================================
# 表を作る
# =========================================================

table4 <- data.frame()

table4 <- rbind(table4, make_row(
  "Physical component", "Mean",
  mean_ci(dat$pcs[dat$hltcatnew == "inadequate"]),
  mean_ci(dat$pcs[dat$hltcatnew == "marginal"]),
  mean_ci(dat$pcs[dat$hltcatnew == "adequate"]),
  summary(pcs_trend)$coefficients["hl_score", "Pr(>|t|)"]
))

table4 <- rbind(table4, make_row(
  "", "β crude",
  beta_ci(pcs_crude, "hltcatnew_refinadequate"),
  beta_ci(pcs_crude, "hltcatnew_refmarginal"),
  c(estimate = NA, lower = NA, upper = NA),
  summary(pcs_trend)$coefficients["hl_score", "Pr(>|t|)"]
))

table4 <- rbind(table4, make_row(
  "", "β adjusted",
  beta_ci(pcs_adj, "hltcatnew_refinadequate"),
  beta_ci(pcs_adj, "hltcatnew_refmarginal"),
  c(estimate = NA, lower = NA, upper = NA),
  summary(pcs_adj_trend)$coefficients["hl_score", "Pr(>|t|)"]
))

table4 <- rbind(table4, make_row(
  "Mental component", "Mean",
  mean_ci(dat$mcs[dat$hltcatnew == "inadequate"]),
  mean_ci(dat$mcs[dat$hltcatnew == "marginal"]),
  mean_ci(dat$mcs[dat$hltcatnew == "adequate"]),
  summary(mcs_trend)$coefficients["hl_score", "Pr(>|t|)"]
))

table4 <- rbind(table4, make_row(
  "", "β crude",
  beta_ci(mcs_crude, "hltcatnew_refinadequate"),
  beta_ci(mcs_crude, "hltcatnew_refmarginal"),
  c(estimate = NA, lower = NA, upper = NA),
  summary(mcs_trend)$coefficients["hl_score", "Pr(>|t|)"]
))

table4 <- rbind(table4, make_row(
  "", "β adjusted",
  beta_ci(mcs_adj, "hltcatnew_refinadequate"),
  beta_ci(mcs_adj, "hltcatnew_refmarginal"),
  c(estimate = NA, lower = NA, upper = NA),
  summary(mcs_adj_trend)$coefficients["hl_score", "Pr(>|t|)"]
))

table4$Adequate[table4$Category != "Mean"] <- "Ref"
table4$Adequate_lower[table4$Category != "Mean"] <- ""
table4$Adequate_upper[table4$Category != "Mean"] <- ""

rownames(table4) <- NULL


# =========================================================
# 結果を表示して保存する
# =========================================================

print(table4)

dir.create("outputs", showWarnings = FALSE)
write.csv(table4, "outputs/table4_result.csv", row.names = FALSE, fileEncoding = "UTF-8")

if (interactive()) View(table4)
