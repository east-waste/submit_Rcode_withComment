#データを読み込む　
#空欄は欠損値NAにする　
dat <- read.csv("hl_qol.csv", stringsAsFactors = FALSE)
dat[dat == ""] <- NA

#hltcatnewが欠損の人を除外する
dat <- dat[!is.na(dat$hltcatnew), ]

#カテゴリ順、基準カテゴリ、傾向検定用スコア、年齢の二乗項を設定する
dat$hltcatnew <- factor(dat$hltcatnew, levels = c("inadequate", "marginal", "adequate"))
dat$hltcatnew_ref <- factor(dat$hltcatnew, levels = c("adequate", "inadequate", "marginal"))
dat$hl_score <- as.numeric(dat$hltcatnew)
dat$age2 <- dat$age^2

#表の中の変数の並び順を指定する
dat$Gender <- factor(dat$Gender, levels = c("Male", "Female"))
dat$ms2g <- factor(dat$ms2g, levels = c("married", "unmarried"))
dat$educatn4g <- factor(dat$educatn4g, levels = c("post grad/bachelor", "advdipl/dipl/certif/trade", "seniorsec/secondary", "no schooling/primary"))
dat$workst3g <- factor(dat$workst3g, levels = c("employed", "retired", "pensioner"))
dat$econ3g <- factor(dat$econ3g, levels = c("very high to higher", "the same", "lower to very low"))
dat$irsd_quint <- factor(dat$irsd_quint, levels = c("highest", "high", "middle", "low"))
dat$ischprocedure <- factor(dat$ischprocedure, levels = c("no", "yes"))
dat$CVDcomorb3g <- factor(dat$CVDcomorb3g, levels = c("none", "just 1", "2 or more"))
dat$CVDriskf3g <- factor(dat$CVDriskf3g, levels = c("0-1 risk factors", "2 risk factors", "3 or more"))

#論文と同じように、p値を少数第三位までの出力にそろえる　0.001より小さい時は <0.001 と出力する
format_p <- function(p) {
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}

#平均と95%信頼区間を計算する
mean_ci <- function(x) {
  x <- x[!is.na(x)]     #欠損を取り除く
  n <- length(x)        #人数
  m <- mean(x)          #平均
  se <- sd(x) / sqrt(n) #標準偏差
  lcl <- m - 1.96 * se  #信頼区間の下限
  ucl <- m + 1.96 * se  #信頼区間の上限
  c(estimate = round(m, 1), lower = round(lcl, 1), upper = round(ucl, 1))
}

#回帰係数と95％信頼区間を取り出す
beta_ci <- function(model, term) {
  est <- summary(model)$coefficients
  ci <- confint(model)
  c(estimate = round(est[term, "Estimate"], 1), lower = round(ci[term, 1], 1), upper = round(ci[term, 2], 1))
}

#行を作る関数(これを各カテゴリについてforループで回す)
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
    P_value = format_p(p_value)
  )
}

#PCSについて、未調整・傾向検定・調整済みの回帰分析を行う
pcs_crude <- lm(pcs ~ hltcatnew_ref, data = dat)
pcs_trend <- lm(pcs ~ hl_score, data = dat)
pcs_adj <- lm(pcs ~ hltcatnew_ref + Gender + age + age2 + ms2g + educatn4g + workst3g + econ3g + irsd_quint + ischprocedure + CVDcomorb3g + CVDriskf3g, data = dat)
pcs_adj_trend <- lm(pcs ~ hl_score + Gender + age + age2 + ms2g + educatn4g + workst3g + econ3g + irsd_quint + ischprocedure + CVDcomorb3g + CVDriskf3g, data = dat)

#MCSについて、未調整・傾向検定・調整済みの回帰分析を行う
mcs_crude <- lm(mcs ~ hltcatnew_ref, data = dat)
mcs_trend <- lm(mcs ~ hl_score, data = dat)
mcs_adj <- lm(mcs ~ hltcatnew_ref + Gender + age + age2 + ms2g + educatn4g + econ3g + irsd_quint, data = dat)
mcs_adj_trend <- lm(mcs ~ hl_score + Gender + age + age2 + ms2g + educatn4g + econ3g + irsd_quint, data = dat)

table4 <- data.frame()

#PCSの平均値を表に追加
table4 <- rbind(table4, make_row("Physical component", "Mean", mean_ci(dat$pcs[dat$hltcatnew == "inadequate"]), mean_ci(dat$pcs[dat$hltcatnew == "marginal"]), mean_ci(dat$pcs[dat$hltcatnew == "adequate"]), summary(pcs_trend)$coefficients["hl_score", "Pr(>|t|)"]))

#PCSの未調整の回帰係数を表に追加
table4 <- rbind(table4, make_row("", "β crude", beta_ci(pcs_crude, "hltcatnew_refinadequate"), beta_ci(pcs_crude, "hltcatnew_refmarginal"), c(estimate = NA, lower = NA, upper = NA), summary(pcs_trend)$coefficients["hl_score", "Pr(>|t|)"]))

#PCSの調整済み回帰係数を表に追加
table4 <- rbind(table4, make_row("", "β adjusted", beta_ci(pcs_adj, "hltcatnew_refinadequate"), beta_ci(pcs_adj, "hltcatnew_refmarginal"), c(estimate = NA, lower = NA, upper = NA), summary(pcs_adj_trend)$coefficients["hl_score", "Pr(>|t|)"]))

#MCSの平均値を表に追加
table4 <- rbind(table4, make_row("Mental component", "Mean", mean_ci(dat$mcs[dat$hltcatnew == "inadequate"]), mean_ci(dat$mcs[dat$hltcatnew == "marginal"]), mean_ci(dat$mcs[dat$hltcatnew == "adequate"]), summary(mcs_trend)$coefficients["hl_score", "Pr(>|t|)"]))

#MCSの未調整の回帰係数を表に追加
table4 <- rbind(table4, make_row("", "β crude", beta_ci(mcs_crude, "hltcatnew_refinadequate"), beta_ci(mcs_crude, "hltcatnew_refmarginal"), c(estimate = NA, lower = NA, upper = NA), summary(mcs_trend)$coefficients["hl_score", "Pr(>|t|)"]))

#MCSの調整済み回帰係数を表に追加
table4 <- rbind(table4, make_row("", "β adjusted", beta_ci(mcs_adj, "hltcatnew_refinadequate"), beta_ci(mcs_adj, "hltcatnew_refmarginal"), c(estimate = NA, lower = NA, upper = NA), summary(mcs_adj_trend)$coefficients["hl_score", "Pr(>|t|)"]))

#AdequateのところにはRefと表示する
table4$Adequate[table4$Category != "Mean"] <- "Ref"

#Adewuateの信頼区間を空欄にする
table4$Adequate_lower[table4$Category != "Mean"] <- ""
table4$Adequate_upper[table4$Category != "Mean"] <- ""

#table4を表示・CSVファイルで保存
print(table4)
write.csv(table4, "table4_result.csv", row.names = FALSE)
