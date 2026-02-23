suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

df <- read_sav("/Users/hosung/AI_Polarization_Pew/data/raw/W119_Dec22/ATP W119.sav")

p  <- as.integer(zap_labels(df$F_PARTY_FINAL))
pl <- as.integer(zap_labels(df$F_PARTYLN_FINAL))
df$party2 <- case_when(
  p == 1                   ~ "Republican",
  p == 2                   ~ "Democrat",
  p %in% c(3,4) & pl == 1 ~ "Republican",
  p %in% c(3,4) & pl == 2 ~ "Democrat",
  TRUE                     ~ NA_character_
)

dpar <- df %>% filter(!is.na(party2))
cat("Partisan N: Rep=", sum(dpar$party2=="Republican"),
    " Dem=", sum(dpar$party2=="Democrat"), "\n\n")

# ── Helper: cross-tab with chi-square ─────────────────────────────────────────
crosstab_pct <- function(data, varname, label_override=NULL) {
  x      <- as.integer(zap_labels(data[[varname]]))
  party  <- data$party2
  lbl    <- attr(data[[varname]], "labels")

  # Drop NA / refused codes (98,99,999)
  valid  <- !is.na(x) & !is.na(party) & !(x %in% c(98, 99, 999))
  x2     <- x[valid]
  p2     <- factor(party[valid], levels=c("Democrat","Republican"))

  if (length(x2) < 10) {
    cat("\n---", varname, "--- (insufficient data)\n")
    return(invisible(NULL))
  }

  tbl <- table(p2, x2)
  pct <- round(prop.table(tbl, 1) * 100, 1)
  cs  <- suppressWarnings(chisq.test(tbl))

  # Build column labels
  col_vals <- colnames(pct)
  col_labs <- col_vals
  if (!is.null(lbl)) {
    for (i in seq_along(col_vals)) {
      idx <- which(lbl == as.integer(col_vals[i]))
      if (length(idx) == 1)
        col_labs[i] <- paste0(col_vals[i], "=", substr(names(lbl)[idx], 1, 30))
    }
  }
  if (!is.null(label_override)) col_labs <- label_override

  # Print
  title <- if (!is.null(label_override)) varname else varname
  cat("\n---", varname, "---\n")
  cat("N valid: Rep=", sum(p2=="Republican"), " Dem=", sum(p2=="Democrat"), "\n")

  m <- matrix(pct, nrow=2, dimnames=list(rownames(pct), col_labs))
  print(m)
  cat("Chi-sq=", round(cs$statistic, 2),
      " df=", cs$parameter,
      " p=", format.pval(cs$p.value, digits=3, eps=0.001), "\n")
  invisible(cs)
}

# ══════════════════════════════════════════════════════════════════════════════
cat("═══════════════════════════════════════════════════════════\n")
cat("SECTION 1: AI AWARENESS & GENERAL USE\n")
cat("═══════════════════════════════════════════════════════════\n")

# Peek at labels
for (v in c("AI_HEARD_W119","USEAI_W119")) {
  cat("\nLabels for", v, ":\n")
  print(attr(df[[v]], "labels"))
}

crosstab_pct(dpar, "AI_HEARD_W119")
crosstab_pct(dpar, "USEAI_W119")

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 2: AI KNOWLEDGE INDEX (AIKNOW)\n")
cat("═══════════════════════════════════════════════════════════\n")

know_correct <- c("AIKNOW1_CORRECT_W119","AIKNOW2_CORRECT_W119",
                  "AIKNOW3_CORRECT_W119","AIKNOW5_CORRECT_W119",
                  "AIKNOW6_CORRECT_W119","AIKNOW7_CORRECT_W119")

# Check labels for one
cat("\nLabels for AIKNOW1_CORRECT_W119:\n")
print(attr(df[["AIKNOW1_CORRECT_W119"]], "labels"))

# Per-item cross-tabs
for (v in know_correct) {
  crosstab_pct(dpar, v)
}

# Pre-built index
cat("\n--- AIKNOW_INDEX_W119 (pre-built) ---\n")
cat("Labels:\n"); print(attr(df[["AIKNOW_INDEX_W119"]], "labels"))
idx_val <- as.integer(zap_labels(dpar$AIKNOW_INDEX_W119))
valid   <- !is.na(idx_val) & !is.na(dpar$party2) & !(idx_val %in% c(98,99,999))
idx2    <- idx_val[valid]
p2      <- dpar$party2[valid]

cat("Mean knowledge index by party:\n")
print(tapply(idx2, p2, mean, na.rm=TRUE))
cat("SD:\n")
print(tapply(idx2, p2, sd, na.rm=TRUE))
tt <- t.test(idx2[p2=="Republican"], idx2[p2=="Democrat"])
cat("t-test: t=", round(tt$statistic,3), " p=", format.pval(tt$p.value, digits=3), "\n")
cat("95% CI for difference:", round(tt$conf.int,3), "\n")

# Distribution by party
tbl_idx <- table(p2, idx2)
cat("\nKnowledge index distribution (%):\n")
print(round(prop.table(tbl_idx,1)*100, 1))
cs_idx <- suppressWarnings(chisq.test(tbl_idx))
cat("Chi-sq=", round(cs_idx$statistic,2), " p=", format.pval(cs_idx$p.value,digits=3), "\n")

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 3: DESRISK - COMFORT, CREATIVITY, NEW TECH\n")
cat("═══════════════════════════════════════════════════════════\n")

for (v in c("DESRISK_COMF_W119","DESRISK_CREAT_W119","DESRISK_NTECH_W119")) {
  cat("\nLabels for", v, ":\n"); print(attr(df[[v]], "labels"))
  crosstab_pct(dpar, v)
}

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 4: AI IN HEALTHCARE (AIHC)\n")
cat("═══════════════════════════════════════════════════════════\n")

aihc_vars <- grep("^AIHC", names(dpar), value=TRUE)
cat("AIHC variables:", paste(aihc_vars, collapse=", "), "\n")

for (v in aihc_vars) {
  cat("\nLabels for", v, ":\n"); print(attr(df[[v]], "labels"))
  crosstab_pct(dpar, v)
}

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 5: AI IN MENTAL HEALTH (AIMH)\n")
cat("═══════════════════════════════════════════════════════════\n")

aimh_vars <- grep("^AIMH", names(dpar), value=TRUE)
cat("AIMH variables:", paste(aimh_vars, collapse=", "), "\n")

for (v in aimh_vars) {
  cat("\nLabels for", v, ":\n"); print(attr(df[[v]], "labels"))
  crosstab_pct(dpar, v)
}

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 6: AI IN PAIN/PROT/CROP/EXT/NEWS/IMAG\n")
cat("═══════════════════════════════════════════════════════════\n")

domain_vars <- grep("^AIPAIN|^AIPROT|^AICROP|^AIEXT|^AINEWS|^AIIMAG", names(dpar), value=TRUE)
cat("Domain AI vars:", paste(domain_vars, collapse=", "), "\n")

for (v in domain_vars) {
  cat("\nLabels for", v, ":\n"); print(attr(df[[v]], "labels"))
  crosstab_pct(dpar, v)
}

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 7: AI AT WORK - GENERAL (AIWRK2, AIWRK3)\n")
cat("═══════════════════════════════════════════════════════════\n")

aiwrk_gen <- grep("^AIWRK2|^AIWRK3", names(dpar), value=TRUE)
for (v in aiwrk_gen) {
  cat("\nLabels for", v, ":\n"); print(attr(df[[v]], "labels"))
  crosstab_pct(dpar, v)
}

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 8: AI AT WORK - HAVE USED (AIWRKH)\n")
cat("═══════════════════════════════════════════════════════════\n")

aiwrkh_vars <- grep("^AIWRKH[0-9]", names(dpar), value=TRUE)
for (v in aiwrkh_vars) {
  cat("\nLabels for", v, ":\n"); print(attr(df[[v]], "labels"))
  crosstab_pct(dpar, v)
}

# ══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════\n")
cat("SECTION 9: AI AT WORK - MIGHT USE (AIWRKM)\n")
cat("═══════════════════════════════════════════════════════════\n")

aiwrkm_vars <- grep("^AIWRKM", names(dpar), value=TRUE)
for (v in aiwrkm_vars) {
  cat("\nLabels for", v, ":\n"); print(attr(df[[v]], "labels"))
  crosstab_pct(dpar, v)
}

cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("ANALYSIS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════\n")
