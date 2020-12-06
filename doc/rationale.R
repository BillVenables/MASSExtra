## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "",
                      message = FALSE,
                      warning = FALSE)

## -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggwebthemes) ## https://gitlab.com/peterbar/ggwebthemes/
  library(visreg)
  library(knitr)
  library(tidyverse)
  library(patchwork)
  library(MASSExtra)
})
options(knitr.kable.NA = "")
theme_set(theme_web_bw() + theme(title = element_text(hjust = 0.5)))

## ----setup, fig.height = 5, fig.width = 10, out.width="100%", fig.cap="Box-cox, old and new displays"----
par(mfrow = c(1, 2))
mod0 <- lm(MPG.city ~ Weight, Cars93)
boxcox(mod0)  ## MASS
box_cox(mod0) ## MASSExtra tweak

## ---- fig.height=5, fig.width=10, out.width="100%", fig.cap="The Box-Cox transformation effect"----
p0 <- ggplot(Cars93) + aes(x = Weight) + geom_point(colour = "#2297E6")  + xlab("Weight (lbs)") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, size=0.7, colour = "black")
p1 <- p0 + aes(y = MPG.city) + ylab("Miles per gallon (MPG)") + ggtitle("Untransformed response")
p2 <- p0 + aes(y = bc(MPG.city, lambda(mod0))) + ggtitle("Transformed response") + 
  ylab(bquote(bc(MPG, .(round(lambda(mod0), 2)))))
p1 + p2

## ---- fig.width=8, fig.height=6, fig.align="center", out.width="75%"----------
big_model <- lm(medv ~ . + (rm + tax + lstat + dis)^2 + poly(dis, 2) + poly(rm, 2) +
                   poly(tax, 2) + poly(lstat, 2), Boston)
big_model %>% drop_term(k = "GIC") %>% plot() %>% kable(booktabs=TRUE, digits=3)

## ---- fig.width=8, fig.height=6, fig.align="center", out.width="65%"----------
base_model <- lm(medv ~ ., Boston)
gic_model <- step_GIC(base_model, scope = list(lower = ~1, upper = formula(big_model)))
drop_term(gic_model) %>% plot() %>% kable(booktabs = TRUE, digits = 3)

## ---- fig.width=12, fig.height=6, fig.align="center", out.width="100%"--------
capture.output(suppressWarnings({
   g1 <- visreg(gic_model, "dis", plot = FALSE, ylim = c(5,50))
   g2 <- visreg(gic_model, "lstat", plot = FALSE, ylim = c(5,50))
   plot(g1, gg = TRUE) + plot(g2, gg = TRUE) 
})) -> junk

