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

