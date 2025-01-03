# Function to calculate and plot scores at peak velocity

# # Fit quartic
# formCode <- paste0("dep ~ age +
#                    I(age^2) +
#                    I(age^3) +
#                    I(age^4) +
#                    (1 + age + I(age^2) |Subject)" )
#
# model <- lmer(formula = formCode, REML = FALSE, data = dataSerum)

peakVel <- function(model, data, condition, plotColours, legendTitle, legendLabels){

  lapply(c("", 2:4), function(q){

  b_intercept <- tidy(model, "fixed") %>%
    filter(term %in% c("(Intercept)", paste0(condition, q))) %>%
    pull(estimate) %>%
    sum()

  b_age <- tidy(model, "fixed") %>%
    filter(term %in% c("age", paste0("age:", condition, q))) %>%
    pull(estimate) %>%
    sum()

  b_age_2 <- tidy(model, "fixed") %>%
    filter(term %in% c("I(age^2)", paste0("I(age^2):", condition, q))) %>%
    pull(estimate) %>%
    sum()

  b_age_3 <- tidy(model, "fixed") %>%
    filter(term %in% c("I(age^3)", paste0("I(age^3):", condition, q))) %>%
    pull(estimate) %>%
    sum()

  b_age_4 <- tidy(model, "fixed") %>%
    filter(term %in% c("I(age^4)", paste0("I(age^4):", condition, q))) %>%
    pull(estimate) %>%
    sum()

  meanAge <- data %>%
    filter(Subject %in% model.frame(model)$Subject) %>%
    pull(age_original) %>%
    mean(na.rm = T)

  # Age at increased velocity (peak velocity)
  velPeak <- -6 * b_age_3 - sqrt((6 * b_age_3)^2 - 4 * (12 * b_age_4) * (2 * b_age_2)) / (24 * b_age_4) + meanAge
  velPeak

  # Age at decreased velocity
  velMin <- -6 * b_age_3 + sqrt((6 * b_age_3)^2 - 4 * (12 * b_age_4) * (2 * b_age_2)) / (24 * b_age_4) + meanAge
  velMin

  dataMod <- data %>%
    filter(Subject %in% model.frame(model)$Subject)

  dataMod$vel <-
    b_age +
    ( 2 * b_age_2 * dataMod$age ) +
    ( 3 * b_age_3 * (dataMod$age)^2 ) +
    ( 4 * b_age_4 * (dataMod$age)^3 )

  grid.arrange(
  ggplot() +
    geom_line(data = dataMod, aes(x= age_original ,  y = pred, color = !!sym(condition) ), linewidth = 1 , na.rm=T) +
    geom_vline(xintercept = velPeak, colour="red", linetype = "longdash") +
    geom_vline(xintercept = velMin, colour="red", linetype = "longdash") +
    scale_color_manual(values = plotColours,
                       labels = legendLabels) +
    scale_fill_manual(values = plotColours, guide = "none") +
    theme(legend.text = element_text(color = "black")) +
    labs(color = paste0(legendTitle)) +
    ylab(paste0("SMFQ Score")) +
    xlab("Age (years)")
,
arrangeGrob(
  ggplot() +
    geom_line(data = dataMod, aes(x= age_original ,  y = vel ), linewidth = 1 , na.rm=T) +
    geom_vline(xintercept = velPeak, colour="red", linetype = "longdash") +
    geom_vline(xintercept = velMin, colour="red", linetype = "longdash") +
    ylab(paste0("Velocity")) +
    xlab("Age (years)"),
  widths = c(2,1)
)
)

  })

}
