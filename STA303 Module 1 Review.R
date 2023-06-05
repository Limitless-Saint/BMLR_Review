#STA303 Module 1 Review

install.packages("palmerpenguins")
library(palmerpenguins)


var_body_mass = var(penguins$body_mass_g, na.rm = TRUE)
mean_body_mass = mean(penguins$body_mass_g, na.rm = TRUE)

t = (mean_body_mass - 4000)/(sqrt(var_body_mass/344))
quant_t_body = qt(p= 0.975, df = 343)

abs(t) <= quant_t_body

t.test(x = penguins$body_mass_g, mu = 4000, var.equal = TRUE)


mod4 <- lm(body_mass_g ~ flipper_length_mm + species, data=penguins)
summary(mod4)

mod4_model_matrix = model.matrix(mod4)

t.test(body_mass_g ~ sex, data = penguins, var.equal = TRUE)

spec_b_mass = lm(data = penguins, formula = body_mass_g ~ species)
anova(spec_b_mass)
