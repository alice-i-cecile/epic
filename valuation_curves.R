# Simple linear valuation curve
val_lin <- data.frame(value=0:300, variable="Carbon Sequestration")
val_lin$score <- val_lin$value*1/300

# Simple exponential valuation curve
val_exp<- data.frame(value=seq(1,5, length.out=100), variable="Air Quality")
val_exp$score <- 1-exp(2*-(val_exp$value-1))

# Simple sigmoidal valuation curve
val_sig<- data.frame(value=seq(0,6, length.out=100), variable="Habitat")
val_sig$score <- 1/(1+exp(2*(-val_sig$value+3)))

# Combing the curves
val_services <- rbind(val_lin, val_exp, val_sig)

# Plotting
value_services_curves <- ggplot(val_services, aes(x=value, y=score)) + geom_line() + facet_wrap("variable", scale="free_x") + theme_bw() + xlab("Ecosystem service") + ylab("Value score")

# Saving
ggsave("value_service_curves.pdf", value_services_curves, height=4, width=6)