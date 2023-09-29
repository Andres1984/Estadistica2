# Load required libraries
library(quantmod)

# Define the tickers of Vanguard funds you want to analyze
tickers <- c("VFIAX", "VGSLX", "VTIAX","VTMGX","VEMAX","VFWAX","VGIVX","VICBX","VSTBX","VEUSX","VWO")

# Fetch historical price data for each fund
getSymbols(tickers, from = "2023-03-01", to = "2023-09-15")

vfiax=Delt(VFIAX$VFIAX.Close)[-1]
vgslx=Delt(VGSLX$VGSLX.Close)[-1]
vtiax=Delt(VTIAX$VTIAX.Close)[-1]
vtmxg=Delt(VTMGX$VTMGX.Close )[-1]
vemax=Delt(VEMAX$VEMAX.Close)[-1]
vfwax=Delt(VFWAX$VFWAX.Close)[-1] 
vgivx=Delt(VGIVX$VGIVX.Close)[-1]
vicbx=Delt(VICBX$VICBX.Close)[-1]
vstbx=Delt(VSTBX$VSTBX.Close)[-1]
veusx=Delt(VEUSX$VEUSX.Close)[-1]
vwo=Delt(VWO$VWO.Close)[-1]

returns_df=cbind(vfiax,vgslx,vtiax,vtmxg,vemax,vfwax,vgivx,vicbx,vstbx,veusx,vwo)
colnames(returns_df)=c("VFIAX", "VGSLX", "VTIAX","VTMGX","VEMAX","VFWAX","VGIVX","VICBX","VSTBX","VEUSX","VWO")

data_for_anova <- data.frame(
  Fund = rep(tickers, each = nrow(returns_df)),
  Returns = as.vector(returns_df)
)

# Perform ANOVA test
result_anova <- aov(Returns ~ Fund, data = data_for_anova)

# Print ANOVA summary
summary(result_anova)


library(ggplot2)

# Create a box plot with ggplot2
ggplot(data_for_anova, aes(x = Fund, y = Returns)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = median(Returns)), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Fund", y = "Returns") +
  theme_minimal()

