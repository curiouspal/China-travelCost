library("foreign")
library("ggplot2")
library("manipulate")

d <- read.spss('/media/anirban/Ubuntu/Downloads/Histogram.sav', to.data.frame=TRUE)
d <- subset(d, f2 <= 4900)
names(d)
d$source01 <- as.factor(d$source01)
summary(d$source01)
for(i in 1:length(d$source01))
  if(!is.na(d$source01[i]))  
    if(d$source01[i]=="0") d$source011[i] <- "Migrant" 
for(i in 1:length(d$source01))
  if(!is.na(d$source01[i])) 
    if(d$source01[i]=="1") d$source011[i] <- "Local"

d$source011 <- as.factor(d$source011)
summary(d$source011)

# write.csv(d, file = "/media/anirban/Ubuntu/Downloads/Histogram.csv")


# round(prop.table(xtabs(formula = ~source01 + is.na(Cost_tran), data=d), 1)*100, 0)



# We remove the cases that have the value of variable f2 as 0.
d <- subset(d, f2!=0)

## We assign the value of variable "f2" as 1 to all cases that have the value of "f2" as 0 so that the log of that number is a finite number. 
# for(i in 1:length(d$f2))
#   if(!is.na(d$f2[i])) if(d$f2[i]==0) d$f2[i] <- 1



## We create a new variable "logf2" which is the log to the base 10 of the variable "f2".
d$logf2 <- log10(d$f2)
summary(d$logf2)
summary(d$source011)

## we split the larger data frame "d" into those who are locals and those who are migrants.
locals <- subset(d, source011=="Local")
migrants <- subset(d, source011=="Migrant")


ggplot(d, aes(x = logf2, fill = source01)) + 
  geom_histogram(color = "grey", size = 0.4, position = "identity", binwidth = 0.2, alpha=0.6, aes(y=..density..)) + 
  stat_function( 
    fun = dnorm, color = "pink", size=2,
    args = with(locals, c(mean = mean(logf2, na.rm = TRUE), sd = sd(logf2, na.rm = TRUE))) 
  ) + 
  stat_function( 
    fun = dnorm, color = "cyan", size = 2,
    args = with(migrants, c(mean = mean(logf2, na.rm = TRUE), sd = sd(logf2, na.rm = TRUE))) 
  ) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=15, angle = 0, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  )

## Mean and standard deviation of logf2 for the two groups.
mean(locals$logf2, na.rm = TRUE)
mean(migrants$logf2, na.rm = TRUE)
sd(locals$logf2, na.rm = TRUE)
sd(migrants$logf2, na.rm = TRUE)


mean(locals$Cost_tran, na.rm = TRUE)
mean(migrants$Cost_tran, na.rm = TRUE)
sd(locals$Cost_tran, na.rm = TRUE)
sd(migrants$Cost_tran, na.rm = TRUE)


ggplot(d, aes(x = f2, fill = source01)) + 
  geom_histogram(color = "grey", size = 0.4, position = "identity", binwidth = 50, alpha=0.6, aes(y=..density..)) + geom_density(aes(color=source01, fill = NULL), alpha=0.3, size=1.5) + coord_cartesian(xlim = c(0, 1000)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=15, angle = 0, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  )


ggplot(d, aes(x = logf2, fill = source011)) + 
  geom_histogram(color = "grey", size = 0.4, position = "identity", binwidth = 0.24268, alpha=0.4, aes(y=..ndensity..)) + 
  stat_function( 
    fun = dnorm, color = "red", size=2,
    args = with(locals, c(mean = mean(logf2, na.rm = TRUE), sd = sd(logf2, na.rm = TRUE))) 
  ) + 
  stat_function( 
    fun = dnorm, color = "darkblue", size = 2,
    args = with(migrants, c(mean = mean(logf2, na.rm = TRUE), sd = sd(logf2, na.rm = TRUE))) 
  ) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=15, angle = 0, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + xlab("Log of monthly transportation cost") + 
  labs(title="\nKernel density histograms - monthly transportation cost comparison \nlocals vs. migrants") + 
  scale_fill_manual(values=c("red", "darkblue"))




manipulate(
  ggplot(d, aes(x = logf2, fill = source011)) + 
    geom_histogram(color = "grey", size = 0.4, position = "identity", binwidth = x, alpha=0.4, aes(y=..density..)), 
  x=slider(0.24, 0.25)
)



ggplot(d, aes(x = logf2, fill = source011)) + 
  geom_histogram(color = "grey", size = 0.4, position = "identity", binwidth = 0.2422, alpha=0.4, aes(y=..ndensity..)) + 
  geom_density(aes(colour = source011, alpha=0.1, kernel = "gaussian", y = ..scaled..)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=15, angle = 0, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + xlab("Log of monthly transportation cost") + 
  labs(title="\nKernel density histograms - monthly transportation cost comparison \nlocals vs. migrants") + 
  scale_fill_manual(values=c("red", "darkblue"))

