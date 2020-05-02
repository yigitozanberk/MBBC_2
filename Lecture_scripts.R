power.t.test(n = 16, delta = 2/4, type = "one.sample", alt = "one.sided")
# assumes sd = 1

power.t.test(n = 16, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")



nosim <- 100000
n <- 16
sigma <- 4
mu0 <- 30
mua <- 32
z <- rnorm(nosim)
xsq <- rchisq(nosim, df = 15)
t <- qt(0.95, 15)

mean(z + sqrt(n)*(mua - mu0)/sigma > t / sqrt(n - 1) * sqrt(xsq))





testStat = (12 - 10) / 4 * sqrt(100)

pt(testStat, df = 99, lower.tail = FALSE)

g1 <- c(140, 138, 150, 148, 135)
g2 <- c(132, 135, 151, 146, 130)
diff <- g1 - g2

# PRACTICE QUIZ

# 1st q
# reject
# 2nd q
# pval of 0.09 fail to reject

# 3rd q
1100 + c(-1, 1)*qt(.975, 9-1)*30/sqrt(9)
# [1] 1076.94 1123.06



n1 <- 100
n2 <- 100
sp <- sqrt(((n1-1)* 2^2 + (n2 - 1)*0.5^2) / (n1 + n2 - 2))
md <- 4 - 6
semd <- sp*sqrt(1/n1 + 1/n2)
rbind(
        md + c(-1 , 1) * qt(.975, n1+n2 - 2)*semd,
        t.test(g2, g1, paired = FALSE, var.equal= TRUE)$conf,
        #for this to work, the dataset needs to have only two levels.
        t.test(g2, g1, paired = TRUE)$conf
)

# 4th q
sx <- 2
sy <- 0.5
df <- ((sx^2/100+sy^2/100)^2)/((sx^2/100)^2/99+(sy^2/100)^2/99)

-2 + c(-1, 1) * qt(0.975, df = df) * sqrt(sx^2/100 + sy^2/100)

# > -2/sqrt(sx^2/100 + sy^2/100)
# [1] -9.701425
# > qt(0.975, df = df)
# [1] 1.981503
# yes it does

# 5th q
nx = 9
ny = 9
muX = 1
muY = -3
sdX = 1.8
sdY = 1.5

sp = sqrt(((nx - 1)*sdX^2 + (ny-1)*sdY^2)/(nx + ny - 2))
ts = (muX - muY)/sp/sqrt(1/nx+1/ny)
2 * pt(abs(ts), 8, lower.tail = FALSE)
# 0.0009055426
# reject

# 6th q
semd = sp*sqrt(1/nx + 1/ny)
-4 + c(-1, 1)*qt(0.975, nx+ny - 2) * semd
# -5.655699 -2.344301


# 7th q
nx = 16
ny = 16

muX = 4
muY = 11
sdX = 28
sdY = 20
sp = sqrt(((nx - 1)*sdX^2 + (ny-1)*sdY^2)/(nx + ny - 2))
ts = (muX - muY)/sp/sqrt(1/nx+1/ny)
2 * pt(abs(ts), 15, lower.tail = FALSE)
# 0.4285266 = about 0.4

# 8th q FALSE - 60% dene
power.t.test(n = 100, delta = 0.01, sd = 0.04, alternative = "one.sided")

# Two-sample t test power calculation 
# 
# n = 100
# delta = 0.01
# sd = 0.04
# sig.level = 0.05
# power = 0.5465182
# alternative = one.sided
# 
# NOTE: n is number in *each* group


# around 80%????

# pnorm(1.645 * 0.004, mean = 0.01, sd = 0.004, lower.tail = FALSE)

# 9th q
# nx = 288
# ny = 288
# 
# muX = 44
# muY = 42.04
# sdX = 12
# sdY = 12
# sp = sqrt(((nx - 1)*sdX^2 + (ny-1)*sdY^2)/(nx + ny - 2))
# ts = (muX - muY)/sp/sqrt(1/nx+1/ny)
# 2 * pt(abs(ts), 15, lower.tail = FALSE)
# # 0.06884417

sd = 12*sqrt(1/288 + 1/288)
# we reject if abs(ts) > 1.96
pnorm(-1.96, mean = 2, sd = 1) + pnorm(1.96, mean = 2, sd = 1)
# 0.484084




# 10th q
# power goes down

# 11th q FALSE
# it isn't necessarily unbiased - only when n1 n2 n3 equal?
# normality assumed?
# impossible to answer?

# 12th q FAlse
# you know you would reject for any smaller
# significance level

#13th q 
# ( - Inf, muX + zstat * s / sqrt(n) )






