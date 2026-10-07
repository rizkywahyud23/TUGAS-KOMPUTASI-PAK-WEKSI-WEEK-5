# 1. Rata-rata waktu tunggu mu=5 menit. Berapa peluang (PX>5)
# menggunakan sebaran eksponensial 
pexp(5, rate = 1/5, lower.tail = FALSE)

#Eksponensial dengan lambda = 0.2  
x_dexp <- seq(0, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = 0.2)
plot(x_dexp, y_dexp, type="l", col="green", lwd=2,
     main="PDF Distribusi Eksponensial (λ=0.2)",
     xlab="x", ylab="f(x)")

# 2. Kereta komuter tiba di stasiun secara acak antara pukul 07.00 hingga 07.20 (interval 20 menit).Berapakah ragam (varians) waktu tunggu penumpang
# menggunakan sebaran uniform (seragam)

a <- 0
b <- 20
# Varians distribusi Uniform(a,b)
varians <- (b-a)^2 / 12
varians

# Uniform dengan interval [0,20]
x_uniform <- seq(a, b, by = 1)
y_uniform <- dunif(x_uniform, min = a, max = b)

# Plot PDF distribusi Uniform
plot(x_uniform, y_uniform,
     type = "l",
     col = "lightblue",
     lwd = 2,
     main = "PDF Distribusi Uniform (0,20)",
     xlab = "Waktu tunggu (menit)",
     ylab = "f(x)")

# 3. Masa pakai sensor suhu memiliki rata-rata mu=10 tahun. Berapa peluang sensor tersebut rusak sebelum mencapai usia 5 tahun?
# menggunakan sebaran eksponensial

pexp(5, rate = 1/10)

#Eksponensial dengan lambda = 0.1
x_dexp <- seq(0, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = 0.1)
plot(x_dexp, y_dexp, type="l", col="blue", lwd=2,
     main="PDF Distribusi Eksponensial (λ=0.1)",
     xlab="x (tahun)", ylab="f(x)")


# 4. Berat bersih kemasan kopi menyebar normal dengan mu=250 gram dan sigma=5 gram. Kemasan dianggap underweight jika beratnya <240gram. Berapa proporsi produk yang tergolong underweight?
# menggunakan sebaran normal

mu <- 250
sigma <- 5
p <- pnorm(240, mean = mu, sd = sigma)
p

# Generate sampel (misal n = 100)
n <- 100
x <- rnorm(n, mean = mu, sd = sigma)
# Plot: histogram + overlay PDF teoritis
hist(x, breaks = 30, probability = TRUE,
     main = "Histogram Sampel N(250, 5^2) dengan PDF teoritis",
     xlab = "x (gram)")

curve(dnorm(x, mean = mu, sd = sigma), from = mu - 4*sigma, to = mu + 4*sigma, add = TRUE, lwd = 2)
abline(v = mean(x), col = "blue", lwd = 2) # mean sampel
abline(v = mu, col = "red", lwd = 2, lty = 2) # mean sebenarnya

legend("topright", legend = c("PDF teoritis", "mean sampel", "mean true"),
       lty = c(1, 1, 2), col = c("black", "blue", "red"), bty = "n")