plot(dataset$Weight[dataset$Gender=='female'], dataset$Height[dataset$Gender=='female'], xlab="Weight (kg)", ylab = "Height (cm)", pch=1, col=2)
points(dataset$Weight[dataset$Gender=='male'], dataset$Height[dataset$Gender=='male'], pch=2, col=4)
