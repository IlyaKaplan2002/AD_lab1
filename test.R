
data <- read.csv('./data/population_usa.csv')

library(e1071)

attach(data)



analizeAll <- function(v, name) {
    rnormData <- rnorm(v)

    plot(density(rnormData), main = paste(name, "(empiric function)"))

    lines(hist(rnormData, xlab = name, main = paste(name, "(frequency range)"))$counts ~ hist(rnormData, xlab = name, main = paste(name, "(frequency range)"))$mids, col = "green")

    boxplot(v, main=paste(name, "(box-and-whiskers diagram)"), horizontal=T, col=c(13)) 

    summaryVal <- c(summary(v))

    deciles <- quantile(v, probs = seq(0, 1, 0.1))

    meanVal <- c(mean(Population))

    geomMean <- c(exp(mean(log(v))))

    getHarmonicMean <- function(v) {
        N <- length(v)
        inverse <- v^(-1)
        sumOfInverse <- sum(inverse)
        HM <- N/sumOfInverse

        return(HM)
    }

    getMode <- function(v) {
        uniq <- unique(v)
        mode <- uniq[which.max(tabulate(match(v,uniq)))]

        return(mode)
    }

    harmonicMean <- c(getHarmonicMean(v))

    mode <- c(getMode(v))

    medianVal <- c(median(v))

    centerData <- data.frame(meanVal,geomMean,harmonicMean,mode,medianVal)


    dispers <- c(var(v))

    standartDeviation <- c(sd(v))

    meanAbsoluteDeviation <- c(mean(abs(v-mean(v))))

    variationCoef <- c(sd(v) / mean(v) * 100)

    scope <- c(max(v)-min(v))

    interQuartal <- c(IQR(v))

    scattering <- data.frame(dispers,standartDeviation,meanAbsoluteDeviation,variationCoef,scope,interQuartal)

    # kurtosis, skewness analises

    kurtosisValue <- c(kurtosis(v))
    skewnessValue <- c(skewness(v))

    kurtosisSkewnessAnalises <- data.frame(kurtosisValue,skewnessValue)

    hist(rnormData, main = paste(name, "(skewless and kurtosis)") , xlab = name)

    write.csv(kurtosisSkewnessAnalises, file = gsub(" ", "", paste("output/",name,"/kurtosisSkewnessAnalises.csv")))
    write.csv(scattering, file = gsub(" ", "", paste("output/",name,"/scattering.csv")))
    write.csv(summaryVal, file = gsub(" ", "", paste("output/", name, "/summury.csv")))
    write.csv(deciles, file = gsub(" ", "", paste("output/", name, "/deciles.csv")))
    write.csv(centerData, file = gsub(" ", "", paste("output/", name, "/centerData.csv")))
}


analizeAll(Population, "Population")
analizeAll(Urban.Population, "UrbanPopulation")
analizeAll(World.Population, "WorldPopulation")


