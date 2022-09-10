data(heights)
heights
max(heights$height)
min(heights$height)
heights[which.min(heights$height),]
median(heights$height)
mean(heights$sex == "Female")  
mean(heights$height > 78) * 1050
((mean(heights$sex == "Female")) * (mean(heights$height > 78)) * 1050)
mean((mean(heights$height > 78))$sex == "Female")
