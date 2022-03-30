

# 1 
age = c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)


#a Use smoothing by bin means to smooth the above data, using a bin depth of 3. Illustrate your steps. Comment
#on the effect of this technique for the given data.
smooth(age, "3R") #shows a forecast of data which is not always accurate


#b How can you determine outliers in the data?
data points that are far away from other data points such as the 70


#c Use min-max normalization to transform the value 35 for age onto the range [0.0, 1.0].

min_max_norm = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

min_max_norm(age) #gets the min max for every number

(35 - min(13)) / (max(70) - min(13)) #gets the min max for the specific number 35 within the age range


#d Use z-score normalization to transform the value 35 for age? (you need to compute mean and standard deviation first)
m = mean(age)#find mean

s = sd(age)#find standard deviation

age.z = (age - m) / s #standardize vector calculation

age.z #display 

target = (35 - m) / s #targeted age of 35

target #display

#e Use normalization by decimal scaling to transform the value 35 for age.
#take 35 as it has two digits and place two zeros after a 1
n = (35/100)

# 2
number = 35 #place number here (one dimensional array)
minNumber = 13 #place minimum here
maxNumber = 70 #place maximum here
(number - min(minNumber)) / (max(maxNumber) - min(minNumber)) #gets the min max for the specific number 


#3 && #4

log2(1/2) # = -1

a1 = 79/165 #total of people per age group
a2 = 49/165 #total of people per age group
a3 = 20/165 #total of people per age group
a4 = 3/165 #total of people per age group
a5 = 4/165 #total of people per age group

p1 = 44/79 #enter the first set of numbers here (juniors)
p2 = 35/79 #enter the second set of numbers here (seniors)

d1 = -(p1*log2(p1)) + -(p2*log2(p2))

d1 #first result p1=44/79 p2=35/79 d1 = 0.99

p3 = 49/49 #enter the first set of numbers here (juniors)
p4 = 0/49 #enter the second set of numbers here (seniors)

d2 = -(p3*log2(p3)) + -(p4*log2(p4))

d2 #second result p1=49/49 p2=0/49 d1 = 0

p5 = 20/20 #enter the first set of numbers here (juniors)
p6 = 0/20 #enter the second set of numbers here (seniors)

d3 = -(p5*log2(p5)) + -(p6*log2(p6))

d3 #third result p1=20/20 p2=0/20 d1 = 0

p7 = 0/3 #enter the first set of numbers here (juniors)
p8 = 3/3 #enter the second set of numbers here (seniors)

d4 = -(p7*log2(p7)) + -(p8*log2(p8))

d4 #fourth result p1=0/3 p2=3/3 d1 = 0

p9 = 0/4 #enter the first set of numbers here (juniors)
p10 = 4/4 #enter the second set of numbers here (seniors)

d5 = -(p9*log2(p9)) + -(p10*log2(p10))

d5 #fifth result p1=0/4 p2=4/4 d1 = 0

r1 = a1*d1
if (is.nan(r1) == TRUE){r1=0}

r2 = a2*d2
if (is.nan(r2) == TRUE){r2=0}

r3 = a3*d3
if (is.nan(r3) == TRUE){r3=0}

r4 = a4*d4
if (is.nan(r4) == TRUE){r4=0}

r5 = a5*d5
if (is.nan(r5) == TRUE){r5=0}

t = r1 + r2 + r3 + r4 + r5
  
t




















