getwd()
# Assignment 2
#  list .csv files in Data directory
csv_files <- list.files(path="./Data",
                        pattern=".csv")
# how many files does that include?
length(csv_files)
length(list.files(path="Data",
                  pattern=".csv"))
# open file and store the contents as an R object named "df" (an object is something like matrix dataframe or list)
df <- read.csv("./Data/wingspan_vs_mass.csv")
# check out the first five lines the wingspan vs mass data set
head(df, n=5)
# find any files (recursively - which means files in the specified directory and any subdirectories) in the Data directory that begin with the letter “b” (lowercase)
list.files(path="./Data",
           pattern="^b",
           recursive=TRUE,
           full.names=TRUE)
# show first line of each of these three files
readLines("./Data/data-shell/creatures/basilisk.dat",n=1)
readLines("./Data/data-shell/data/pdb/benzaldehyde.pdb",n=1)
readLines("./Data/Messy_Take2/b_df.csv",n=1)
# do this in a single line of code
bfiles <- list.files(path="./Data",
                     pattern="^b",
                     recursive=TRUE,
                     full.names=TRUE)
for (i in bfiles) {
  print(readLines(i,n=1))
}
# first line of all files that that end in .csv
#(1 way) name csv_files to use as the vector in the for loop
csv_files <- list.files(path="./Data",
                        pattern=".csv",
                        recursive=TRUE,
                        full.names = TRUE)
for (i in csv_files) {
  print(readLines(i,n=1))
}
#(2 way) use the pathway directly as the vector inside the for loop
for (i in list.files(path="./Data",
                     pattern=".csv",
                     recursive=TRUE,
                     full.names = TRUE)) {
  print(readLines(i,n=1))
}