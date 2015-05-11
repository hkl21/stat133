Regular expression

cNames = c("Dewitt County", 
           "Lac qui Parle County", 
           "St John the Baptist Parish", 
           "Stone County")

test = cNames[3]
string = "The Slippery St Frances"

1. substring
substring(test, 1,2)

[1] "St"

newName = paste("St.", substring(test, 3, nchar(test)), sep = "")

newName
[1] "St. John the Baptist Parish"


2.strsplit

string = "The Slippery St Frances"
chars1 = unlist(strsplit(string, " "))
chars1

[1] "The"      "Slippery" "St"       "Frances" 

chars = unlist(strsplit(string, ""))
chars

[1] "T" "h" "e" " " "S" "l" "i" "p" "p" "e" "r" "y" " " "S" "t" " " "F"
[18] "r" "a" "n" "c" "e" "s"

## more more

possible = which(chars == "S")
possible

[1]  5 14
# 5????????? 14??????

substring(string, possible, possible + 2)
[1] "Sli" "St "


3.gsub

strings = c("a test", "and one and one is two", "one two three")

gsub("one", "1", strings)

[1] "a test"             "and 1 and 1 is two" "1 two three"  

4. Lab

movies <- c("The Shawshank Redemption (1994)"   ,
            "The Godfather (1972)" ,
            "The Godfather: Part II (1974)",
            "Pulp Fiction (1994)",
            "The Good, the Bad and the Ugly (1966)",
            "12 Angry Men (1957)")

gregexpr("\\(.*\\)", movies[1])

gregexpr ????????? ??????????????? ????????? ????????????


gsub("[[:blank:]].*$", "", movies[5])

[1] "The"

?????? ?????? ???????????? ??? ???????????? ??????


gsub(" \\(.*$", "", movies[5])

?????? ?????? ??? ??????

cats = c("diplocat", "Hi cat", "mat", "at", "t!", "ct")
The < stands for beginning of a word and > stands for the end of a word In R we have to escape the \ with an extra \.

grep("\\<(cat|at|t)\\>", cats)

??? cat ?????? at ?????? t??? 


caats = c("cat", "caat.", "caats", "caaaat", "my cat")
grep("\\<ca+t\\>", caats)

cat or caat or caaat ????????? ?????? ??????

# the {1,} is equivalent to +
grep("\\<ca{1,}t\\>", caats)




nums = c("1.2", "-3000", "5lo", "hi2", "12.", "+57")
grep("^[-+]?[[:digit:]]+(\\.[[:digit:]]+)?$", nums)






movies <- c("The Shawshank Redemption (1994)"   ,
            "The Godfather (1972)" ,
            "The Godfather: Part II (1974)",
            "Pulp Fiction (1994)",
            "The Good, the Bad and the Ugly (1966)",
            "12 Angry Men (1957)")

substring




###
paste(x, y, z, ., sep = " ", collapse = NULL)


cNames = c("Dewitt County", 
           "Lac qui Parle County", 
           "St John the Baptist Parish", 
           "Stone County")

substring(cNames, 1, 3)

test = cNames[3]

###



newName = paste("St.", substring(test, 3, length(test)), sep = "")

newName


paste(x, y, z, sep = "**", collapse ="   111  ")

#paste?????? x??? ????????? elment??? y,z??? ????????? elments??? ????????????
#sep??? x,y,z??? ????????? ????????? ??? 
#collapse??? x,y,z??? ????????? elments?????? ????????? ????????? ????????? elments?????? ?????????????????? ???????????? ???



newNames = cNames
whichRep = substring(cNames, 1, 3) == "St "
newNames[whichRep] = 
  paste("St. ", substring(cNames[whichRep], 4, nchar(cNames[whichRep])), sep = "")
newNames

############

string = "The Slippery St Frances"
chars1 = unlist(strsplit(string, " "))
chars1

chars = unlist(strsplit(string, ""))
chars

possible = which(chars == "S")
possible

substring(string, possible, possible + 2)

#############


cats = c("diplocat", "Hi cat", "mat", "at", "t!", "ct")


grep("\\<(cat|at|t)\\>", cats)
grep("\\<(ca|a)?t\\>", cats)


caats = c("cat", "caat.", "caats", "caaaat", "my cat")
grep("\\<ca+t\\>", caats)
# the {1,} is equivalent to +
grep("\\<ca{1,}t\\>", caats)



dogs = c("dogmatic", "TopDog","Doggone it!", "RUN DOG RUN")
# The tolower function is handy here.
grep("dog", tolower(dogs))
grep("[Dd][Oo][Gg]", dogs)


nums = c("1.2", "-3000", "5lo", "hi2", "12.", "+57")
grep("^[-+]?[[:digit:]]+(\\.[[:digit:]]+)?$", nums)


