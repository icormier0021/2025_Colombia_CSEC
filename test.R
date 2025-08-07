test1 <- table(vacsDesign$variables[["SexTransact"]], vacsDesign$variables[["SexAct"]], useNA = "always")
print(test1)
#the design variables show that 
test2 <- table(VACSData$Q500,VACSData$Q407, useNA = "always")
print(test2)
test3 <- table(df$SexTransact, df$SexAct, useNA = "always")
print(test3)

# Define the full set of levels for both variables
# (union of all levels in all datasets)
sextransact_levels <- union(
        levels(factor(vacsDesign$variables[["SexTransact"]])),
        union(
                levels(factor(VACSData$Q407)),
                levels(factor(df$SexTransact))
        )
)

sexact_levels <- union(
        levels(factor(vacsDesign$variables[["SexAct"]])),
        union(
                levels(factor(VACSData$Q500)),
                levels(factor(df$SexAct))
        )
)

# Ensure NAs are treated consistently by adding NA as a level if desired
sextransact_levels <- c(sextransact_levels, NA)
sexact_levels      <- c(sexact_levels, NA)

# Convert all to factors with the same levels
vacsDesign$variables[["SexTransact"]] <- factor(vacsDesign$variables[["SexTransact"]],
                                                levels = sextransact_levels)
vacsDesign$variables[["SexAct"]] <- factor(vacsDesign$variables[["SexAct"]],
                                           levels = sexact_levels)

VACSData$Q407 <- factor(VACSData$Q407, levels = sextransact_levels)
VACSData$Q500 <- factor(VACSData$Q500, levels = sexact_levels)

df$SexTransact <- factor(df$SexTransact, levels = sextransact_levels)
df$SexAct      <- factor(df$SexAct, levels = sexact_levels)

# Now make the tables — dimensions will match
test1 <- table(vacsDesign$variables[["SexTransact"]],
               vacsDesign$variables[["SexAct"]],
               useNA = "always")

test2 <- table(VACSData$Q407,
               VACSData$Q500,
               useNA = "always")

test3 <- table(df$SexTransact,
               df$SexAct,
               useNA = "always")

# Check dimensions
dim(test1)
dim(test2)
dim(test3)
