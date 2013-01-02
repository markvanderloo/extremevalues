
# simple plot positions
plotpositions(c(1,3,5))

# plot positions: duplicates get mean position
plotpositions(c(1,3,5,3))

# plot positions: duplicates get original position
plotpositions(c(1,3,5,3),duplicates='asis')

