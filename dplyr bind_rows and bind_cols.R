one <- mtcars[1:4, ]
two <- mtcars[11:14, ]
three <- mtcars[15:32,]
dim(mtcars)
one
two
three


# You can either supply data frames as arguments
bind_rows(one, two)
# Or a single argument containing a list of data frames
bind_rows(list(one, two, three))

split(mtcars, mtcars$cyl)

bind_rows(split(mtcars, mtcars$cyl))



# When you supply a column name with the `.id` argument, a new
# column is created to link each row to its original data frame
bind_rows(list(one, two), .id = "id")
bind_rows(list(a = one, b = two), .id = "id")
bind_rows("group 1" = one, "group 2" = two, .id = "groups")

# Columns don't need to match when row-binding
bind_rows(data.frame(x = 1:3), data.frame(y = 1:4))
## Not run: 
# Rows do need to match when column-binding
bind_cols(data.frame(x = 1), data.frame(y = 1:2))

## End(Not run)

bind_cols(one, two)
bind_cols(list(one, two))
bind_cols(list(one, two, three))

# combine applies the same coercion rules
f1 <- factor("a")
f2 <- factor("b")
c(f1, f2)
unlist(list(f1, f2))

combine(f1, f2)
combine(list(f1, f2))
