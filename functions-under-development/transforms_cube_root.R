# http://rcompanion.org/handbook/I_12.html
#
# Avoid complex numbers for some cube roots
cube_root <- function(x) {
  sign(x) * abs(x)^(1/3)
}