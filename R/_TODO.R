# TODO:
#
# 1. create functions with formula interface based on:
#    heplots::leveneTests()
#    heplots::bartlettTests()
#
#    broom::tidy(bartlett.test(count ~ spray, data = InsectSprays))
#
# Function may be called `test_variances()`
#
# 2. Create formula interface for heplots::boxM(). May use S4 objects
#    df, formula
#    formula, any
#    df, df     arba     any, any
