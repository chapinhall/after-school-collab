#--------------------------------------------------------------------------------------------------
#
# GENERATE SYNTHETIC DATA
#
# This script generates a synthetic data set that can be used to demonstrate all key functions of
#   the Chapin Hall Collaborative's suite of analytics and reports without the concern of displaying
#   identifying or otherwise sensitive data.
#
# The subtle distinction between this and the scramble_data.R script is that, rather than masking 
#   identifers (e.g. names of organizations and sites) and jittering real data, this approach boils
#   real data down into its patterns and draws a completely synthetic--but real looking--data set
#   with completely synthetic names.
#
# Author: Nick Mader
#--------------------------------------------------------------------------------------------------

# Still to fit in: generating names for organizations and sites
#   - Simplest if 
# XXX Return to think about need for/means of dealing with categorical variables.
#   - Perhaps generate a multinomial prediction
# XXX Think about how to create meaningful addresses (since X and Y variables ought to be preserved,
#   but don't have a generally monotonic relationships that can be captured with a correlation
#   coefficient). Perhaps these can be sampled using some conditional probability means... not sure
#   if this can be done with individual addresses, but could be with tracts or community areas, and
#   match between tract and own characteristics. If done with community area, could jitter from there.
#   - Current thought is to block by commmunity area, do a multinomial selection of tracts within
#     community area based on race, and can jitter slightly to get distinct addresses


# 0. Set up WorkLoad Data

# 1. Determine the characteristics of each variables--e.g. categorical, discrete or continuous; if
#     continuous, how it is distributed; and how the values are rounded
#     - Continuous: ISAT, attendance, HS test scores, MVMS
#     - Categorical: race, program participation, grade level, community area
#     - Binary: free lunch, gender, IEP, on-track status

# 2. Normalize continuous variables using BoxCox and rescaling to mu=0,s=1
  x <- exp(rnorm(100)); fivenum(x)
  library("car")
  pow <- powerTransform(x)
  x.bc <- bcPower(x, coef(pow))
  mean(x.bc); var(x.bc)


# 3. Subset the data in to different distinct populations, e.g. by year, neighborhood, 
#     and/or demographic subset. These must be no smaller than 10 individuals, and ideally a fair amount
#     larger. Save information about the sample size.
#     - Define subsets by: race (white, black, hisp, other), gender, grade, in Collab or not, commmunity area
#     - Within subsets: scores, attendance, which organization and which program/site/partner
#     - Post draws: determine census tract based on community area, race, in multinomial prediction

# 4. For each subset, calculate the variance covariance matrix for all measures. (At this point,
#     all categorical variables must have been either used to create the subsets, or must have been
#     discretized.)
# 5. For each subset, take multivariate normal draws from the variance-covariance matrix to get a sample
#     of the appropriate size for the subset. Rescale to restore the mean and variance of the variables,
#     reverse the Box-Cox transformation, and round according to the right number of significant digits.
# 6. Generate new names. Use a unique mapping between real and new names for categoricals used to define
#     subsets. Same idea but different process: map names of variables (e.g. participation in given
#     site) to a new value.
#     - Organization names to planets
#     - Sites to jumble of scientists and composers
#     - Programs to board games
#     - Gender and race are left consistent (but could be bass/treble, and sweet/sour/bitter/salty)
# 7. Generate diagnostics: check how well different moments compare with the real data
#     - Various conditional mean/variances, e.g. test scores by grade by org
#     - Distributions acros scategoricals, e.g. community areas (... or, well, something not guaranteed
#       by construction...)

