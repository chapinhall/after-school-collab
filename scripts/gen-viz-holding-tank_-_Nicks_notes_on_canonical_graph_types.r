# 4. Comparison on multiple, thematically-linked measures -- e.g. neighborhood characteristics. This is sort of an extension of the comparison on a single (continuous) measure

This is achieved the same as Graph 3, but where the aesx is its own construction. Just need to do special data prep ahead of time.


# 5. Comparison of a single measure across sites (or programs across sites)

Same as Graph 3, but with no fill since we do not want to compare within the x. Just have x and y. However, the x is a long list of sites, probably affiliated with a given organization. (Or we may compare
                                                                                                                                                                                           sites across organizations if doing benchmarking activities. )

# 6. Regression analysis, comparing specification -- e.g. raw vs. gain vs. lagged reg vs. full

For now, the default is to use the ascending color scale no matter what, and to apply standard errors no matter what. By spec or by site should differ only in data fed in and determination of order.

# 7. Regression analysis, comparing sites

## Within calls to canonical graphs, the arguments should be pretty straightforward. ERW's calls using "_params" are pretty simple. Could be even more simplified if calls were grouped by canonical graph

# 2. Comparison on a single (continuous) measure -- e.g. % free/reduced priced lunch
#      * Non-org, org, and possibly school-based peers -- currently vertical bars, with value labels

myGraphOut <- paste0("g:/YSS_Graph_Testing",site,"/")
Plot <- ggplot(data=data, aes_string(x=aesx, y=aesy, fill=aesfill)) +
  geom_bar(stat="identity", position="dodge", width=0.7) +
  ggtitle(plotTitle) + xlab(xlab) + ylab(ylab) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = useFill) +
  guides(fill = guide_legend(title = NULL)) + extra
print(Plot)

# 3. Comparison on a single (continuous measure, calculated/presented by category -- e.g. test scale scores or school attendance by grade)



# 4. Comparison on multiple, thematically-linked measures -- e.g. neighborhood characteristics. This is sort of an extension of the comparison on a single (continuous) measure
# 5. Comparison of a single measure across sites (or programs across sites)
# 6. Regression analysis, comparing specification -- e.g. raw vs. gain vs. lagged reg vs. full
# 7. Regression analysis, comparing sites
