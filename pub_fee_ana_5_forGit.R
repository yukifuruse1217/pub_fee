# Load necessary library
# install.packages("readr")
# install.packages("tidyr")
# install.packages("dplyr")

library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(stats)
#install.packages("cols4all", dependencies = TRUE)
library(cols4all)
library(RColorBrewer)
#install.packages("Polychrome")
library(Polychrome)
#install.packages("patchwork")
library(patchwork)
# install.packages("lmtest")
library(lmtest)
# install.packages("vars")
library(vars)



# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("circlize")  # Dependency for ComplexHeatmap

library(ggplot2)
library(dplyr)
library(circlize)


# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.16")  # Use the appropriate version for your R version
# BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)

library(tidyverse)
library(ggrepel)
library(ggtext)

library(showtext)
font_add_google("Lato")
showtext_auto()

library(tseries)





# Read the CSV file
### IF
# read tsv file from clipboard

df <- read.table("if_3_rev_constRate.csv" ,sep=",", header = TRUE)
colnames(df)[1] <- "journal"

# Use gather() function from tidyr to convert wide format to long format
df_temp <- gather(df, key = "year", value = "value", -journal)

# remove character x from df_temp$year
df_temp$year <- as.numeric(gsub("X", "", df_temp$year))
df_temp$value <- as.numeric(df_temp$value)

# rename value
colnames(df_temp)[3] <- "ImpFac"

# Calculate rate change of value from previous year for each journal
df_temp <- df_temp %>% group_by(journal) %>% mutate(rateImpFac = ImpFac/lag(ImpFac))


df_combine <- df_temp






### Pubnum
# read tsv file from clipboard

df <- read.table("pubnum_3_rev_constRate.csv" ,sep=",", header = TRUE)
colnames(df)[1] <- "journal"

# Use gather() function from tidyr to convert wide format to long format
df_temp <- gather(df, key = "year", value = "value", -journal)

# remove character x from df_temp$year
df_temp$year <- as.numeric(gsub("X", "", df_temp$year))
df_temp$value <- as.numeric(df_temp$value)

# rename value
colnames(df_temp)[3] <- "PubNum"

# Calculate rate change of value from previous year for each journal
df_temp <- df_temp %>% group_by(journal) %>% mutate(ratePubNum = PubNum/lag(PubNum))

# Exclude first year RatePubNum
df_temp <- df_temp %>% group_by(journal) %>% mutate(ratePubNum = ifelse(is.na(lag(ratePubNum)), NA, ratePubNum))

# Change the ratePubNum excluding NA at the first year to NA
df_temp$ratePubNum[is.na(df_temp$ratePubNum)] <- NA


df_combine <- left_join(df_combine, df_temp, by = c("journal", "year"))





### Price
# read tsv file from clipboard

df <- read.table("price_3_rev_constRate.csv" ,sep=",", header = TRUE)
colnames(df)[1] <- "journal"

# Use gather() function from tidyr to convert wide format to long format
df_temp <- gather(df, key = "year", value = "value", -journal)

# remove character x from df_temp$year
df_temp$year <- as.numeric(gsub("X", "", df_temp$year))
df_temp$value <- as.numeric(df_temp$value)

# rename value
colnames(df_temp)[3] <- "Price"

# Calculate rate change of value from previous year for each journal
df_temp <- df_temp %>% group_by(journal) %>% mutate(ratePrice = Price/lag(Price))


df_combine <- left_join(df_combine, df_temp, by = c("journal", "year"))

# df_price <- df_temp





### pubnum country
# read tsv file from clipboard

df <- read.table("pubmed_country_3_rev_constRate.csv" ,sep=",", header = TRUE)
colnames(df)[1] <- "year"

# Use gather() function from tidyr to convert wide format to long format
df_temp <- gather(df, key = "country", value = "value", -year)

# Calculate rate compared with "all"
df_temp <- df_temp %>% mutate(prop = value / value[country == "all"])


df_country <- df_temp





# Shorten names

df_combine$journal <- gsub("New England Journal of Medicine", "N Engl J Med", df_combine$journal)
df_combine$journal <- gsub("Lancet Global Health", "Lancet Glob Health", df_combine$journal)
df_combine$journal <- gsub("Nature Communications", "Nat Commun", df_combine$journal)
df_combine$journal <- gsub("Science Advances", "Sci Adv", df_combine$journal)
df_combine$journal <- gsub("JAMA Network Open", "JAMA Netw Open", df_combine$journal)
df_combine$journal <- gsub("Cell Reports", "Cell Rep", df_combine$journal)
df_combine$journal <- gsub("Genome Biology", "Genome Biol", df_combine$journal)
df_combine$journal <- gsub("PLoS Biology", "PLoS Biol", df_combine$journal)
df_combine$journal <- gsub("PLoS Medicine", "PLoS Med", df_combine$journal)
df_combine$journal <- gsub("Frontiers in Neuroscience", "Front Neurosci", df_combine$journal)
df_combine$journal <- gsub("Frontiers in Medicine", "Front Med", df_combine$journal)
df_combine$journal <- gsub("Scientific Reports", "Sci Rep", df_combine$journal)
df_combine$journal <- gsub("Frontiers in Psychiatry", "Front Psychiatry", df_combine$journal)
df_combine$journal <- gsub("International Journal of Molecular Sciences", "Int J Mol Sci", df_combine$journal)
df_combine$journal <- gsub("MEDIATORS OF INFLAMMATION", "Mediators Inflamm", df_combine$journal)
df_combine$journal <- gsub("Biomed Research International", "Biomed Res Int", df_combine$journal)
df_combine$journal <- gsub("Frontiers in Immunology", "Front Immunol", df_combine$journal)
df_combine$journal <- gsub("BMC Public Health", "BMC Public Health", df_combine$journal)




# Remove journals
df_combine_all <- df_combine


df_combine <- df_combine %>% filter(journal != "N Engl J Med" &
                                      journal != "JAMA" &
                                      journal != "Front Med" &
                                      journal != "Science")
















































# Draw line graph for Price

# Remove NA in Price
df_NAclean <- df_combine %>% filter(!is.na(Price))



# cols <- kelly.colors(length(unique(df_NAclean$journal)))
# cols <- dark.colors(length(unique(df_NAclean$journal)))
# cols <- sky.colors(length(unique(df_NAclean$journal)))

# cols <- light.colors(length(unique(df_NAclean$journal)))

# cols <- glasbey.colors(length(unique(df_NAclean$journal)))
cols <- palette36.colors(length(unique(df_NAclean$journal)))
# cols <- alphabet.colors(length(unique(df_NAclean$journal)))
# cols <- green.armytage.colors(length(unique(df_NAclean$journal)))


cols <- as.character(cols)
cols[2] <- "black"

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))



f1A <- ggplot(df_NAclean, aes(x = year, y = Price, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Article processing charge", x = "Year", y = "USD") +
  scale_x_continuous(expand = c(0,0), limits = c(2000, 2031), breaks = seq(2000, 2024, by = 4), ) +
  ylim(NA, 12000) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )


  
# 1200x1200





























# Draw line graph for ImpFac



# Remove NA in ImpFac
df_NAclean <- df_combine %>% filter(!is.na(ImpFac))


cols <- palette36.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)
cols[2] <- "black"


df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))



sf1B <- ggplot(df_NAclean, aes(x = year, y = ImpFac, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Impact factor", x = "Year", y = "Impact factor") +
  scale_x_continuous(expand = c(0,0), limits = c(2000, 2031), breaks = seq(2000, 2024, by = 4), ) +
  # ylim(NA, 12000) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )


# 1200x1200




















# Draw line graph for PubNum

# Remove NA in PubNum
df_NAclean <- df_combine %>% filter(!is.na(PubNum))

cols <- palette36.colors(length(unique(df_NAclean$journal)))
cols <- as.character(cols)
cols[2] <- "black"


df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))



sf1A <- ggplot(df_NAclean, aes(x = year, y = PubNum, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Publications", x = "Year", y = "Count") +
  scale_x_continuous(expand = c(0,0), limits = c(2000, 2031), breaks = seq(2000, 2024, by = 4), ) +
  # ylim(NA, 12000) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )


# 1200x1200



















(sf1A | sf1B) +
  plot_layout(ncol = 2, heights = c(1), widths = c(1,1)) +
  
  #  (pA / pA2 / pB / pC) +  
  #    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA / pB / pC) | (pD / pD2 / pE / pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA | pB | pC) / (pD | pD2 | pE | pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1)) +
  
  plot_annotation(
    #    title = "Title",
    #    subtitle = "Subtitle",
    #    caption = "Caption",
    tag_levels = "A",
    ##    tag_prefix = "fig ",
    ##    tag_suffix = ":"
  ) & 
  theme(plot.tag = element_text(size = 36))

# 2400x3000
# now sf4





















































































# Draw line graph for Countries

# Remove "all" in country
df_country <- df_country %>% filter(country != "all")
# Remove 2024 in year
df_country <- df_country %>% filter(year != 2024)

cols <- light.colors(length(unique(df_country$country)))
cols <- as.character(cols)



df_country <- df_country %>% group_by(country) %>% mutate(name_lab = if_else(year == max(year), country, NA_character_))



f2D <- ggplot(df_country, aes(x = year, y = value, group = country, color = country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Publications", x = "Year", y = "Count") +
  scale_x_continuous(expand = c(0,0), limits = c(2000, 2031), breaks = seq(2000, 2024, by = 4), ) +
  # ylim(NA, 12000) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )


# 1200x1200










f2E <- ggplot(df_country, aes(x = year, y = prop*100, group = country, color = country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Publication proportions", x = "Year", y = "Percentage") +
  scale_x_continuous(expand = c(0,0), limits = c(2000, 2031), breaks = seq(2000, 2024, by = 4), ) +
  ylim(NA, 30) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )


# 1200x1200














































































# Compare within journal


### NEED price full csv (NOT csv for graph)

journal_name <- "Int J Mol Sci" #Sci Rep   PLos One   Int J Mol Sci

# smallest year where ImpFac is not NA
xlim_year1 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$ImpFac)])
xlim_year2 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$PubNum)])
xlim_year3 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$Price)])

xlim_year <- max(c(xlim_year1, xlim_year2, xlim_year3))


impfac <- df_combine$ImpFac[df_combine$journal == journal_name] / df_combine$ImpFac[df_combine$journal == journal_name & df_combine$year == xlim_year]

pubnum <- df_combine$PubNum[df_combine$journal == journal_name] / df_combine$PubNum[df_combine$journal == journal_name & df_combine$year == xlim_year]

price <- df_combine$Price[df_combine$journal == journal_name] / df_combine$Price[df_combine$journal == journal_name & df_combine$year == xlim_year]

impfac_rate <- df_combine$rateImpFac[df_combine$journal == journal_name]

pubnum_rate <- df_combine$ratePubNum[df_combine$journal == journal_name]

price_rate <- df_combine$ratePrice[df_combine$journal == journal_name]





year <- c(1996:2024)

value <- c(impfac, pubnum, price)
value_rate <- c(impfac_rate, pubnum_rate, price_rate)
class <- c(rep("Impact Factor", length(year)), rep("Publication count", length(year)), rep("APC", length(year)))
year <- rep(year, 3)

df_journal_graph <- data.frame(year, value, class)
df_journal_graph_rate <- data.frame(year, value_rate, class)

# level change
df_journal_graph$class <- factor(df_journal_graph$class, levels = c("APC", "Publication count", "Impact Factor"))
df_journal_graph_rate$class <- factor(df_journal_graph_rate$class, levels = c("APC", "Publication count", "Impact Factor"))

# draw line graph for value on y, year on x, linetype by class

sf2A <- ggplot(df_journal_graph, aes(x = year, y = log2(value), group = class, linetype = class)) + geom_line(size=2, color="darkgray") + # geom_point() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(xlim_year, 2025), breaks = seq(xlim_year, 2024, by = 2), ) +
  scale_y_continuous(breaks = seq(-1, 7, by = 1), limits = c(-1, 7)) +
  # legend and axis title size
  theme(legend.position = "top", legend.text = element_text(size = 24),
        axis.title = element_text(size = 28), axis.text = element_text(size = 28),
        # title size
        plot.title = element_text(size = 32)) +
  guides(linetype = guide_legend(override.aes = list(size = 15))) +
  # axis title change
  xlab("Year") + ylab("Log2 change from year with first IF") +
  # legend title change
  labs(linetype = "") +
  # Graph title add
  ggtitle("International Journal of Molecular Sciences")

# 800x800








sf2D <- ggplot(df_journal_graph_rate, aes(x = year, y = log2(value_rate), group = class, linetype = class)) + geom_line(size=2, color="darkgray") + # geom_point() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(xlim_year, 2025), breaks = seq(xlim_year, 2024, by = 2), ) +
  scale_y_continuous(breaks = seq(-2, 2, by = 1), limits = c(-2, 2)) +
  # legend and axis title size
  theme(legend.position = "top", legend.text = element_text(size = 24),
        axis.title = element_text(size = 28), axis.text = element_text(size = 28),
        # title size
        plot.title = element_text(size = 32)) +
  guides(linetype = guide_legend(override.aes = list(size = 15))) +
  # axis title change
  xlab("Year") + ylab("Log2 change from previous year") +
  # legend title change
  labs(linetype = "") +
  # Graph title add
  ggtitle("International Journal of Molecular Sciences")

# 800x800






### NEED price full csv (NOT csv for graph)

journal_name <- "PLoS One" #Sci Rep   PLos One   Int J Mol Sci

# smallest year where ImpFac is not NA
xlim_year1 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$ImpFac)])
xlim_year2 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$PubNum)])
xlim_year3 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$Price)])

xlim_year <- max(c(xlim_year1, xlim_year2, xlim_year3))


impfac <- df_combine$ImpFac[df_combine$journal == journal_name] / df_combine$ImpFac[df_combine$journal == journal_name & df_combine$year == xlim_year]

pubnum <- df_combine$PubNum[df_combine$journal == journal_name] / df_combine$PubNum[df_combine$journal == journal_name & df_combine$year == xlim_year]

price <- df_combine$Price[df_combine$journal == journal_name] / df_combine$Price[df_combine$journal == journal_name & df_combine$year == xlim_year]

impfac_rate <- df_combine$rateImpFac[df_combine$journal == journal_name]

pubnum_rate <- df_combine$ratePubNum[df_combine$journal == journal_name]

price_rate <- df_combine$ratePrice[df_combine$journal == journal_name]





year <- c(1996:2024)

value <- c(impfac, pubnum, price)
value_rate <- c(impfac_rate, pubnum_rate, price_rate)
class <- c(rep("Impact Factor", length(year)), rep("Publication count", length(year)), rep("APC", length(year)))
year <- rep(year, 3)

df_journal_graph <- data.frame(year, value, class)
df_journal_graph_rate <- data.frame(year, value_rate, class)

# level change
df_journal_graph$class <- factor(df_journal_graph$class, levels = c("APC", "Publication count", "Impact Factor"))
df_journal_graph_rate$class <- factor(df_journal_graph_rate$class, levels = c("APC", "Publication count", "Impact Factor"))

# draw line graph for value on y, year on x, linetype by class

sf2B <- ggplot(df_journal_graph, aes(x = year, y = log2(value), group = class, linetype = class)) + geom_line(size=2, color="darkgray") + # geom_point() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(xlim_year, 2025), breaks = seq(xlim_year, 2024, by = 2), ) +
  scale_y_continuous(breaks = seq(-1, 3, by = 1), limits = c(-1, 3)) +
  # legend and axis title size
  theme(legend.position = "top", legend.text = element_text(size = 24),
        axis.title = element_text(size = 28), axis.text = element_text(size = 28),
        # title size
        plot.title = element_text(size = 32)) +
  guides(linetype = guide_legend(override.aes = list(size = 15))) +
  # axis title change
  xlab("Year") + ylab("Log2 change from year with first IF") +
  # legend title change
  labs(linetype = "") +
  # Graph title add
  ggtitle(journal_name)

# 800x800








sf2E <- ggplot(df_journal_graph_rate, aes(x = year, y = log2(value_rate), group = class, linetype = class)) + geom_line(size=2, color="darkgray") + # geom_point() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(xlim_year, 2025), breaks = seq(xlim_year, 2024, by = 2), ) +
  scale_y_continuous(breaks = seq(-2, 2, by = 1), limits = c(-2, 2)) +
  # legend and axis title size
  theme(legend.position = "top", legend.text = element_text(size = 24),
        axis.title = element_text(size = 28), axis.text = element_text(size = 28),
        # title size
        plot.title = element_text(size = 32)) +
  guides(linetype = guide_legend(override.aes = list(size = 15))) +
  # axis title change
  xlab("Year") + ylab("Log2 change from previous year") +
  # legend title change
  labs(linetype = "") +
  # Graph title add
  ggtitle(journal_name)

# 800x800














### NEED price full csv (NOT csv for graph)

journal_name <- "Sci Rep" #Sci Rep   PLos One   Int J Mol Sci

# smallest year where ImpFac is not NA
xlim_year1 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$ImpFac)])
xlim_year2 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$PubNum)])
xlim_year3 <- min(df_combine$year[df_combine$journal == journal_name & !is.na(df_combine$Price)])

xlim_year <- max(c(xlim_year1, xlim_year2, xlim_year3))


impfac <- df_combine$ImpFac[df_combine$journal == journal_name] / df_combine$ImpFac[df_combine$journal == journal_name & df_combine$year == xlim_year]

pubnum <- df_combine$PubNum[df_combine$journal == journal_name] / df_combine$PubNum[df_combine$journal == journal_name & df_combine$year == xlim_year]

price <- df_combine$Price[df_combine$journal == journal_name] / df_combine$Price[df_combine$journal == journal_name & df_combine$year == xlim_year]

impfac_rate <- df_combine$rateImpFac[df_combine$journal == journal_name]

pubnum_rate <- df_combine$ratePubNum[df_combine$journal == journal_name]

price_rate <- df_combine$ratePrice[df_combine$journal == journal_name]





year <- c(1996:2024)

value <- c(impfac, pubnum, price)
value_rate <- c(impfac_rate, pubnum_rate, price_rate)
class <- c(rep("Impact Factor", length(year)), rep("Publication count", length(year)), rep("APC", length(year)))
year <- rep(year, 3)

df_journal_graph <- data.frame(year, value, class)
df_journal_graph_rate <- data.frame(year, value_rate, class)

# level change
df_journal_graph$class <- factor(df_journal_graph$class, levels = c("APC", "Publication count", "Impact Factor"))
df_journal_graph_rate$class <- factor(df_journal_graph_rate$class, levels = c("APC", "Publication count", "Impact Factor"))

# draw line graph for value on y, year on x, linetype by class

sf2C <- ggplot(df_journal_graph, aes(x = year, y = log2(value), group = class, linetype = class)) + geom_line(size=2, color="darkgray") + # geom_point() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(xlim_year, 2025), breaks = seq(xlim_year, 2024, by = 2), ) +
  scale_y_continuous(breaks = seq(-1, 6, by = 1), limits = c(-1, 6)) +
  # legend and axis title size
  theme(legend.position = "top", legend.text = element_text(size = 24),
        axis.title = element_text(size = 28), axis.text = element_text(size = 28),
        # title size
        plot.title = element_text(size = 32)) +
  guides(linetype = guide_legend(override.aes = list(size = 15))) +
  # axis title change
  xlab("Year") + ylab("Log2 change from year with first IF") +
  # legend title change
  labs(linetype = "") +
  # Graph title add
  ggtitle("Scientific Reports")

# 800x800








sf2F <- ggplot(df_journal_graph_rate, aes(x = year, y = log2(value_rate), group = class, linetype = class)) + geom_line(size=2, color="darkgray") + # geom_point() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(xlim_year, 2025), breaks = seq(xlim_year, 2024, by = 2), ) +
  scale_y_continuous(breaks = seq(-2, 2, by = 1), limits = c(-2, 2)) +
  # legend and axis title size
  theme(legend.position = "top", legend.text = element_text(size = 24),
        axis.title = element_text(size = 28), axis.text = element_text(size = 28),
        # title size
        plot.title = element_text(size = 32)) +
  guides(linetype = guide_legend(override.aes = list(size = 15))) +
  # axis title change
  xlab("Year") + ylab("Log2 change from previous year") +
  # legend title change
  labs(linetype = "") +
  # Graph title add
  ggtitle("Scientific Reports")

# 800x800








(sf2A | sf2B | sf2C) / (sf2D | sf2E | sf2F) +
  plot_layout(heights = c(1,1), widths = c(1,1,1)) +
  
  #  (pA / pA2 / pB / pC) +  
  #    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA / pB / pC) | (pD / pD2 / pE / pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA | pB | pC) / (pD | pD2 | pE | pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1)) +
  
  plot_annotation(
    #    title = "Title",
    #    subtitle = "Subtitle",
    #    caption = "Caption",
    tag_levels = "A",
    ##    tag_prefix = "fig ",
    ##    tag_suffix = ":"
  ) & 
  theme(plot.tag = element_text(size = 36))

# 3000x2000
# now sf5

































































































# Relative price compared to 2000 for each journal
base_year <- 2010 # 2005, 2010, 2015, 2020

df_price <- df_combine %>% group_by(journal) %>% mutate(relativePrice = Price/Price[year == base_year])


# Draw line graph for each journal, value on y, year on x
df_NAclean <- df_price %>% filter(!is.na(Price))

# Remove journals when relativePrice is NA
df_NAclean <- df_NAclean %>% filter(!is.na(relativePrice))



cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))



f1B <- ggplot(df_NAclean, aes(x = year, y = relativePrice, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Relative article processing charge", x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 4), breaks = seq(1, 4, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )

# 1200x800


(f1A | f1B) +
  plot_layout(ncol = 2, heights = c(1), widths = c(10,7)) +
  
  #  (pA / pA2 / pB / pC) +  
  #    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA / pB / pC) | (pD / pD2 / pE / pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA | pB | pC) / (pD | pD2 | pE | pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1)) +
  
  plot_annotation(
    #    title = "Title",
    #    subtitle = "Subtitle",
    #    caption = "Caption",
    tag_levels = "A",
    ##    tag_prefix = "fig ",
    ##    tag_suffix = ":"
  ) & 
  theme(plot.tag = element_text(size = 36))

# 2400x1000
# fig1

























# Relative price compared to 2000 for each journal
base_year <- 2005

df_price <- df_combine %>% group_by(journal) %>% mutate(relativePrice = Price/Price[year == base_year])


# Draw line graph for each journal, value on y, year on x
df_NAclean <- df_price %>% filter(!is.na(Price))

# Remove journals when relativePrice is NA
df_NAclean <- df_NAclean %>% filter(!is.na(relativePrice))



cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))



sf3A <- ggplot(df_NAclean, aes(x = year, y = relativePrice, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Article processing charge", x = "Year", y = "Relative rate from 2005") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 7), breaks = seq(1, 7, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



# Relative price compared to 2000 for each journal
base_year <- 2010 # 2005, 2010, 2015, 2020

df_price <- df_combine %>% group_by(journal) %>% mutate(relativePrice = Price/Price[year == base_year])


# Draw line graph for each journal, value on y, year on x
df_NAclean <- df_price %>% filter(!is.na(Price))

# Remove journals when relativePrice is NA
df_NAclean <- df_NAclean %>% filter(!is.na(relativePrice))



cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))


df_relativePrice2010 <- df_NAclean


sf3B <- ggplot(df_NAclean, aes(x = year, y = relativePrice, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Article processing charge", x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 4), breaks = seq(1, 4, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



# Relative price compared to 2000 for each journal
base_year <- 2015

df_price <- df_combine %>% group_by(journal) %>% mutate(relativePrice = Price/Price[year == base_year])


# Draw line graph for each journal, value on y, year on x
df_NAclean <- df_price %>% filter(!is.na(Price))

# Remove journals when relativePrice is NA
df_NAclean <- df_NAclean %>% filter(!is.na(relativePrice))



cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))




sf3C <- ggplot(df_NAclean, aes(x = year, y = relativePrice, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Article processing charge", x = "Year", y = "Relative rate from 2015") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )


























































# Read inflation data

# df_inflation <- read.csv(file = 'consumer-price-index.csv', header = TRUE)

df_inflation <- read.csv(file = 'inflation-of-consumer-prices_2.csv', header = TRUE)
df_inflation$Inflation2 <- df_inflation$Inflation * 0.01 + 1



#change column names
colnames(df_inflation)[1] <- "Country"
# colnames(df_inflation)[4] <- "Inflation"
colnames(df_inflation)[5] <- "Inflation"

# keep 15 countries
df_inflation <- df_inflation %>% filter(Country == "United States"
                                        | Country == "China"
                                        | Country == "United Kingdom"
                                        | Country == "Germany"
                                        | Country == "Japan"
                                        | Country == "France"
                                        | Country == "India"
                                        | Country == "Italy"
                                        | Country == "Canada"
                                        | Country == "Australia"
                                        | Country == "Spain"
                                        | Country == "Russia"
                                        | Country == "South Korea"
                                        | Country == "Brazil"
                                        | Country == "Netherlands")











# Relative price compared to 2000 for each journal
base_year <- 2010 # 2005, 2010, 2015, 2020



# df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = Inflation/Inflation[Year == base_year])


# multiply inflation rate from oldest year by country
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = cumprod(Inflation))
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = relative/relative[Year == base_year])



# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_inflation$Country)))

cols <- as.character(cols)



df_inflation <- df_inflation %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))

df_inflation2010 <- df_inflation



f2A <- ggplot(df_inflation, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Consumer price index", x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 4), breaks = seq(1, 4, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )

# 1200x800
















# Relative price compared to 2000 for each journal
base_year <- 2005 # 2005, 2010, 2015, 2020



# df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = Inflation/Inflation[Year == base_year])


# multiply inflation rate from oldest year by country
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = cumprod(Inflation))
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = relative/relative[Year == base_year])



# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_inflation$Country)))

cols <- as.character(cols)



df_inflation <- df_inflation %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))





sf3D <- ggplot(df_inflation, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Consumer price index", x = "Year", y = "Relative rate from 2005") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 7), breaks = seq(1, 7, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



# Relative price compared to 2000 for each journal
base_year <- 2010 # 2005, 2010, 2015, 2020



# df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = Inflation/Inflation[Year == base_year])


# multiply inflation rate from oldest year by country
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = cumprod(Inflation))
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = relative/relative[Year == base_year])



# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_inflation$Country)))

cols <- as.character(cols)



df_inflation <- df_inflation %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))





sf3E <- ggplot(df_inflation, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Consumer price index", x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 4), breaks = seq(1, 4, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )




# Relative price compared to 2000 for each journal
base_year <- 2015 # 2005, 2010, 2015, 2020



# df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = Inflation/Inflation[Year == base_year])


# multiply inflation rate from oldest year by country
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = cumprod(Inflation))
df_inflation <- df_inflation %>% group_by(Country) %>% mutate(relative = relative/relative[Year == base_year])



# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_inflation$Country)))

cols <- as.character(cols)



df_inflation <- df_inflation %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))





sf3F <- ggplot(df_inflation, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Consumer price index", x = "Year", y = "Relative rate from 2015") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )

























































### Research budget

### Population
df_population <- read.csv("population_OWD.csv", header = TRUE)

#change column names
colnames(df_population)[1] <- "Country"
colnames(df_population)[4] <- "Population"

# keep 15 countries
df_population <- df_population %>% filter(Country == "United States"
                                        | Country == "China"
                                        | Country == "United Kingdom"
                                        | Country == "Germany"
                                        | Country == "Japan"
                                        | Country == "France"
                                        | Country == "India"
                                        | Country == "Italy"
                                        | Country == "Canada"
                                        | Country == "Australia"
                                        | Country == "Spain"
                                        | Country == "Russia"
                                        | Country == "South Korea"
                                        | Country == "Brazil"
                                        | Country == "Netherlands")


### GBD
df_gbd <- read.csv("gdp-per-capita-worldbank.csv", header = TRUE)

#change column names
colnames(df_gbd)[1] <- "Country"
colnames(df_gbd)[4] <- "GBDperCapita"

# keep 15 countries
df_gbd <- df_gbd %>% filter(Country == "United States"
                                        | Country == "China"
                                        | Country == "United Kingdom"
                                        | Country == "Germany"
                                        | Country == "Japan"
                                        | Country == "France"
                                        | Country == "India"
                                        | Country == "Italy"
                                        | Country == "Canada"
                                        | Country == "Australia"
                                        | Country == "Spain"
                                        | Country == "Russia"
                                        | Country == "South Korea"
                                        | Country == "Brazil"
                                        | Country == "Netherlands")


df_research_comb <- right_join(df_population, df_gbd, by = c("Country", "Code", "Year"))




### Research
df_research <- read.csv("research-spending-gdp.csv", header = TRUE)

#change column names
colnames(df_research)[1] <- "Country"
colnames(df_research)[4] <- "ResearchGBDpercent"

# keep 15 countries
df_research <- df_research %>% filter(Country == "United States"
                                        | Country == "China"
                                        | Country == "United Kingdom"
                                        | Country == "Germany"
                                        | Country == "Japan"
                                        | Country == "France"
                                        | Country == "India"
                                        | Country == "Italy"
                                        | Country == "Canada"
                                        | Country == "Australia"
                                        | Country == "Spain"
                                        | Country == "Russia"
                                        | Country == "South Korea"
                                        | Country == "Brazil"
                                        | Country == "Netherlands")

df_research_comb <- left_join(df_research_comb, df_research, by = c("Country", "Code", "Year"))




df_research_comb$research_calc <- df_research_comb$Population * df_research_comb$GBDperCapita * df_research_comb$ResearchGBDpercent / 100


# Draw graph
df_NAclean <- df_research_comb %>% filter(!is.na(research_calc))



# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_NAclean$Country)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))






f2B1 <- ggplot(df_NAclean, aes(x = Year, y = ResearchGBDpercent, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Research & development spending (% in GDP)", x = "Year", y = "Percentage") +
  scale_x_continuous(expand = c(0,0), limits = c(2000, 2031), breaks = seq(2000, 2024, by = 4), ) +
  #scale_y_continuous(limits = c(NA, 7), breaks = seq(1, 7, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )

# 1200x800



f2B2 <- ggplot(df_NAclean, aes(x = Year, y = research_calc, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Research & development spending (real figure is USD)", x = "Year", y = "USD") +
  scale_x_continuous(expand = c(0,0), limits = c(2000, 2031), breaks = seq(2000, 2024, by = 4), ) +
  #scale_y_continuous(limits = c(NA, 7), breaks = seq(1, 7, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )

# 1200x800









# Relative
base_year <- 2010 # 2005, 2010, 2015, 2020

df_research_comb_original <- df_research_comb
# df_research_comb <- df_research_comb_original

# For australia, copy values in odd years from previous years
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 1 & df_research_comb$Year <= 2010, lag(df_research_comb$research_calc), df_research_comb$research_calc)
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 0 & df_research_comb$Year >= 2011, lag(df_research_comb$research_calc), df_research_comb$research_calc)




df_research_comb <- df_research_comb %>% group_by(Country) %>% mutate(relative = research_calc/research_calc[Year == base_year])

df_NAclean <- df_research_comb %>% filter(!is.na(relative))




# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_NAclean$Country)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))

df_relativeRD2010 <- df_NAclean



f2C <- ggplot(df_NAclean, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Relative research & development spending", x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 4), breaks = seq(1, 4, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )

# 1200x800





(f2A | f2B1) / (f2B2 | f2C) / (f2D | f2E) +
  plot_layout(heights = c(1,1,1), widths = c(1,1)) +
  
  #  (pA / pA2 / pB / pC) +  
  #    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA / pB / pC) | (pD / pD2 / pE / pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA | pB | pC) / (pD | pD2 | pE | pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1)) +
  
  plot_annotation(
    #    title = "Title",
    #    subtitle = "Subtitle",
    #    caption = "Caption",
    tag_levels = "A",
    ##    tag_prefix = "fig ",
    ##    tag_suffix = ":"
  ) & 
  theme(plot.tag = element_text(size = 36))

# 2400x2400
# fig2

























# Relative
base_year <- 2005 # 2005, 2010, 2015, 2020

df_research_comb_original <- df_research_comb
# df_research_comb <- df_research_comb_original

# For australia, copy values in odd years from previous years
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 1 & df_research_comb$Year <= 2010, lag(df_research_comb$research_calc), df_research_comb$research_calc)
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 0 & df_research_comb$Year >= 2011, lag(df_research_comb$research_calc), df_research_comb$research_calc)




df_research_comb <- df_research_comb %>% group_by(Country) %>% mutate(relative = research_calc/research_calc[Year == base_year])

df_NAclean <- df_research_comb %>% filter(!is.na(relative))




# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_NAclean$Country)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))





sf3G <- ggplot(df_NAclean, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Research & development spending", x = "Year", y = "Relative rate from 2005") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 7), breaks = seq(1, 7, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )




# Relative
base_year <- 2010 # 2005, 2010, 2015, 2020

df_research_comb_original <- df_research_comb
# df_research_comb <- df_research_comb_original

# For australia, copy values in odd years from previous years
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 1 & df_research_comb$Year <= 2010, lag(df_research_comb$research_calc), df_research_comb$research_calc)
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 0 & df_research_comb$Year >= 2011, lag(df_research_comb$research_calc), df_research_comb$research_calc)




df_research_comb <- df_research_comb %>% group_by(Country) %>% mutate(relative = research_calc/research_calc[Year == base_year])

df_NAclean <- df_research_comb %>% filter(!is.na(relative))




# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_NAclean$Country)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))





sf3H <- ggplot(df_NAclean, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Research & development spending", x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 4), breaks = seq(1, 4, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



# Relative
base_year <- 2015 # 2005, 2010, 2015, 2020

df_research_comb_original <- df_research_comb
# df_research_comb <- df_research_comb_original

# For australia, copy values in odd years from previous years
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 1 & df_research_comb$Year <= 2010, lag(df_research_comb$research_calc), df_research_comb$research_calc)
df_research_comb$research_calc <- ifelse(df_research_comb$Country == "Australia" & df_research_comb$Year %% 2 == 0 & df_research_comb$Year >= 2011, lag(df_research_comb$research_calc), df_research_comb$research_calc)




df_research_comb <- df_research_comb %>% group_by(Country) %>% mutate(relative = research_calc/research_calc[Year == base_year])

df_NAclean <- df_research_comb %>% filter(!is.na(relative))




# Draw line graph for each journal, value on y, year on x

cols <- light.colors(length(unique(df_NAclean$Country)))

cols <- as.character(cols)



df_NAclean <- df_NAclean %>% group_by(Country) %>% mutate(name_lab = if_else(Year == max(Year), Country, NA_character_))





sf3I <- ggplot(df_NAclean, aes(x = Year, y = relative, group = Country, color = Country)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Research & development spending", x = "Year", y = "Relative rate from 2015") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = Country, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )















(sf3A | sf3D | sf3G) / (sf3B | sf3E | sf3H) / (sf3C | sf3F | sf3I) +
  plot_layout(heights = c(1,1,1), widths = c(10,10,10)) +
  
  #  (pA / pA2 / pB / pC) +  
  #    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA / pB / pC) | (pD / pD2 / pE / pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA | pB | pC) / (pD | pD2 | pE | pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1)) +
  
  plot_annotation(
    #    title = "Title",
    #    subtitle = "Subtitle",
    #    caption = "Caption",
    tag_levels = "A",
    ##    tag_prefix = "fig ",
    ##    tag_suffix = ":"
  ) & 
  theme(plot.tag = element_text(size = 36))

# 4000x3000
# now sf2
























































base_year <- 2010

# APC / eco-inflation or RD inflation
# select country
countries_list <- c("United States", "China", "United Kingdom", "Germany", "Japan", "France", "India", "Italy", "Canada", "Australia", "Spain", "Russia", "South Korea", "Brazil", "Netherlands")
countries_list <- sort(countries_list)



country_select <- countries_list[1]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev1A <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0A <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[2]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev1B <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0B <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[3]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1C <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0C <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[4]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1D <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0D <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[5]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1E <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0E <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[6]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1F <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0F <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[7]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev1G <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0G <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[8]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1H <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0H <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[9]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1I <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0I <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[10]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1J <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0J <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[11]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev1K <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0K <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[12]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1L <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0L <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[13]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1M <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0M <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[14]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1N <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0N <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










country_select <- countries_list[15]

df_relativeRD2010eachCountry <- df_relativeRD2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeRD2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2021, df_NAclean$journal, NA) 

sfRev1O <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )



df_relativeInflation2010eachCountry <- df_inflation2010 %>% filter(Country == country_select)
df_relativePrice2010_relativeCountry <- left_join(df_relativePrice2010, df_relativeInflation2010eachCountry, by = c("year"="Year"))
df_relativePrice2010_relativeCountry$relativerelative <- df_relativePrice2010_relativeCountry$relativePrice / df_relativePrice2010_relativeCountry$relative
df_NAclean <- df_relativePrice2010_relativeCountry

cols <- light.colors(length(unique(df_NAclean$journal)))

cols <- as.character(cols)

df_NAclean <- df_NAclean %>% group_by(journal) %>% mutate(name_lab = if_else(year == max(year), journal, NA_character_))
df_NAclean$name_lab <- ifelse(df_NAclean$year == 2020, df_NAclean$journal, NA) 

sfRev0O <- ggplot(df_NAclean, aes(x = year, y = relativerelative, group = journal, color = journal)) + geom_line(size=2, alpha=0.5) + # geom_point() +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = country_select, x = "Year", y = "Relative rate from 2010") +
  scale_x_continuous(expand = c(0,0), limits = c(base_year, 2031), breaks = seq(base_year, 2024, by = 2), ) +
  scale_y_continuous(limits = c(NA, 3), breaks = seq(1, 3, by = 1), ) +
  # add label of journal name
  # geom_text(data = df_NAclean %>% filter(year == 2024), aes(label = journal), hjust = +0.9, vjust = 0, size = 8) +
  # change legend title
  labs(color = "") +
  # change font size of x axis title and legend
  theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(size =28),
        axis.title.y = element_text(size = 28), axis.text.y = element_text(size =28),
        # title size
        plot.title = element_text(size = 32)) +
  theme(legend.position = "none") +
  geom_text_repel(
    aes(color = journal, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2025.0, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf
  )










(sfRev0A | sfRev0B | sfRev0C | sfRev0D) / (sfRev0E | sfRev0F | sfRev0G | sfRev0H) / (sfRev0I | sfRev0J | sfRev0K | sfRev0L) / (sfRev0M | sfRev0N | sfRev0O | plot_spacer()) +
  plot_layout(heights = c(1,1,1,1), widths = c(10,10,10,10)) +
  
  #  (pA / pA2 / pB / pC) +  
  #    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA / pB / pC) | (pD / pD2 / pE / pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA | pB | pC) / (pD | pD2 | pE | pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1)) +
  
  plot_annotation(
    #    title = "Title",
    #    subtitle = "Subtitle",
    #    caption = "Caption",
    tag_levels = "A",
    ##    tag_prefix = "fig ",
    ##    tag_suffix = ":"
  ) & 
  theme(plot.tag = element_text(size = 36))

# 4000x2500
# now sf1: APC/inflation










(sfRev1A | sfRev1B | sfRev1C | sfRev1D) / (sfRev1E | sfRev1F | sfRev1G | sfRev1H) / (sfRev1I | sfRev1J | sfRev1K | sfRev1L) / (sfRev1M | sfRev1N | sfRev1O | plot_spacer()) +
  plot_layout(heights = c(1,1,1,1), widths = c(10,10,10,10)) +
  
  #  (pA / pA2 / pB / pC) +  
  #    plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA / pB / pC) | (pD / pD2 / pE / pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1, 1, 1)) +
  
  # (pA | pB | pC) / (pD | pD2 | pE | pF) +  
  # plot_layout(ncol = 1, heights = c(1, 1)) +
  
  plot_annotation(
    #    title = "Title",
    #    subtitle = "Subtitle",
    #    caption = "Caption",
    tag_levels = "A",
    ##    tag_prefix = "fig ",
    ##    tag_suffix = ":"
  ) & 
  theme(plot.tag = element_text(size = 36))

# 4000x2500
# now sf3: APC/RD



























































































































































#### Association analysis







# Remove journals
df_combine <- df_combine %>% filter(journal != "N Engl J Med" &
                                      journal != "Lancet" &
                                      journal != "BMJ" &
                                      journal != "Nature" &
                                      journal != "Science" &
                                      journal != "JAMA" &
                                      journal != "Cell" &
                                      journal != "Front Med")
























### Causal association








df_shift <- df_combine

# Shift the value of IF for the next year for each journal
df_shift <- df_shift %>% group_by(journal) %>% mutate(ImpFac = lag(ImpFac))
df_shift <- df_shift %>% group_by(journal) %>% mutate(rateImpFac = lag(rateImpFac))

# Shift the value of PubNum for the previous year for each journal
# df_shift <- df_shift %>% group_by(journal) %>% mutate(PubNum = lead(PubNum))
# df_shift <- df_shift %>% group_by(journal) %>% mutate(ratePubNum = lead(ratePubNum))

# Rate in log10
df_shift$rateImpFac <- log10(df_shift$rateImpFac)
df_shift$ratePubNum <- log10(df_shift$ratePubNum)
df_shift$ratePrice <- log10(df_shift$ratePrice)

# Inf to NA
df_shift$rateImpFac <- ifelse(df_shift$rateImpFac == Inf, NA, df_shift$rateImpFac)
# rateImpFac <- -1 when journal=Biomed Res Int, year=2023
df_shift$rateImpFac <- ifelse(df_shift$journal == "Biomed Res Int" & df_shift$year == 2023, -1, df_shift$rateImpFac)
df_shift$rateImpFac <- ifelse(df_shift$journal == "Biomed Res Int" & df_shift$year == 2024, 1, df_shift$rateImpFac)

# test stationality
test <- df_shift %>% filter(journal == "PLoS One")
test2 <- test$PubNum
#omit NA
test3 <- test2[!is.na(test2)]

# test3 <- diff(test3)

adf.test(test3)

















### Cross-correlation Function
### Example
# Create two time series
set.seed(123)
ts1 <- cumsum(rnorm(100, mean=100, sd=10))
ts2 <- lag(ts1, n=3) + rnorm(100, mean=10, sd=10)  # ts2 is ts1 lagged by 3

ts1 <- diff(ts1)
ts2 <- diff(ts2)

# Use the ccf function to calculate cross-correlation
ccf_result <- ccf(ts1, ts2, lag.max=10, plot=TRUE, na.action = na.pass)

# Display the result
ccf_result


data_length <- length(ts1)
std_error <- 1 / sqrt(data_length)
z_scores <- ccf_result$acf / std_error
p_values <- 2 * (1 - pnorm(abs(z_scores)))

p_values
### Example end








### Negative means ts2 happens later than ts1 (when moving ts2 negatively (past), then match)
### (Positive meant ts2 happens earlier than ts1 (when moving ts2 positively (future), then match))


























### Cross-correlation Function between rateImpFac and ratePubNum
result_list <- list()
name_list <- list()
p_list <- list()
test_num <- 0

for (i in unique(df_shift$journal)) {
  test_num <- test_num + 1
  
  df_temp <- df_shift %>% filter(journal == i)
  ts1 <- ts(df_temp$rateImpFac)
  
  ts2 <- ts(df_temp$ratePubNum)
  ccf_result <- ccf(ts1, ts2, lag.max=4, plot=FALSE, na.action = na.pass)
  
  data_length <- length(ts1)
  std_error <- 1 / sqrt(data_length)
  z_scores <- ccf_result$acf / std_error
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  result_list[[test_num]] <- ccf_result
  name_list[[test_num]] <- i
  p_list [[test_num]] <- p_values
}




journal <- c()
correlation <- c()
pvalue <- c()
lagtime <- c()

test_num <- 0

for (i in 1:length(name_list)) {
  test_num <- test_num + 1
  
  journal <- c(journal, rep(name_list[[test_num]], 9))
  correlation <- c(correlation, result_list[[test_num]]$acf)
  pvalue <- c(pvalue, p_list[[test_num]])
  lagtime <- c(lagtime, seq(-4, 4, 1))
}

ccf_ImpFac_Rate_PubNum <- cbind(journal, correlation, pvalue, lagtime)
ccf_ImpFac_Rate_PubNum <- as.data.frame(ccf_ImpFac_Rate_PubNum)

ccf_ImpFac_Rate_PubNum$correlation <- as.numeric(ccf_ImpFac_Rate_PubNum$correlation)
ccf_ImpFac_Rate_PubNum$pvalue <- as.numeric(ccf_ImpFac_Rate_PubNum$pvalue)
ccf_ImpFac_Rate_PubNum$lagtime <- as.numeric(ccf_ImpFac_Rate_PubNum$lagtime)

# Keep only negative lags
ccf_ImpFac_Rate_PubNum <- ccf_ImpFac_Rate_PubNum %>% filter(lagtime < 0)




write.table(ccf_ImpFac_Rate_PubNum,file="clipboard-16384",sep="\t")






































### Cross-correlation Function ratePubNum then rateImpFac
result_list <- list()
name_list <- list()
p_list <- list()
test_num <- 0

for (i in unique(df_shift$journal)) {
  test_num <- test_num + 1
  
  df_temp <- df_shift %>% filter(journal == i)
  ts2 <- ts(df_temp$rateImpFac)
  
  ts1 <- ts(df_temp$ratePubNum)
  ccf_result <- ccf(ts1, ts2, lag.max=4, plot=FALSE, na.action = na.pass)
  
  data_length <- length(ts1)
  std_error <- 1 / sqrt(data_length)
  z_scores <- ccf_result$acf / std_error
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  result_list[[test_num]] <- ccf_result
  name_list[[test_num]] <- i
  p_list [[test_num]] <- p_values
}




journal <- c()
correlation <- c()
pvalue <- c()
lagtime <- c()

test_num <- 0

for (i in 1:length(name_list)) {
  test_num <- test_num + 1
  
  journal <- c(journal, rep(name_list[[test_num]], 9))
  correlation <- c(correlation, result_list[[test_num]]$acf)
  pvalue <- c(pvalue, p_list[[test_num]])
  lagtime <- c(lagtime, seq(-4, 4, 1))
}

ccf_PubNum_Rate_ImpFac <- cbind(journal, correlation, pvalue, lagtime)
ccf_PubNum_Rate_ImpFac <- as.data.frame(ccf_PubNum_Rate_ImpFac)

ccf_PubNum_Rate_ImpFac$correlation <- as.numeric(ccf_PubNum_Rate_ImpFac$correlation)
ccf_PubNum_Rate_ImpFac$pvalue <- as.numeric(ccf_PubNum_Rate_ImpFac$pvalue)
ccf_PubNum_Rate_ImpFac$lagtime <- as.numeric(ccf_PubNum_Rate_ImpFac$lagtime)

# Keep only negative lags
ccf_PubNum_Rate_ImpFac <- ccf_PubNum_Rate_ImpFac %>% filter(lagtime < 0)




write.table(ccf_PubNum_Rate_ImpFac,file="clipboard-16384",sep="\t")









































































### Cross-correlation Function between rateImpFac and ratePrice
result_list <- list()
name_list <- list()
p_list <- list()
test_num <- 0

for (i in unique(df_shift$journal)) {
  test_num <- test_num + 1
  
  df_temp <- df_shift %>% filter(journal == i)
  ts1 <- ts(df_temp$rateImpFac)
  
  ts2 <- ts(df_temp$ratePrice)
  ccf_result <- ccf(ts1, ts2, lag.max=4, plot=FALSE, na.action = na.pass)
  
  data_length <- length(ts1)
  std_error <- 1 / sqrt(data_length)
  z_scores <- ccf_result$acf / std_error
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  result_list[[test_num]] <- ccf_result
  name_list[[test_num]] <- i
  p_list [[test_num]] <- p_values
}




journal <- c()
correlation <- c()
pvalue <- c()
lagtime <- c()

test_num <- 0

for (i in 1:length(name_list)) {
  test_num <- test_num + 1
  
  journal <- c(journal, rep(name_list[[test_num]], 9))
  correlation <- c(correlation, result_list[[test_num]]$acf)
  pvalue <- c(pvalue, p_list[[test_num]])
  lagtime <- c(lagtime, seq(-4, 4, 1))
}

ccf_ImpFac_Rate_Price <- cbind(journal, correlation, pvalue, lagtime)
ccf_ImpFac_Rate_Price <- as.data.frame(ccf_ImpFac_Rate_Price)

ccf_ImpFac_Rate_Price$correlation <- as.numeric(ccf_ImpFac_Rate_Price$correlation)
ccf_ImpFac_Rate_Price$pvalue <- as.numeric(ccf_ImpFac_Rate_Price$pvalue)
ccf_ImpFac_Rate_Price$lagtime <- as.numeric(ccf_ImpFac_Rate_Price$lagtime)

# Keep only negative lags
ccf_ImpFac_Rate_Price <- ccf_ImpFac_Rate_Price %>% filter(lagtime < 0)





write.table(ccf_ImpFac_Rate_Price,file="clipboard-16384",sep="\t")




































### Cross-correlation Function, ratePrice then rateImpFac
result_list <- list()
name_list <- list()
p_list <- list()
test_num <- 0

for (i in unique(df_shift$journal)) {
  test_num <- test_num + 1
  
  df_temp <- df_shift %>% filter(journal == i)
  ts2 <- ts(df_temp$rateImpFac)
  
  ts1 <- ts(df_temp$ratePrice)
  ccf_result <- ccf(ts1, ts2, lag.max=4, plot=FALSE, na.action = na.pass)
  
  data_length <- length(ts1)
  std_error <- 1 / sqrt(data_length)
  z_scores <- ccf_result$acf / std_error
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  result_list[[test_num]] <- ccf_result
  name_list[[test_num]] <- i
  p_list [[test_num]] <- p_values
}




journal <- c()
correlation <- c()
pvalue <- c()
lagtime <- c()

test_num <- 0

for (i in 1:length(name_list)) {
  test_num <- test_num + 1
  
  journal <- c(journal, rep(name_list[[test_num]], 9))
  correlation <- c(correlation, result_list[[test_num]]$acf)
  pvalue <- c(pvalue, p_list[[test_num]])
  lagtime <- c(lagtime, seq(-4, 4, 1))
}

ccf_Price_Rate_ImpFac <- cbind(journal, correlation, pvalue, lagtime)
ccf_Price_Rate_ImpFac <- as.data.frame(ccf_Price_Rate_ImpFac)

ccf_Price_Rate_ImpFac$correlation <- as.numeric(ccf_Price_Rate_ImpFac$correlation)
ccf_Price_Rate_ImpFac$pvalue <- as.numeric(ccf_Price_Rate_ImpFac$pvalue)
ccf_Price_Rate_ImpFac$lagtime <- as.numeric(ccf_Price_Rate_ImpFac$lagtime)

# Keep only negative lags
ccf_Price_Rate_ImpFac <- ccf_Price_Rate_ImpFac %>% filter(lagtime < 0)





write.table(ccf_Price_Rate_ImpFac,file="clipboard-16384",sep="\t")


































































### Cross-correlation Function between ratePubNum and ratePrice
result_list <- list()
name_list <- list()
p_list <- list()
test_num <- 0

for (i in unique(df_shift$journal)) {
  test_num <- test_num + 1
  
  df_temp <- df_shift %>% filter(journal == i)
  ts1 <- ts(df_temp$ratePubNum)
  
  ts2 <- ts(df_temp$ratePrice)
  ccf_result <- ccf(ts1, ts2, lag.max=4, plot=FALSE, na.action = na.pass)
  
  data_length <- length(ts1)
  std_error <- 1 / sqrt(data_length)
  z_scores <- ccf_result$acf / std_error
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  result_list[[test_num]] <- ccf_result
  name_list[[test_num]] <- i
  p_list [[test_num]] <- p_values
}




journal <- c()
correlation <- c()
pvalue <- c()
lagtime <- c()

test_num <- 0

for (i in 1:length(name_list)) {
  test_num <- test_num + 1
  
  journal <- c(journal, rep(name_list[[test_num]], 9))
  correlation <- c(correlation, result_list[[test_num]]$acf)
  pvalue <- c(pvalue, p_list[[test_num]])
  lagtime <- c(lagtime, seq(-4, 4, 1))
}

ccf_PubNum_Rate_Price <- cbind(journal, correlation, pvalue, lagtime)
ccf_PubNum_Rate_Price <- as.data.frame(ccf_PubNum_Rate_Price)

ccf_PubNum_Rate_Price$correlation <- as.numeric(ccf_PubNum_Rate_Price$correlation)
ccf_PubNum_Rate_Price$pvalue <- as.numeric(ccf_PubNum_Rate_Price$pvalue)
ccf_PubNum_Rate_Price$lagtime <- as.numeric(ccf_PubNum_Rate_Price$lagtime)

# Keep only negative lags
ccf_PubNum_Rate_Price <- ccf_PubNum_Rate_Price %>% filter(lagtime < 0)







write.table(ccf_PubNum_Rate_Price,file="clipboard-16384",sep="\t")



















































### Cross-correlation Function, ratePrice then ratePubNum
result_list <- list()
name_list <- list()
p_list <- list()
test_num <- 0

for (i in unique(df_shift$journal)) {
  test_num <- test_num + 1
  
  df_temp <- df_shift %>% filter(journal == i)
  ts2 <- ts(df_temp$ratePubNum)
  
  ts1 <- ts(df_temp$ratePrice)
  ccf_result <- ccf(ts1, ts2, lag.max=4, plot=FALSE, na.action = na.pass)
  
  data_length <- length(ts1)
  std_error <- 1 / sqrt(data_length)
  z_scores <- ccf_result$acf / std_error
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  result_list[[test_num]] <- ccf_result
  name_list[[test_num]] <- i
  p_list [[test_num]] <- p_values
}




journal <- c()
correlation <- c()
pvalue <- c()
lagtime <- c()

test_num <- 0

for (i in 1:length(name_list)) {
  test_num <- test_num + 1
  
  journal <- c(journal, rep(name_list[[test_num]], 9))
  correlation <- c(correlation, result_list[[test_num]]$acf)
  pvalue <- c(pvalue, p_list[[test_num]])
  lagtime <- c(lagtime, seq(-4, 4, 1))
}

ccf_Price_Rate_PubNum <- cbind(journal, correlation, pvalue, lagtime)
ccf_Price_Rate_PubNum <- as.data.frame(ccf_Price_Rate_PubNum)

ccf_Price_Rate_PubNum$correlation <- as.numeric(ccf_Price_Rate_PubNum$correlation)
ccf_Price_Rate_PubNum$pvalue <- as.numeric(ccf_Price_Rate_PubNum$pvalue)
ccf_Price_Rate_PubNum$lagtime <- as.numeric(ccf_Price_Rate_PubNum$lagtime)

# Keep only negative lags
ccf_Price_Rate_PubNum <- ccf_Price_Rate_PubNum %>% filter(lagtime < 0)







write.table(ccf_Price_Rate_PubNum,file="clipboard-16384",sep="\t")





















































































































### CCF graphs ###
df_ccf <- read.csv("pub_fee_CCF6.csv", header = TRUE, sep = ",")

# Shourten name
# df_ccf$journal <- gsub("New England Journal of Medicine", "N Engl J Med", df_ccf$journal)
# df_ccf$journal <- gsub("Lancet Global Health", "Lancet Glob Health", df_ccf$journal)
# df_ccf$journal <- gsub("Nature Communications", "Nat Commun", df_ccf$journal)
# df_ccf$journal <- gsub("Science Advances", "Sci Adv", df_ccf$journal)
# df_ccf$journal <- gsub("JAMA Network Open", "JAMA Netw Open", df_ccf$journal)
# df_ccf$journal <- gsub("Cell Reports", "Cell Rep", df_ccf$journal)
# df_ccf$journal <- gsub("Genome Biology", "Genome Biol", df_ccf$journal)
# df_ccf$journal <- gsub("PLoS Biology", "PLoS Biol", df_ccf$journal)
# df_ccf$journal <- gsub("PLoS Medicine", "PLoS Med", df_ccf$journal)
# df_ccf$journal <- gsub("Frontiers in Neuroscience", "Front Neurosci", df_ccf$journal)
# df_ccf$journal <- gsub("Frontiers in Medicine", "Front Med", df_ccf$journal)
# df_ccf$journal <- gsub("Scientific Reports", "Sci Rep", df_ccf$journal)
# df_ccf$journal <- gsub("Frontiers in Psychiatry", "Front Psychiatry", df_ccf$journal)
# df_ccf$journal <- gsub("International Journal of Molecular Sciences", "Int J Mol Sci", df_ccf$journal)




# Remove some journal
df_ccf <- df_ccf %>% filter(journal != "Heliyon" &
                                journal != "JAMA Netw Open" # &
                                # journal != "Science Advances" &
                                # journal != "Frontiers in Psychiatry" &
                                # journal != "eBioMedicine"
                            )


# substitute "International Journal of Molecular Sciences" to "Int J Mol Sci"
# df_ccf$journal <- ifelse(df_ccf$journal == "International Journal of Molecular Sciences", "Int J Mol Sci", df_ccf$journal)

# change test names
df_ccf$test <- ifelse(df_ccf$test == "A_ImpFac_PubNum", "IF-Count",
                       ifelse(df_ccf$test == "B_PubNum_ImpFac", "Count-IF",
                              ifelse(df_ccf$test == "C_ImpFac_Price", "IF-Fee",
                                     ifelse(df_ccf$test == "D_Price_ImpFac", "Fee-IF",
                                            ifelse(df_ccf$test == "E_PubNum_Price", "Count-Fee",
                                                   ifelse(df_ccf$test == "F_Price_PubNum", "Fee-Count", NA))))))


df_ccf$test2 <- ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _1", "IF-Count_lag1",
                       ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _2", "IF-Count_lag2",
                              ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _3", "IF-Count_lag3",
                                     ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _4", "IF-Count_lag4",
                                            ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _1", "Count-IF_lag1",
                                                   ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _2", "Count-IF_lag2",
                                                          ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _3", "Count-IF_lag3",
                                                                 ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _4", "Count-IF_lag4",
                                                                        ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _1", "IF-Fee_lag1",
                                                                               ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _2", "IF-Fee_lag2",
                                                                                      ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _3", "IF-Fee_lag3",
                                                                                             ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _4", "IF-Fee_lag4",
                                                                                                    ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _1", "Fee-IF_lag1",
                                                                                                           ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _2", "Fee-IF_lag2",
                                                                                                                  ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _3", "Fee-IF_lag3",
                                                                                                                         ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _4", "Fee-IF_lag4",
                                                                                                                                ifelse(df_ccf$test2 == "E_PubNum_Price_lag _1", "Count-Fee_lag1",
                                                                                                                                       ifelse(df_ccf$test2 == "E_PubNum_Price_lag _2", "Count-Fee_lag2",
                                                                                                                                              ifelse(df_ccf$test2 == "E_PubNum_Price_lag _3", "Count-Fee_lag3",
                                                                                                                                                     ifelse(df_ccf$test2 == "E_PubNum_Price_lag _4", "Count-Fee_lag4",
                                                                                                                                                            ifelse(df_ccf$test2 == "F_Price_PubNum_lag _1", "Fee-Count_lag1",
                                                                                                                                                                   ifelse(df_ccf$test2 == "F_Price_PubNum_lag _2", "Fee-Count_lag2",
                                                                                                                                                                          ifelse(df_ccf$test2 == "F_Price_PubNum_lag _3", "Fee-Count_lag3",
                                                                                                                                                                                 ifelse(df_ccf$test2 == "F_Price_PubNum_lag _4", "Fee-Count_lag4", NA))))))))))))))))))))))))


# level change
df_ccf$journal <- factor(df_ccf$journal, levels = c("Cell Rep",
                                                      "Sci Adv",
                                                      "Nat Commun",
                                                      "Sci Rep",
                                                      "PLoS One",
                                                      "PLoS Biol",
                                                      "PLoS Med",
                                                      "Front Immunol",
                                                      "Front Neurosci",
                                                      "Front Psychiatry",
                                                      "Molecules",
                                                      "Int J Mol Sci",
                                                      "BMC Bioinformatics",
                                                      "BMC Public Health",
                                                      "Genome Biol",
                                                      "Mediators Inflamm",
                                                      "Biomed Res Int",
                                                      "Lancet Glob Health",
                                                      "eBioMedicine",
                                                      "BMJ Open"))



df_ccf$test <- factor(df_ccf$test, levels = c("Count-Fee",
                                              "IF-Fee",
                                              "Fee-Count",
                                              "Fee-IF",
                                              "Count-IF",
                                              "IF-Count"))
                                              



df_ccf$test2 <- factor(df_ccf$test2, levels = c("Count-Fee_lag1",
                                                "Count-Fee_lag2",
                                                "Count-Fee_lag3",
                                                "Count-Fee_lag4",
                                                "IF-Fee_lag1",
                                                "IF-Fee_lag2",
                                                "IF-Fee_lag3",
                                                "IF-Fee_lag4",
                                                "Fee-Count_lag1",
                                                "Fee-Count_lag2",
                                                "Fee-Count_lag3",
                                                "Fee-Count_lag4",
                                                "Fee-IF_lag1",
                                                "Fee-IF_lag2",
                                                "Fee-IF_lag3",
                                                "Fee-IF_lag4",
                                                "Count-IF_lag1",
                                                "Count-IF_lag2",
                                                "Count-IF_lag3",
                                                "Count-IF_lag4",
                                                "IF-Count_lag1",
                                                "IF-Count_lag2",
                                                "IF-Count_lag3",
                                                "IF-Count_lag4"))



# Remove  "Count-IF" & "IF-Count" rows
df_ccf_original <- df_ccf
# df_ccf <- df_ccf_original

df_ccf <- df_ccf %>% filter(test != "Count-IF" & test != "IF-Count")










df_ccf <- df_ccf %>% filter(test != "Fee-Count" & test != "Fee-IF")
df_ccf$test <- ifelse(df_ccf$test == "Count-Fee", "Publication count", as.character(df_ccf$test))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag1", "Publication count_lag1", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag2", "Publication count_lag2", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag3", "Publication count_lag3", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag4", "Publication count_lag4", as.character(df_ccf$test2))
df_ccf$test <- ifelse(df_ccf$test == "IF-Fee", "Impact factor", as.character(df_ccf$test))
df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag1", "Impact factor_lag1", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag2", "Impact factor_lag2", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag3", "Impact factor_lag3", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag4", "Impact factor_lag4", as.character(df_ccf$test2))









# df_ccf <- df_ccf %>% filter(test != "Count-Fee" & test != "IF-Fee")
# df_ccf$test <- ifelse(df_ccf$test == "Fee-Count", "Publication count", as.character(df_ccf$test))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag1", "Publication count_lag1", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag2", "Publication count_lag2", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag3", "Publication count_lag3", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag4", "Publication count_lag4", as.character(df_ccf$test2))
# df_ccf$test <- ifelse(df_ccf$test == "Fee-IF", "Impact factor", as.character(df_ccf$test))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag1", "Impact factor_lag1", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag2", "Impact factor_lag2", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag3", "Impact factor_lag3", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag4", "Impact factor_lag4", as.character(df_ccf$test2))









df_ccf$test <- factor(df_ccf$test, levels = c("Publication count",
                                              "Impact factor"))

df_ccf$test2 <- factor(df_ccf$test2, levels = c("Publication count_lag1",
                                                "Publication count_lag2",
                                                "Publication count_lag3",
                                                "Publication count_lag4",
                                                "Impact factor_lag1",
                                                "Impact factor_lag2",
                                                "Impact factor_lag3",
                                                "Impact factor_lag4"))


### Heatmap




df_ccf3 <- df_ccf
### keep only P <0.05
# df_ccf$correlation <- ifelse(df_ccf$pvalue < 0.05, df_ccf$correlation, 0)




### Correlation

# Spread the dataframe to wide format
# keep only journal, test2, correlation
df_ccf3 <- df_ccf3 %>% dplyr::select(journal, test2, correlation)

# Spread the dataframe to wide format
df_wide <- df_ccf3 %>%
  spread(key = test2, value = correlation)

# Extract the matrix and remove the first column which contains the row names
heatmap_matrix <- as.matrix(df_wide[, -1, drop = FALSE])
rownames(heatmap_matrix) <- df_wide$journal

# Define color function
col_fun <- colorRamp2(c(-1, 0, 1), c("red", "white", "green"))
na_color <- "gray"


# Create the heatmap
# sf6A
Heatmap(heatmap_matrix, 
        name = "Correlation",
        column_title = "Changes before APC change",
        column_title_gp = gpar(fontsize = 40, fontface = "bold"),
        col = col_fun,
        na_col = na_color,  # This parameter sets the color for NA values
        cluster_rows = FALSE,  # Clusters only rows
        cluster_columns = FALSE,  # No clustering on columns
        show_row_names = TRUE,
        show_column_names = TRUE,
        row_names_side = "left",
        # column_names_side = "top",
        rect_gp = gpar(col = "darkgray", lwd = 2),
        row_names_gp = gpar(fontsize = 20),
        column_names_gp = gpar(fontsize = 16),
        heatmap_legend_param = list(
          labels_gp = gpar(fontsize = 20),
          title_gp = gpar(fontsize = 20)))
        

#1000x1000









### Correlation select
### Extract strongest
df_ccf_extract <- df_ccf %>% group_by(test, journal) %>% top_n(-1, pvalue)
# df_ccf_extract <- df_ccf %>% group_by(test, journal) %>% top_n(-1, absCorrelation)

### keep only P <0.05
df_ccf_extract$correlation <- ifelse(df_ccf_extract$pvalue < 0.05, df_ccf_extract$correlation, 0)


# Spread the dataframe to wide format
# keep only journal, test2, correlation
df_ccf3 <- df_ccf_extract %>% dplyr::select(journal, test, correlation)


# Spread the dataframe to wide format
df_wide <- df_ccf3 %>%
  spread(key = test, value = correlation)

# Extract the matrix and remove the first column which contains the row names
heatmap_matrix <- as.matrix(df_wide[, -1, drop = FALSE])
rownames(heatmap_matrix) <- df_wide$journal

# Define color function
col_fun <- colorRamp2(c(-1, 0, 1), c("red", "white", "green"))
na_color <- "gray"


# Create the heatmap
# f3A
Heatmap(heatmap_matrix, 
        name = "Correlation",
        column_title = "Changes before APC change",
        column_title_gp = gpar(fontsize = 24, fontface = "bold"),
        col = col_fun,
        na_col = na_color,  # This parameter sets the color for NA values
        cluster_rows = FALSE,  # Clusters only rows
        cluster_columns = FALSE,  # No clustering on columns
        show_row_names = TRUE,
        show_column_names = TRUE,
        row_names_side = "left",
        # column_names_side = "top",
        rect_gp = gpar(col = "darkgray", lwd = 2),
        row_names_gp = gpar(fontsize = 20),
        column_names_gp = gpar(fontsize = 20),
        heatmap_legend_param = list(
          labels_gp = gpar(fontsize = 20),
          title_gp = gpar(fontsize = 20)))

# 600x1000






















































### CCF graphs ###
df_ccf <- read.csv("pub_fee_CCF6.csv", header = TRUE, sep = ",")

# Shourten name
# df_ccf$journal <- gsub("New England Journal of Medicine", "N Engl J Med", df_ccf$journal)
# df_ccf$journal <- gsub("Lancet Global Health", "Lancet Glob Health", df_ccf$journal)
# df_ccf$journal <- gsub("Nature Communications", "Nat Commun", df_ccf$journal)
# df_ccf$journal <- gsub("Science Advances", "Sci Adv", df_ccf$journal)
# df_ccf$journal <- gsub("JAMA Network Open", "JAMA Netw Open", df_ccf$journal)
# df_ccf$journal <- gsub("Cell Reports", "Cell Rep", df_ccf$journal)
# df_ccf$journal <- gsub("Genome Biology", "Genome Biol", df_ccf$journal)
# df_ccf$journal <- gsub("PLoS Biology", "PLoS Biol", df_ccf$journal)
# df_ccf$journal <- gsub("PLoS Medicine", "PLoS Med", df_ccf$journal)
# df_ccf$journal <- gsub("Frontiers in Neuroscience", "Front Neurosci", df_ccf$journal)
# df_ccf$journal <- gsub("Frontiers in Medicine", "Front Med", df_ccf$journal)
# df_ccf$journal <- gsub("Scientific Reports", "Sci Rep", df_ccf$journal)
# df_ccf$journal <- gsub("Frontiers in Psychiatry", "Front Psychiatry", df_ccf$journal)
# df_ccf$journal <- gsub("International Journal of Molecular Sciences", "Int J Mol Sci", df_ccf$journal)




# Remove some journal
df_ccf <- df_ccf %>% filter(journal != "Heliyon" &
                              journal != "JAMA Netw Open" # &
                            # journal != "Science Advances" &
                            # journal != "Frontiers in Psychiatry" &
                            # journal != "eBioMedicine"
)


# substitute "International Journal of Molecular Sciences" to "Int J Mol Sci"
# df_ccf$journal <- ifelse(df_ccf$journal == "International Journal of Molecular Sciences", "Int J Mol Sci", df_ccf$journal)

# change test names
df_ccf$test <- ifelse(df_ccf$test == "A_ImpFac_PubNum", "IF-Count",
                      ifelse(df_ccf$test == "B_PubNum_ImpFac", "Count-IF",
                             ifelse(df_ccf$test == "C_ImpFac_Price", "IF-Fee",
                                    ifelse(df_ccf$test == "D_Price_ImpFac", "Fee-IF",
                                           ifelse(df_ccf$test == "E_PubNum_Price", "Count-Fee",
                                                  ifelse(df_ccf$test == "F_Price_PubNum", "Fee-Count", NA))))))


df_ccf$test2 <- ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _1", "IF-Count_lag1",
                       ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _2", "IF-Count_lag2",
                              ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _3", "IF-Count_lag3",
                                     ifelse(df_ccf$test2 == "A_ImpFac_PubNum_lag _4", "IF-Count_lag4",
                                            ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _1", "Count-IF_lag1",
                                                   ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _2", "Count-IF_lag2",
                                                          ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _3", "Count-IF_lag3",
                                                                 ifelse(df_ccf$test2 == "B_PubNum_ImpFac_lag _4", "Count-IF_lag4",
                                                                        ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _1", "IF-Fee_lag1",
                                                                               ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _2", "IF-Fee_lag2",
                                                                                      ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _3", "IF-Fee_lag3",
                                                                                             ifelse(df_ccf$test2 == "C_ImpFac_Price_lag _4", "IF-Fee_lag4",
                                                                                                    ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _1", "Fee-IF_lag1",
                                                                                                           ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _2", "Fee-IF_lag2",
                                                                                                                  ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _3", "Fee-IF_lag3",
                                                                                                                         ifelse(df_ccf$test2 == "D_Price_ImpFac_lag _4", "Fee-IF_lag4",
                                                                                                                                ifelse(df_ccf$test2 == "E_PubNum_Price_lag _1", "Count-Fee_lag1",
                                                                                                                                       ifelse(df_ccf$test2 == "E_PubNum_Price_lag _2", "Count-Fee_lag2",
                                                                                                                                              ifelse(df_ccf$test2 == "E_PubNum_Price_lag _3", "Count-Fee_lag3",
                                                                                                                                                     ifelse(df_ccf$test2 == "E_PubNum_Price_lag _4", "Count-Fee_lag4",
                                                                                                                                                            ifelse(df_ccf$test2 == "F_Price_PubNum_lag _1", "Fee-Count_lag1",
                                                                                                                                                                   ifelse(df_ccf$test2 == "F_Price_PubNum_lag _2", "Fee-Count_lag2",
                                                                                                                                                                          ifelse(df_ccf$test2 == "F_Price_PubNum_lag _3", "Fee-Count_lag3",
                                                                                                                                                                                 ifelse(df_ccf$test2 == "F_Price_PubNum_lag _4", "Fee-Count_lag4", NA))))))))))))))))))))))))


# level change
df_ccf$journal <- factor(df_ccf$journal, levels = c("Cell Rep",
                                                    "Sci Adv",
                                                    "Nat Commun",
                                                    "Sci Rep",
                                                    "PLoS One",
                                                    "PLoS Biol",
                                                    "PLoS Med",
                                                    "Front Immunol",
                                                    "Front Neurosci",
                                                    "Front Psychiatry",
                                                    "Molecules",
                                                    "Int J Mol Sci",
                                                    "BMC Bioinformatics",
                                                    "BMC Public Health",
                                                    "Genome Biol",
                                                    "Mediators Inflamm",
                                                    "Biomed Res Int",
                                                    "Lancet Glob Health",
                                                    "eBioMedicine",
                                                    "BMJ Open"))



df_ccf$test <- factor(df_ccf$test, levels = c("Count-Fee",
                                              "IF-Fee",
                                              "Fee-Count",
                                              "Fee-IF",
                                              "Count-IF",
                                              "IF-Count"))




df_ccf$test2 <- factor(df_ccf$test2, levels = c("Count-Fee_lag1",
                                                "Count-Fee_lag2",
                                                "Count-Fee_lag3",
                                                "Count-Fee_lag4",
                                                "IF-Fee_lag1",
                                                "IF-Fee_lag2",
                                                "IF-Fee_lag3",
                                                "IF-Fee_lag4",
                                                "Fee-Count_lag1",
                                                "Fee-Count_lag2",
                                                "Fee-Count_lag3",
                                                "Fee-Count_lag4",
                                                "Fee-IF_lag1",
                                                "Fee-IF_lag2",
                                                "Fee-IF_lag3",
                                                "Fee-IF_lag4",
                                                "Count-IF_lag1",
                                                "Count-IF_lag2",
                                                "Count-IF_lag3",
                                                "Count-IF_lag4",
                                                "IF-Count_lag1",
                                                "IF-Count_lag2",
                                                "IF-Count_lag3",
                                                "IF-Count_lag4"))



# Remove  "Count-IF" & "IF-Count" rows
df_ccf_original <- df_ccf
# df_ccf <- df_ccf_original

df_ccf <- df_ccf %>% filter(test != "Count-IF" & test != "IF-Count")










# df_ccf <- df_ccf %>% filter(test != "Fee-Count" & test != "Fee-IF")
# df_ccf$test <- ifelse(df_ccf$test == "Count-Fee", "Publication count", as.character(df_ccf$test))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag1", "Publication count_lag1", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag2", "Publication count_lag2", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag3", "Publication count_lag3", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "Count-Fee_lag4", "Publication count_lag4", as.character(df_ccf$test2))
# df_ccf$test <- ifelse(df_ccf$test == "IF-Fee", "Impact factor", as.character(df_ccf$test))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag1", "Impact factor_lag1", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag2", "Impact factor_lag2", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag3", "Impact factor_lag3", as.character(df_ccf$test2))
# df_ccf$test2 <- ifelse(df_ccf$test2 == "IF-Fee_lag4", "Impact factor_lag4", as.character(df_ccf$test2))









df_ccf <- df_ccf %>% filter(test != "Count-Fee" & test != "IF-Fee")
df_ccf$test <- ifelse(df_ccf$test == "Fee-Count", "Publication count", as.character(df_ccf$test))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag1", "Publication count_lag1", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag2", "Publication count_lag2", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag3", "Publication count_lag3", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-Count_lag4", "Publication count_lag4", as.character(df_ccf$test2))
df_ccf$test <- ifelse(df_ccf$test == "Fee-IF", "Impact factor", as.character(df_ccf$test))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag1", "Impact factor_lag1", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag2", "Impact factor_lag2", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag3", "Impact factor_lag3", as.character(df_ccf$test2))
df_ccf$test2 <- ifelse(df_ccf$test2 == "Fee-IF_lag4", "Impact factor_lag4", as.character(df_ccf$test2))









df_ccf$test <- factor(df_ccf$test, levels = c("Publication count",
                                              "Impact factor"))

df_ccf$test2 <- factor(df_ccf$test2, levels = c("Publication count_lag1",
                                                "Publication count_lag2",
                                                "Publication count_lag3",
                                                "Publication count_lag4",
                                                "Impact factor_lag1",
                                                "Impact factor_lag2",
                                                "Impact factor_lag3",
                                                "Impact factor_lag4"))


### Heatmap




df_ccf3 <- df_ccf
### keep only P <0.05
# df_ccf$correlation <- ifelse(df_ccf$pvalue < 0.05, df_ccf$correlation, 0)




### Correlation

# Spread the dataframe to wide format
# keep only journal, test2, correlation
df_ccf3 <- df_ccf3 %>% dplyr::select(journal, test2, correlation)

# Spread the dataframe to wide format
df_wide <- df_ccf3 %>%
  spread(key = test2, value = correlation)

# Extract the matrix and remove the first column which contains the row names
heatmap_matrix <- as.matrix(df_wide[, -1, drop = FALSE])
rownames(heatmap_matrix) <- df_wide$journal

# Define color function
col_fun <- colorRamp2(c(-1, 0, 1), c("red", "white", "green"))
na_color <- "gray"


# Create the heatmap
#sf6B
Heatmap(heatmap_matrix, 
                name = "Correlation",
                column_title = "Changes after APC change",
                column_title_gp = gpar(fontsize = 40, fontface = "bold"),
                col = col_fun,
                na_col = na_color,  # This parameter sets the color for NA values
                cluster_rows = FALSE,  # Clusters only rows
                cluster_columns = FALSE,  # No clustering on columns
                show_row_names = TRUE,
                show_column_names = TRUE,
                row_names_side = "left",
                # column_names_side = "top",
                rect_gp = gpar(col = "darkgray", lwd = 2),
                row_names_gp = gpar(fontsize = 20),
                column_names_gp = gpar(fontsize = 16),
                heatmap_legend_param = list(
                  labels_gp = gpar(fontsize = 20),
                  title_gp = gpar(fontsize = 20)))


#1000x1000









### Correlation select
### Extract strongest
df_ccf_extract <- df_ccf %>% group_by(test, journal) %>% top_n(-1, pvalue)
# df_ccf_extract <- df_ccf %>% group_by(test, journal) %>% top_n(-1, absCorrelation)

### keep only P <0.05
df_ccf_extract$correlation <- ifelse(df_ccf_extract$pvalue < 0.05, df_ccf_extract$correlation, 0)


# Spread the dataframe to wide format
# keep only journal, test2, correlation
df_ccf3 <- df_ccf_extract %>% dplyr::select(journal, test, correlation)


# Spread the dataframe to wide format
df_wide <- df_ccf3 %>%
  spread(key = test, value = correlation)

# Extract the matrix and remove the first column which contains the row names
heatmap_matrix <- as.matrix(df_wide[, -1, drop = FALSE])
rownames(heatmap_matrix) <- df_wide$journal

# Define color function
col_fun <- colorRamp2(c(-1, 0, 1), c("red", "white", "green"))
na_color <- "gray"


# Create the heatmap
# f3B

Heatmap(heatmap_matrix, 
               name = "Correlation",
               column_title = "Changes after APC change",
               column_title_gp = gpar(fontsize = 24, fontface = "bold"),
               col = col_fun,
               na_col = na_color,  # This parameter sets the color for NA values
               cluster_rows = FALSE,  # Clusters only rows
               cluster_columns = FALSE,  # No clustering on columns
               show_row_names = TRUE,
               show_column_names = TRUE,
               row_names_side = "left",
               # column_names_side = "top",
               rect_gp = gpar(col = "darkgray", lwd = 2),
               row_names_gp = gpar(fontsize = 20),
               column_names_gp = gpar(fontsize = 20),
               heatmap_legend_param = list(
                 labels_gp = gpar(fontsize = 20),
                 title_gp = gpar(fontsize = 20)))

# 600x1000





