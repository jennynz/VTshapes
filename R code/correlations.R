## Inter-speaker and Intra-speaker Corre#ations

# Performs Pearson's product-moment correlations between the first and second
# principal components of each speaker with all other speakers, and then between
# the two sets of each speaker. Before running this script, please run
# readinMRIdata_oneSet.R and pcaAreaFunctions.R

# Adapted by Jenny Sahng
# 16/08/2016


# Inter-speaker Correlations

cor.test(pc.VTareas$x[,1],pc.resfreqIV$x[,1])
