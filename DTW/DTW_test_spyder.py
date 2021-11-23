# -*- coding: utf-8 -*-
"""
Created on Wed Nov 17 15:59:01 2021

@author: Sebastian
"""
# Load/reload relevant packages
from dtaidistance import dtw
from dtaidistance import dtw_visualisation as dtwvis
from dtaidistance import clustering
from sklearn import preprocessing
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


df_hist = pd.read_csv('../historical_data.csv')
df_hist.rename(columns={'Unnamed: 0': 'month'}, inplace=True)
df_hist.rename(columns={'Month': 'month_bw'}, inplace=True)
df_hist.fillna(0, inplace=True)

df_hist.head()


# Normlize entire dataframe (see: https://www.journaldev.com/45109/normalize-data-in-python)
# axis=0 for normalization in columns instead of rows
d = preprocessing.normalize(df_hist, axis=0)
df_hist_scaled = pd.DataFrame(d, columns=df_hist.columns)
df_hist_scaled = df_hist_scaled.drop(["month", "month_bw"], axis=1)
df_hist_scaled.head()


# Comparison of two timeseries
s1 = np.array(df_hist_scaled.iasian)
s2 = np.array(df_hist_scaled.iafric)
path = dtw.warping_path(s1, s2)
dtwvis.plot_warping(s1, s2, path, filename="Comparions_iasian_iafric.png")


# Comparison of multiple timeseries
df_array = np.transpose(np.array(df_hist_scaled))
distance_matrix = pd.DataFrame(dtw.distance_matrix(df_array),
                               columns=df_hist_scaled.columns,
                               index=df_hist_scaled.columns)

# Clustering
# Custom Hierarchical clustering
model1 = clustering.Hierarchical(dtw.distance_matrix, {})
cluster_idx = model1.fit(df_array)
# Augment Hierarchical object to keep track of the full tree
model2 = clustering.HierarchicalTree(model1)
cluster_idx = model2.fit(df_array)
# SciPy linkage clustering
model3 = clustering.LinkageTree(dtw.distance_matrix, {})
cluster_idx = model3.fit(df_array)

'''
model2.plot("HierachicalTree.png")
model3.plot("LinkageTree.png")
'''

# Plotting Dendrogram
# HierachicalTree
fig, ax = plt.subplots(nrows=1, ncols=2, figsize=(10, 15))
show_ts_label = df_hist_scaled.columns
model2.plot("HierachicalTree_Dendrogram.png", axes=ax, ts_height=1,
            bottom_margin=0, top_margin=0,
            show_ts_label=show_ts_label, ts_label_margin=-15,
            show_tr_label=True, tr_label_margin=0.1,
            ts_left_margin=5, ts_sample_length=1,
            tr_left_margin=2)

# LinkageTree
fig, ax = plt.subplots(nrows=1, ncols=2, figsize=(10, 15))
show_ts_label = df_hist_scaled.columns
model3.plot("LinkageTree_Dendrogram.png", axes=ax, ts_height=1,
            bottom_margin=0.1, top_margin=0,
            show_ts_label=show_ts_label, ts_label_margin=-15,
            show_tr_label=True, tr_label_margin=0.1,
            ts_left_margin=5, ts_sample_length=1,
            tr_left_margin=2)

fig.add_axes()
