import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pylab
from numpy.polynomial.polynomial import polyfit
from sklearn.metrics import r2_score
import math
import argparse
import scipy
import scipy.stats as stats
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run compare.py")
    parser.add_argument("-f", "--file", dest="file", required=True)
    parser.add_argument("-r", "--reference", dest="reference", required=True)
    parser.add_argument("-w", "--window", dest="window", required=True)
    args = parser.parse_args()
    dfShiny1 = pd.read_csv(args.file, sep=";")
    dfShiny2 = pd.read_csv(args.reference, sep=";")
    
    day = dfShiny1.groupby("Animal").agg({'HP': ['min', 'max', 'mean']})
    night = dfShiny2.groupby("Animal").agg({'HP': ['min', 'max', 'mean']})
    day = day[("HP", "min")]
    night = night[("HP", "min")]
    
    df = pd.DataFrame(
        data={
            "y": ["Day"] * len(day.index.tolist()) + ["Night"] * len(night.index.tolist()),
            "x" : day.tolist() + night.tolist(),
            "animals" : day.index.tolist() + night.index.tolist()
        }
    )

    import seaborn as sns
    ax = sns.boxplot(data=df, x="y", y="x")
    ax = sns.stripplot(data=df, x="y", y="x")
    plt.show()
