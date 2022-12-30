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


def hist_plot(data1, data2):
    data1 = np.asarray(data1)
    data2 = np.asarray(data2)
    mean = np.mean([data1, data2], axis=0)
    diff = data1 - data2
    plt.hist(diff)


def calc_ks(data1, data2):
    data1 = np.asarray(data1)
    data2 = np.asarray(data2)
    mean = np.mean([data1, data2], axis=0)
    diff = data1 - data2
    return stats.kstest(diff, stats.norm.cdf)


def calc_shapiro(data1, data2):
    data1 = np.asarray(data1)
    data2 = np.asarray(data2)
    mean = np.mean([data1, data2], axis=0)
    diff = data1 - data2
    return stats.shapiro(diff)


def bland_altman(data1, data2, *args, **kwargs):
    data1 = np.asarray(data1)
    data2 = np.asarray(data2)
    mean = np.mean([data1, data2], axis=0)
    diff = data1 - data2  # Difference between data1 and data2
    md = np.mean(diff)  # Mean of the difference
    sd = np.std(diff, axis=0)  # Standard deviation of the difference

    plt.scatter(mean, diff, *args, **kwargs)
    plt.axhline(md, color="blue", linestyle="--")
    plt.axhline(md + 1.96 * sd, color="red", linestyle="--")
    plt.axhline(md - 1.96 * sd, color="red", linestyle="--")


import seaborn as sns
from statannot import add_stat_annotation

################################################################################
# Note: Datasets are coming from here:
## https://hmgubox2.helmholtz-muenchen.de/index.php/s/tSi66Ajo3DLHXc9?path=%2FCalorimetryDataSets
################################################################################


def find_approx_square(N):
    """Very primitive idea to find an approximate square subplot arrangement"""
    U = math.ceil(math.sqrt(N))
    L = math.floor(math.sqrt(N))
    if U * L != N:
        return U, U
    else:
        return L, U


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run compare.py")
    parser.add_argument("-f", "--file", dest="file", required=True)
    parser.add_argument("-r", "--reference", dest="reference", required=True)
    parser.add_argument("-w", "--window", dest="window", required=True)
    parser.add_argument("-o", "--output", dest="output", required=True)
    parser.add_argument("-t", "--time", dest="time", required=True)
    parser.add_argument("-n", "--name", dest="name", required=True)
    parser.add_argument("-m", "--metadata", dest="metadata", required=False)
    args = parser.parse_args()

    dfCalimera = pd.read_csv(args.reference, sep="\t")
    dfShiny = pd.read_csv(args.file, sep=";")
    # dfShiny.loc[(dfShiny["HP"] > 1), "HP"] *= 0.005


def qq_plot(data1, data2, *args, **kwargs):
    data1 = np.asarray(data1)
    data2 = np.asarray(data2)
    mean = np.mean([data1, data2], axis=0)
    diff = data1 - data2
    stats.probplot(diff, dist="norm", plot=pylab)


def bland_altman(data1, data2, *args, **kwargs):
    data1 = np.asarray(data1)
    data2 = np.asarray(data2)
    mean = np.mean([data1, data2], axis=0)
    diff = data1 - data2  # Difference between data1 and data2
    md = np.mean(diff)  # Mean of the difference
    sd = np.std(diff, axis=0)  # Standard deviation of the difference

    plt.scatter(mean, diff, *args, **kwargs)
    plt.axhline(md, color="blue", linestyle="--")

    plt.text(5.0, float(md + 0.1), f"{abs(float(md)-0)}")
    plt.axhline(md + 1.96 * sd, color="red", linestyle="--")
    plt.axhline(md - 1.96 * sd, color="red", linestyle="--")

    b, m = polyfit(mean, diff, 1)
    print(f"b: {b}")
    print(f"m: {m}")
    plt.plot(mean, b + m * mean, color="yellow", linestyle="-.")
    # plt.plot(0, 0, color="black", linestyle=":")
    slope, intercept, r_value, p_value, std_err = scipy.stats.linregress(mean, diff)
    print(f"r2: {r_value*r_value}")
    plt.text(6.5, 0, f"$R^2$: {r_value*r_value}")
    plt.text(5.5, 1, f"p-value: {p_value}")
    # plt.axline(xy1=(0, b), slope=m, color='r', label=f'$y = {m:.2f}x {b:+.2f}$')


import seaborn as sns
from statannot import add_stat_annotation

################################################################################
# Note: Datasets are coming from here:
## https://hmgubox2.helmholtz-muenchen.de/index.php/s/tSi66Ajo3DLHXc9?path=%2FCalorimetryDataSets
################################################################################


def find_approx_square(N):
    """Very primitive idea to find an approximate square subplot arrangement"""
    U = math.ceil(math.sqrt(N))
    L = math.floor(math.sqrt(N))
    if U * L != N:
        return U, U
    else:
        return L, U


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run compare.py")
    parser.add_argument("-f", "--file", dest="file", required=True)
    parser.add_argument("-r", "--reference", dest="reference", required=True)
    parser.add_argument("-w", "--window", dest="window", required=True)
    parser.add_argument("-o", "--output", dest="output", required=True)
    parser.add_argument("-t", "--time", dest="time", required=True)
    parser.add_argument("-n", "--name", dest="name", required=True)
    parser.add_argument("-m", "--metadata", dest="metadata", required=False)
    args = parser.parse_args()

    dfCalimera = pd.read_csv(args.reference, sep="\t")
    dfShiny = pd.read_csv(args.file, sep=";")
    # dfShiny.loc[(dfShiny["HP"] > 1), "HP"] *= 0.005
    animal_ids = dfCalimera.columns.tolist()[4:]
    num_rows2 = math.ceil((len(dfShiny.index) - 1) / len(animal_ids))
    num_rows = len(dfCalimera.index) - 1

    print(num_rows2)
    print("calimera")
    print(num_rows)
    if num_rows != num_rows2:
        print("do not match!")

    rmsds = []

    for index, animal in enumerate(animal_ids):
        timepoints = dfShiny.loc[
            (dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")
        ]["Time"].tolist()
        # Shiny might have too many rows as well depending on averaging etc.
        timepoints = [t for t in timepoints if t < num_rows]
        datapoints = dfShiny.loc[
            (dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")
        ]["HP"].tolist()
        datapoints = datapoints[0 : len(timepoints)]
        datapointsRef = dfCalimera[animal][1:].tolist()
        datapointsRef = [datapointsRef[i] for i in timepoints]
        rmsds.append(
            np.sqrt(np.mean((np.array(datapointsRef) - np.array(datapoints)) ** 2))
        )

    L, U = find_approx_square(len(animal_ids))
    for index, column in enumerate(animal_ids):
        plt.subplot(L, U, index + 1)  # 3, 4
        plt.plot(
            range(0, num_rows),
            dfCalimera[column][1:],
            label=f"Animal {column} (Calimera)",
        )
        plt.legend(loc=2, prop={"size": 6})
        plt.ylabel("kcal/h")
        plt.xlabel(f"Time")

    for index, animal in enumerate(animal_ids):
        plt.subplot(L, U, index + 1)  # 3, 4
        plt.plot(
            dfShiny.loc[
                (dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")
            ]["Time"].tolist(),
            dfShiny.loc[
                (dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")
            ]["HP"].tolist(),
            label=f"Animal {animal} (based on O2)",
        )
        plt.plot(
            dfShiny.loc[
                (dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "CO2")
            ]["Time"].tolist(),
            dfShiny.loc[
                (dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "CO2")
            ]["HP"].tolist(),
            label=f"Animal {animal} (based on CO2)",
        )
        plt.legend(loc=1, prop={"size": 6})
        plt.text(0.1, 0.2, f"RMSD={'{:10.4f}'.format(rmsds[index])}")
        plt.ylabel("kcal/h")
        plt.xlabel(f"Time")

    ### TODO: crucical to figure the scaling out...  otherwise will get screwed depending on window size! (time discretization matters also?)
    # filter based on metadata daylight period... 7 to 7...
    min_RMRs = []
    total_EEs = []
    for index, animal in enumerate(animal_ids):
        total_EEs.append(
            dfShiny.loc[dfShiny["Animal"] == int(animal)]["HP"].sum() / 3
        )  # 5 minutes interval, thus divide by 5*12=60 TODO keep track of number of days, cannot just sum all data

    for index, animal in enumerate(animal_ids):
        min_RMRs.append(
            24 * min(dfShiny.loc[dfShiny["Animal"] == int(animal)]["HP"].tolist())
        )  # *6 (10 minute interval) # 5 minute interval = 12 (since value in shiny app is kcal/h)

    min_RMRsRef = []
    total_EEsRef = []
    for index, animal in enumerate(animal_ids):
        total_EEsRef.append(
            dfCalimera[animal][1:].sum() / 3
        )  # need to divide by 6 because 6x 10 minute interval...
        min_RMRsRef.append(
            24 * min(dfCalimera[animal][1:].tolist())
        )  # *6 # TODO: maybe need to divide by 6 or 12 depending on interval length in summation?

    manager = plt.get_current_fig_manager()
    # manager.window.showMaximized()
    # plt.show()
    plt.suptitle(
        f"Time discretization {args.time} minutes. Window size = {args.window} intervals, 25% lowest for averaging of RMR in each window. Dataset: {args.name}"
    )
    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.savefig(
        f"{args.output}/comparison_with_calimera_window_size={args.window}_time_trace_RMR_over_day.png",
        bbox_inches="tight",
    )
    plt.clf()

    data = {"Our": min_RMRs, "Ref": min_RMRsRef}
    order = ["Our", "Theirs"]
    df = pd.DataFrame(
        data={
            "x": min_RMRs + min_RMRsRef,
            "y": ["Our"] * len(min_RMRs) + ["Theirs"] * len(min_RMRsRef),
        }
    )
    ax = sns.boxplot(data=df, x="y", y="x", order=order)
    add_stat_annotation(
        ax,
        data=df,
        x="y",
        y="x",
        order=order,
        box_pairs=[("Our", "Theirs")],
        test="t-test_ind",
        text_format="star",
        loc="outside",
        verbose=2,
    )
    sns.stripplot(data=df, x="y", y="x", dodge=True, ax=ax)

    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.ylabel("kcal/day")
    plt.suptitle(f"Daily RMR. Dataset: {args.name}")
    plt.savefig(
        f"{args.output}/comparison_with_calimera_window_size={args.window}_boxplots_with_stats_RMR_per_day.png",
        bbox_inches="tight",
    )

    plt.clf()

    plt.boxplot(data.values(), showfliers=False, sym="")
    plt.xticks([1, 2], ["w/o activity data", "w activity data"])
    plt.plot([1] * len(min_RMRs), min_RMRs, "r.", alpha=0.2)
    plt.ylim([0, 20])
    for index, val in enumerate(min_RMRs):
        plt.text(1.01, val, animal_ids[index], fontsize=6)
    plt.plot([2] * len(min_RMRs), min_RMRsRef, "r.", alpha=0.2)
    for index, val in enumerate(min_RMRsRef):
        plt.text(2.01, val, animal_ids[index], fontsize=6)

    # plt.show()
    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.ylabel("kcal/day")
    plt.suptitle(f"Daily RMR. Dataset: {args.name}")
    plt.savefig(
        f"{args.output}/comparison_with_calimera_window_size={args.window}_boxplots_RMR_per_day.png",
        bbox_inches="tight",
    )

    plt.clf()
    df = pd.DataFrame(
        {
            "Energy": min_RMRs + min_RMRsRef + total_EEs,
            "Animals": animal_ids + animal_ids + animal_ids,
            "Type": ["RMR"] * len(min_RMRs)
            + ["RMR_ref"] * len(min_RMRsRef)
            + ["Total"] * len(total_EEs),
        }
    )
    g = sns.catplot(
        data=df,
        kind="bar",
        x="Animals",
        y="Energy",
        hue="Type",
        palette="dark",
        alpha=0.6,
        height=6,
        ci="sd",
        estimator=np.mean,
        capsize=0.2,
    )
    g.despine(left=True)
    g.set_axis_labels("Animal ID", "Energy expenditure over day [kcal/day]")
    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.savefig(
        f"{args.output}/comparison_with_calimera_window_size={args.window}_barplots_with_stats_RMR_per_day.png",
        bbox_inches="tight",
    )
    plt.clf()

    # shapiro = calc_shapiro(min_RMRs, min_RMRsRef)
    # ks = calc_ks(min_RMRs, min_RMRsRef)
    # print(f"KS test: p-value = {ks.pvalue}")
    # print(f"Shapiro-Wilk test: p-value = {shapiro.pvalue}")
    # if ks.pvalue < 0.05 or shapiro.pvalue < 0.05:
    #    print("Test might not be applicable (Bland-Altman)")

    plt.clf()
    qq_plot(min_RMRs, min_RMRsRef)
    plt.title("Q-Q plot")
    plt.ylabel("Expected mean")
    plt.xlabel("Observed mean")
    plt.savefig(
        f"{args.output}/qq_plot={args.window}_RMR_per_day.png", bbox_inches="tight"
    )
    plt.clf()
    plt.rcParams["text.usetex"] = True
    bland_altman(min_RMRs, min_RMRsRef)
    plt.title("Bland-Altman plot RMR method without and with activity data")
    plt.xlabel(r"$\frac{S_1+S_2}{2}$")
    plt.ylabel(r"$S_1-S_2$")
    plt.rcParams["text.usetex"] = False
    plt.savefig(
        f"{args.output}/bland_altman={args.window}_RMR_per_day.png", bbox_inches="tight"
    )
    plt.clf()
    hist_plot(min_RMRs, min_RMRsRef)
    plt.savefig(
        f"{args.output}/histogram={args.window}_RMR_per_day.png", bbox_inches="tight"
    )
    plt.clf()

    if args.metadata:
        metadata = pd.read_csv(f"{args.metadata}.csv", sep=";")
        xs = metadata["Weight"].tolist()
        metadata["RMR"] = min_RMRs
        metadata.names = ["Weight", "Animal", "RMR"]
        g = sns.scatterplot(data=metadata, y="RMR", x="Weight")
        slope, intercept, r_value, p_value, std_err = scipy.stats.linregress(
            metadata["RMR"], metadata["Weight"]
        )
        ypos = max(metadata["RMR"]) - max(metadata["RMR"]) / 50
        xpos = max(metadata["Weight"]) - max(metadata["Weight"]) * 10 / 100
        plt.text(xpos, ypos, f"R²: {r_value**2:9.4e} and p-value: {p_value:9.4e}")
        f.set_size_inches(16, 10)
        plt.savefig(
            f"{args.output}/comparison_with_calimera_window_size={args.window}_weight_vs_RMR.png",
            bbox_inches="tight",
        )

    # df = pd.DataFrame({ 'Our' : min_RMRs, 'Total' : total_EEs, 'Theirs': min_RMRsRef, 'Animals' : animal_ids})
    # df.plot(kind='bar', stacked=True, color=['red', 'skyblue', 'green'])
    # plt.show()
