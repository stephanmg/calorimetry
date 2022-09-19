import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
import argparse
import scipy

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
    if U*L != N:
        return U, U
    else:
        return L, U

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Run compare.py')
    parser.add_argument('-f', '--file', dest='file', required=True)
    parser.add_argument('-r', '--reference', dest='reference', required=True)
    parser.add_argument('-w', '--window', dest='window', required=True)
    parser.add_argument('-o', '--output', dest='output', required=True)
    parser.add_argument('-t', '--time', dest='time', required=True)
    parser.add_argument('-n', '--name', dest='name', required=True)
    parser.add_argument('-m', '--metadata', dest='metadata', required=False)
    args = parser.parse_args()

    dfCalimera = pd.read_csv(args.reference, sep='\t')
    dfShiny = pd.read_csv(args.file, sep=";")
    #dfShiny.loc[(dfShiny["HP"] > 1), "HP"] *= 0.005
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
        timepoints = dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")]["Time"].tolist()
        # Shiny might have too many rows as well depending on averaging etc.
        timepoints = [t for t in timepoints if t < num_rows]
        datapoints =  dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")]["HP"].tolist()
        datapoints = datapoints[0:len(timepoints)]
        datapointsRef = dfCalimera[animal][1:].tolist()
        datapointsRef = [datapointsRef[i] for i in timepoints]
        rmsds.append(np.sqrt(np.mean((np.array(datapointsRef)-np.array(datapoints))**2)))
        
    L, U = find_approx_square(len(animal_ids))
    for index, column in enumerate(animal_ids):
        plt.subplot(L, U, index + 1) # 3, 4
        plt.plot(range(0, num_rows), dfCalimera[column][1:], label=f"Animal {column} (Calimera)")
        plt.legend(loc=2, prop={'size' : 6})
        plt.ylabel('kcal/h')
        plt.xlabel(f'Time')

    for index, animal in enumerate(animal_ids):
        plt.subplot(L, U, index + 1) # 3, 4
        plt.plot(dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")]["Time"].tolist(), dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")]["HP"].tolist(), label=f"Animal {animal} (based on O2)")
        plt.plot(dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "CO2")]["Time"].tolist(), dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "CO2")]["HP"].tolist(), label=f"Animal {animal} (based on CO2)")
        plt.legend(loc=1, prop={'size' : 6})
        plt.text(0.1, 0.2, f"RMSD={'{:10.4f}'.format(rmsds[index])}")
        plt.ylabel('kcal/h')
        plt.xlabel(f'Time')


    # filter based on metadata daylight period... 7 to 7...
    min_RMRs = []
    total_EEs = []
    for index, animal in enumerate(animal_ids):
        total_EEs.append(dfShiny.loc[dfShiny["Animal"] == int(animal)]["HP"].sum())

    for index, animal in enumerate(animal_ids):
        min_RMRs.append(24 * min(dfShiny.loc[dfShiny["Animal"] == int(animal)]["HP"].tolist()))  # *6 (10 minute interval) # 5 minute interval = 12

    min_RMRsRef = []
    total_EEsRef = []
    for index, animal in enumerate(animal_ids):
        total_EEsRef.append(dfCalimera[animal][1:].sum() / 24)
        min_RMRsRef.append(24 * min(dfCalimera[animal][1:].tolist())) # *6

    manager = plt.get_current_fig_manager()
    # manager.window.showMaximized()
    # plt.show()
    plt.suptitle(f"Time discretization {args.time} minutes. Window size = {args.window} intervals, 25% lowest for averaging of RMR in each window. Dataset: {args.name}")
    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.savefig(f"{args.output}/comparison_with_calimera_window_size={args.window}_time_trace_RMR_over_day.png", bbox_inches='tight')
    plt.clf()

    data = {'Our' : min_RMRs, 'Ref' : min_RMRsRef}
    order = ['Our', 'Theirs']
    df = pd.DataFrame(data={'x': min_RMRs + min_RMRsRef, 'y': ["Our"]*len(min_RMRs) + ["Theirs"]*len(min_RMRsRef)})
    ax = sns.boxplot(data=df, x='y', y='x', order=order)
    add_stat_annotation(ax, data=df, x='y', y='x', order=order, box_pairs=[("Our", "Theirs")], test='t-test_ind', text_format='star', loc='outside', verbose=2)
    sns.stripplot(data=df, x="y", y="x", dodge=True, ax=ax)
    
    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.ylabel('kcal/day')
    plt.suptitle(f"Daily RMR. Dataset: {args.name}")
    plt.savefig(f"{args.output}/comparison_with_calimera_window_size={args.window}_boxplots_with_stats_RMR_per_day.png", bbox_inches='tight')

    plt.clf()
    
    plt.boxplot(data.values(), showfliers=False, sym='')
    plt.xticks([1,2], ['w/o activity data', 'w activity data'])
    plt.plot([1]*len(min_RMRs), min_RMRs, 'r.', alpha=.2)
    plt.ylim([0, 20])
    for index, val in enumerate(min_RMRs):
        plt.text(1.01, val, animal_ids[index], fontsize=6)
    plt.plot([2]*len(min_RMRs), min_RMRsRef, 'r.', alpha=.2)
    for index, val in enumerate(min_RMRsRef):
        plt.text(2.01, val, animal_ids[index], fontsize=6)

    # plt.show()
    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.ylabel('kcal/day')
    plt.suptitle(f"Daily RMR. Dataset: {args.name}")
    plt.savefig(f"{args.output}/comparison_with_calimera_window_size={args.window}_boxplots_RMR_per_day.png", bbox_inches='tight')

    plt.clf()
    df = pd.DataFrame({ 'Energy': min_RMRs + min_RMRsRef + total_EEs, 'Animals' : animal_ids + animal_ids + animal_ids, 
        'Type' : ["RMR"] * len(min_RMRs) + ["RMR_ref"] * len(min_RMRsRef) + ["Total"] * len(total_EEs)})
    g = sns.catplot(data=df, kind="bar", x='Animals', y='Energy', hue="Type", palette='dark', alpha=.6, height=6, ci='sd', estimator=np.mean, capsize=.2)
    g.despine(left=True)
    g.set_axis_labels("Animal ID", "Energy expenditure over day [kcal/day]")
    f = plt.gcf()
    f.set_size_inches(16, 10)
    plt.savefig(f"{args.output}/comparison_with_calimera_window_size={args.window}_barplots_with_stats_RMR_per_day.png", bbox_inches='tight')

    plt.clf()
    if args.metadata:
        metadata = pd.read_csv(f"{args.metadata}.csv", sep=";")
        xs = metadata["Weight"].tolist()
        metadata["RMR"] = min_RMRs
        metadata.names = ["Weight", "Animal", "RMR"]
        g = sns.scatterplot(data=metadata, y="RMR", x="Weight")
        slope, intercept, r_value, p_value, std_err = scipy.stats.linregress(metadata["RMR"], metadata["Weight"])
        ypos = max(metadata["RMR"]) - max(metadata["RMR"]) / 50
        xpos = max(metadata["Weight"]) - max(metadata["Weight"])*10/100
        plt.text(xpos, ypos, f"R²: {r_value**2:9.4e} and p-value: {p_value:9.4e}")
        f.set_size_inches(16, 10)
        plt.savefig(f"{args.output}/comparison_with_calimera_window_size={args.window}_weight_vs_RMR.png", bbox_inches='tight')

    #df = pd.DataFrame({ 'Our' : min_RMRs, 'Total' : total_EEs, 'Theirs': min_RMRsRef, 'Animals' : animal_ids})
    #df.plot(kind='bar', stacked=True, color=['red', 'skyblue', 'green'])
    #plt.show()
