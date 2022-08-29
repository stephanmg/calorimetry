import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
import argparse

    
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Run plot_comp_times.py')
    parser.add_argument('-f', '--file', dest='file', required=True)
    parser.add_argument('-r', '--reference', dest='reference', required=True)
    parser.add_argument('-w', '--window', dest='window', required=True)
    args = parser.parse_args()

    dfCalimera = pd.read_csv(args.reference, sep='\t')
    dfShiny = pd.read_csv(args.file, sep=";")
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
        
    
    for index, column in enumerate(animal_ids):
        plt.subplot(6, 6, index + 1) # 3, 4
        plt.plot(range(0, num_rows), dfCalimera[column][1:], label=f"Animal {column} (Calimera)")
        plt.legend(loc=2, prop={'size' : 6})

    for index, animal in enumerate(animal_ids):
        plt.subplot(6, 6, index + 1) # 3, 4
        plt.plot(dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")]["Time"].tolist(), dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "O2")]["HP"].tolist(), label=f"Animal {animal} (based on O2)")
        plt.plot(dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "CO2")]["Time"].tolist(), dfShiny.loc[(dfShiny["Animal"] == int(animal)) & (dfShiny["Component"] == "CO2")]["HP"].tolist(), label=f"Animal {animal} (based on CO2)")
        plt.legend(loc=1, prop={'size' : 6})
        plt.text(0.1, 0.2, f"RMSD={'{:10.4f}'.format(rmsds[index])}")

    min_RMRs = []
    for index, animal in enumerate(animal_ids):
        min_RMRs.append(24  * min(dfShiny.loc[dfShiny["Animal"] == int(animal)]["HP"].tolist()))  # *6 (10 minute interval) # 5 minute interval = 12

    min_RMRsRef = []
    for index, animal in enumerate(animal_ids):
        min_RMRsRef.append(24  * min(dfCalimera[animal][1:].tolist())) # *6

    manager = plt.get_current_fig_manager()
    # manager.window.showMaximized()
    plt.suptitle(f"Time discretization 10 minutes, Window size = {args.window} intervals, 25% lowest for averaging of resting metabolic rate in each window")
    plt.savefig(f"comparison_with_calimera_window_size={args.window}.png", bbox_inches='tight')
    plt.show()


    data = {'Our' : min_RMRs, 'Ref' : min_RMRsRef}
    plt.boxplot(data.values(), showfliers=False, sym='')
    plt.xticks([1,2], ['w/o activity data', 'w activity data'])
    plt.plot([1]*len(min_RMRs), min_RMRs, 'r.', alpha=.2)
    plt.ylim([0, 20])
    for index, val in enumerate(min_RMRs):
        plt.text(1.01, val, animal_ids[index], fontsize=6)
    plt.plot([2]*len(min_RMRs), min_RMRsRef, 'r.', alpha=.2)
    for index, val in enumerate(min_RMRsRef):
        plt.text(2.01, val, animal_ids[index], fontsize=6)


    plt.show()
