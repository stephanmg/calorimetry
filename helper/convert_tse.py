#!/usr/bin/env python3.9

import csv
import pandas as pd
import re
import os
import argparse


def parse_cli_args():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(description="Run convert_tse.py")

    parser.add_argument(
        "-i", "--file-in", dest="file_in", help="Input file", required=True
    )

    parser.add_argument(
        "-o", "--file-out", dest="file_out", help="Output file", required=True
    )

    return parser.parse_args()


def prepend_line(file_name, line):
    dummy_file = file_name + ".bak"
    with open(file_name, "r") as read_obj, open(dummy_file, "w") as write_obj:
        write_obj.write(line + "\n")
        for line in read_obj:
            write_obj.write(line)
    os.remove(file_name)
    os.rename(dummy_file, file_name)


def convert_tse():
    """Converts some non-standard TSE format to TSE format"""
    # Note: The indices might need to be adjust depending on TSE file type
    NUMBER_OF_SEPARATING_EMPTY_LINES = 1  # 3
    OFFSET_EMPTY_LINE_TO_STOP = 1  # 0
    DATA_OFFSET = 4  # 3

    lines = []
    with open(args.file_in) as f:
        reader = csv.reader(f)
        for idx, line in enumerate(reader):
            if len(line) == 0:
                NUMBER_OF_SEPARATING_EMPTY_LINES += 1
                lines.append([""])
            else:
                lines.append(line[0].split(";"))

    items_first_column = [item[0] for item in lines]
    items_second_column = [item[1] if len(item) > 1 else [[]] for item in lines]
    stop = items_first_column.index("Stop")
    box = items_first_column.index("Box")
    date = items_first_column.index("Date")
    pat = re.compile(r"\d\d\.\d\d\.\d\d")
    dates = [
        i + NUMBER_OF_SEPARATING_EMPTY_LINES
        for i, x in enumerate(items_first_column)
        if re.search(pat, str(x))
    ]
    dateNames = [name for name in items_first_column if re.search(pat, str(name))]
    indices = [item[0] for item in lines if item[0] != ""]

    animalNames = []
    print(indices)
    print("len of indices")
    print(len(indices))
    for val in indices:
        if (
            items_first_column.index(val) > box - NUMBER_OF_SEPARATING_EMPTY_LINES
            and items_first_column.index(val)
            < stop - OFFSET_EMPTY_LINE_TO_STOP + NUMBER_OF_SEPARATING_EMPTY_LINES + 1
        ):
            animalNames.append(items_second_column[items_first_column.index(val)])

    boxes = [lines[date].index(box) for box in lines[date] if box.startswith("Box")]
    boxNames = [box for box in lines[date] if box.startswith("Box")]
    keys = [
        "XY+YZ",
        "VO2(3)",
        "VCO2(3)",
        "Temp",
        "Box",
        "Date",
        "Time",
        "RER",
        "Animal No.",
    ]
    animals = {key: [] for key in keys}

    # Note: The indices might need to be adjust depending on TSE file type
    identifiers = {
        "XY+YZ": 9,  # 0
        "Time": 1,  # 1
        "VO2(3)": 6,  # 2
        "VCO2(3)": 7,  # 3
        "Temp": 1,  # 4
        "RER": 8,  # 5
    }
    identifiers_meta = {"Box": boxNames, "Animal No.": animalNames, "Date": dateNames}

    for boxIndex, box in enumerate(boxes):
        # add all meta identifiers
        for index, j in enumerate(dates[:-1]):
            for i in range(0, dates[index + 1] - j):
                for key, val in identifiers_meta.items():
                    if key == "Date":
                        animals[key].append(val[index])
                    else:
                        animals[key].append(
                            re.search("(\d+)", val[boxIndex]).group(1)
                            if key == "Box"
                            else val[boxIndex + 1]
                        )

        # append for last measurement day
        for i in range(0, len(lines) - dates[-1]):
            for key, val in identifiers_meta.items():
                if key == "Date":
                    animals[key].append(val[index])
                else:
                    animals[key].append(
                        re.search("(\d+)", val[boxIndex]).group(1)
                        if key == "Box"
                        else val[boxIndex + 1]
                    )

        # add all data identifiers
        for index, j in enumerate(dates[:-1]):
            for i in range(0, dates[index + 1] - j):
                for key, val in identifiers.items():
                    animals[key].append(
                        lines[j + DATA_OFFSET + i][box + val].replace(".", ",")
                        if key != "Time"
                        else str(lines[j + DATA_OFFSET + i][val])
                    )

        # append for last measurement day
        for i in range(0, len(lines) - dates[-1]):
            for key, val in identifiers.items():
                animals[key].append(
                    lines[j + DATA_OFFSET + i][box + val].replace(".", ",")
                    if key != "Time"
                    else str(lines[j + DATA_OFFSET + i][val])
                )

    # consistency check if all data of all boxes has been recorded accordingly
    assert (len(lines) - stop - NUMBER_OF_SEPARATING_EMPTY_LINES - 1) * len(boxes) == (
        min([len(value) for _, value in animals.items()])
    )

    # data creation from dict of lists
    df = pd.DataFrame.from_dict(animals)
    # insert units after column labels
    df.loc[-1] = ["[Cnts]", "[ml/h]", "[ml/h]", "[C]", "", "", "", "", ""]
    df.index = df.index + 1  # shifting index
    df.sort_index(inplace=True)
    df = df[["Date", "Time", "Animal No.", "Box", "Temp", "VO2(3)", "VCO2(3)", "RER"]]
    df.to_csv(args.file_out, index=False, sep=";", decimal=",")

    # prepend given metadata to exported csv file (insert TSE header)
    for line in reversed(lines[0:date]):
        prepend_line(args.file_out, ";".join([str(l) for l in line]))


if __name__ == "__main__":
    args = parse_cli_args()
    convert_tse()
