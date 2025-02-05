# Part 1
import pandas as pd
import numpy as np

Crime = pd.read_csv("crime_data.csv")
FirstFive = Crime.head(5)
print(FirstFive)

# I will need Weapon Desc column in Part 4
Crime["Weapon Desc"] = Crime["Weapon Desc"].replace(np.nan, "No information", regex=False)
Missing = Crime.isna().sum()
print(Missing)

Cleanable = Missing[(Missing / len(Crime)) > 0.5].index
print(Cleanable)

Crime2 = Crime.drop(columns=Cleanable)
print(Crime2.columns)

Crime2["DATE OCC"] = pd.to_datetime(Crime2["DATE OCC"], format="%m/%d/%Y %I:%M:%S %p")
Crime2["Year"] = Crime2["DATE OCC"].dt.year
Crime2["Month"] = Crime2["DATE OCC"].dt.month
Crime2["Day"] = Crime2["DATE OCC"].dt.day
print(Crime2.columns)

Crime2["TIME OCC"] = Crime2["TIME OCC"].astype(str)
print(type(Crime2["TIME OCC"][0]))

TimeOfCrime = []
times = Crime2["TIME OCC"]
for i in range(0, len(times)):
    if len(times[i]) < 4:
        while len(times[i]) < 4:
            times[i] = "0" + times[i]
        result = times[i]
    else:
        result = times[i]
    TimeOfCrime.append(result)

Crime2["TimeOfCrime"] = TimeOfCrime

# Part 2
TypeOfCrime = Crime2["Crm Cd Desc"]
print(TypeOfCrime.value_counts().nlargest(3))

hour = []
for toc in Crime2["TimeOfCrime"]:
    hour.append(toc[:2])
Crime2["Hour"] = hour
Crime4 = Crime2.groupby("Hour").agg(NumOfCrimes=("Hour", "count")).reset_index()
print(Crime4)

Crime5 = Crime2.groupby("Vict Sex").agg(NumOfCrimes=("Vict Sex", "count"),
                                        AvgVicAge=("Vict Age", "mean")).reset_index()
print(Crime5)

# Part 4
conditions = [
    (Crime2["Crm Cd Desc"] == "BURGLARY") & (Crime2["Weapon Desc"] != "No information"),
    (Crime2["Crm Cd Desc"] == "BURGLARY") & (Crime2["Weapon Desc"] == "No information"),
    (Crime2["Crm Cd Desc"] != "BURGLARY") & (Crime2["Weapon Desc"] != "No information"),
    (Crime2["Crm Cd Desc"] != "BURGLARY") & (Crime2["Weapon Desc"] == "No information")
]
choices = [8, 3, 5, 1]
Crime2["Severity Score"] = np.select(conditions, choices, default=1)

# Variant 2
# Crime2["Severity Score"] = 1
# for index, row in Crime2.iterrows():
#     if Crime2[Crime2["Crm Cd Desc"] == "BURGLARY"]:
#         if Crime2[Crime2["Weapon Desc"] != "No information"]:
#             Crime2.loc[index, "Severity Score"] = 8
#         else:
#             Crime2.loc[index, "Severity Score"] = 3
#     elif Crime2[Crime2["Weapon Desc"] != "No information"]:
#         Crime2.loc[index, "Severity Score"] = 5
#     else:
#         Crime2.loc[index, "Severity Score"] = 1

Crime6 = Crime2.groupby("AREA NAME").agg(SeverityScore=("Severity Score", "sum")).reset_index()
print(Crime6)
