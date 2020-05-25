import pandas as pd
results = pd.read_csv("/media/hhew0002/f0df6edb-45fe-4416-8076-34757a0abceb/hhew0002/Academic/Competitions/M5 Competition/M5Comp/results/lgbm_submission.csv")
def add_state(row):
    if "_CA_" in row["id"]:
        return "CA"
    elif "_TX_" in row["id"]:
        return "TX"
    else:
        return "WI"

results['state_id'] = results.apply (lambda row: add_state(row), axis=1)

def cat_id(row):
    if "HOBBIES_" in row["id"]:
        return "HOBBIES"
    elif "HOUSEHOLD_" in row["id"]:
        return "HOUSEHOLD"
    else:
        return "FOOD"

results['cat_id'] = results.apply (lambda row: cat_id(row), axis=1)
results['cat_id'] = pd.Categorical(results['cat_id'], ["HOBBIES", "HOUSEHOLD", "FOOD"])

def item_id(row):
   x = "_".join(row["id"].split("_")[:3])
   return x

results['item_id'] = results.apply (lambda row: item_id(row), axis=1)

def store_id(row):
   x = "_".join(row["id"].split("_")[3:5])
   return x

results['store_id'] = results.apply (lambda row: store_id(row), axis=1)


def stage(row):
    x = row["id"].rsplit("_")[-1]
    return x

results['stage'] = results.apply (lambda row: stage(row), axis=1)
results['stage'] = pd.Categorical(results['stage'], ["validation", "evaluation"])
results.sort_values(["stage", "state_id", "store_id", "cat_id", "item_id"], inplace=True)
results = results.drop(columns=["state_id", "cat_id", "item_id", "store_id", "stage"])

results.to_csv("/media/hhew0002/f0df6edb-45fe-4416-8076-34757a0abceb/hhew0002/Academic/Competitions/M5 Competition/M5Comp/results/lgbm_sorted_submission.csv", index=False)
