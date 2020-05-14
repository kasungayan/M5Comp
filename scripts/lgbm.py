
# coding: utf-8

# In[1]:


import lightgbm as lgbm
import numpy as np
import pandas as pd

# define data types for the sales data
numCols = [f"d_{day}" for day in range(1, 1914)]

# Define all categorical columns
catCols = ['id', 'item_id', 'dept_id','store_id', 'cat_id', 'state_id']

# Define the correct data types for "sales_train_validation.csv"
dtype = {numCol: "float32" for numCol in numCols}
dtype.update({catCol: "category" for catCol in catCols if catCol != "id"})

# read the sales data
sales_data = pd.read_csv("./data/sales_train_validation.csv", usecols=catCols + numCols, dtype=dtype)
sales_data = sales_data.drop(columns=["item_id", "dept_id", "store_id"])


# In[2]:


# read the calendar data
calendarDTypes = {"event_name_1": "category",
                  "event_name_2": "category",
                  "event_type_1": "category",
                  "event_type_2": "category",
                  "weekday": "category",
                  'wm_yr_wk': 'int16',
                  "wday": "category",
                  "month": "category",
                  "year": "int16",
                  "snap_CA": "category",
                  'snap_TX': 'category',
                  'snap_WI': 'category' }
calendar_data = pd.read_csv("./data/calendar.csv", dtype=calendarDTypes)

# remove the last 28 points from the calendar data
horizon = 28
calendar_data = calendar_data.iloc[:-horizon, :]


for col, colDType in calendarDTypes.items():
    if colDType == "category":
        calendar_data[col] = calendar_data[col].cat.codes.astype("int16")
        calendar_data[col] -= calendar_data[col].min()

def add_weekend(row):
    if row["wday"] == 1 or row["wday"] == 2:
        return 1
    else:
        return 0

calendar_data['weekend'] = calendar_data.apply (lambda row: add_weekend(row), axis=1)
calendar_data = calendar_data.drop(columns=["date", "wm_yr_wk", "year", "weekday", "wday"])


# In[8]:


# read the price data
price_data = pd.read_csv("./data/Item-prices.txt", header=None)
price_data = price_data.iloc[:,:-horizon]

column_names = [f"d_{day}" for day in range(1, 1942)]
price_data.columns = column_names

# add the item id column
price_data["id"] = sales_data["id"]
# melt the price data
price_data_melted = pd.melt(price_data, id_vars=['id'], var_name="d", value_name="price")


# In[4]:


# embed the sales data to have a lag length of 10
data = pd.melt(sales_data, id_vars=['id', 'cat_id', 'state_id'], var_name="d", value_name="sales")

for lags in range(1,11):
    data['lag' + str(lags)] = data[["id","sales"]].groupby("id")["sales"].shift(lags)


# In[6]:


# merge the calendar data into the sales data
full_training_data = data.merge(calendar_data, on="d", copy=False)
full_training_data = full_training_data.dropna()

# merge the price data into the sales data
full_training_data = full_training_data.merge(price_data_melted, on=["id","d"], copy=False)

# create only one column for the snap features
def add_snap(row):
    if row["state_id"] == "CA":
        return row["snap_CA"]
    elif row["state_id"] == "TX":
        return row["snap_TX"]
    else:
        return row["snap_WI"]

full_training_data['snap'] = full_training_data.apply (lambda row: add_snap(row), axis=1)
full_training_data = full_training_data.drop(columns=["id", "d", "snap_CA", "snap_WI", "snap_TX"])
full_training_data.info()


# In[22]:


# create the training x and y data
full_training_data_x = full_training_data.drop(columns=["sales"])
full_training_data_y = full_training_data.sales

# seperate the training and validation data
np.random.seed(777)

# Define categorical features
cat_features = ['cat_id', 'state_id', 'weekend', 'month', 'event_name_1', 'event_name_2', 'event_type_1', 'event_type_2', 'snap']

# validation data
validation_indices = np.random.choice(full_training_data_x.index.values, 2_000_000, replace = False)
training_indices = np.setdiff1d(full_training_data_x.index.values, validation_indices)

training_data = lgbm.Dataset(full_training_data_x.loc[training_indices], label = full_training_data_y.loc[training_indices],
                        categorical_feature = cat_features, free_raw_data = False)
valid_data = lgbm.Dataset(full_training_data_x.loc[validation_indices], label = full_training_data_y.loc[validation_indices],
                        categorical_feature = cat_features, free_raw_data = False)


# In[10]:


#build the lightgbm model
params = {
          "objective" : "poisson",
          "metric" :"rmse",
          "force_row_wise" : True,
          "learning_rate" : 0.075,
          "sub_row" : 0.75,
          "bagging_freq" : 1,
          "lambda_l2" : 0.1,
          "metric": ["rmse"],
          'verbosity': 1,
          'num_iterations' : 1200,
          'num_leaves': 128,
          "min_data_in_leaf": 100,
         }

# Train LightGBM model
m_lgb = lgbm.train(params, training_data, valid_sets = [valid_data], verbose_eval = 20)

# save the model if needed
m_lgb.save_model("model.lgb")


# In[13]:


# create the initial testing data
lag_size = 10
testing_data = sales_data.iloc[:, -lag_size:]
testing_data['cat_id'] = sales_data.cat_id
testing_data['state_id'] = sales_data.state_id
testing_data['id'] = sales_data.id

for day in range(1914, 1942):
    testing_data[f"d_{day}"] = np.nan

testing_data = pd.melt(testing_data, id_vars=['id', 'cat_id', 'state_id'], var_name="d", value_name="sales")

# embed the sales data to have a lag length of 10
for lags in range(1,11):
    testing_data['lag' + str(lags)] = testing_data[["id","sales"]].groupby("id")["sales"].shift(lags)

# merge the calendar data into the sales data
testing_data = testing_data.merge(calendar_data, on="d", copy=False)

# merge the price data into the sales data
testing_data = testing_data.merge(price_data_melted, on=["id", "d"], copy=False)

testing_data['snap'] = testing_data.apply (lambda row: add_snap(row), axis=1)
testing_data = testing_data.drop(columns=["snap_CA", "snap_WI", "snap_TX"])


# In[21]:


# perform the recursive forecasting

cols = [f"F{i}" for i in range(1,29)]
for prediction_point in range(1914, 1942):
    current_testing_data = testing_data[testing_data["d"].str.split("d_").str[1].astype(int) == prediction_point]
    current_testing_data = current_testing_data.drop(columns=["id", "d", "sales"])
    prediction = m_lgb.predict(current_testing_data)

    # add the most recent prediction back to the testing data
    testing_data.loc[testing_data["d"].str.split("d_").str[1].astype(int) == prediction_point, "sales"] = prediction

    # recreate the lags
    for lags in range(1,11):
        testing_data['lag' + str(lags)] = testing_data[["id","sales"]].groupby("id")["sales"].shift(lags)


sub = testing_data.loc[testing_data["d"].str.split("d_").str[1].astype(int) >= 1914, ["id", "sales"]].copy()
sub["F"] = [f"F{rank}" for rank in sub.groupby("id")["id"].cumcount()+1]
sub = sub.set_index(["id", "F" ]).unstack()["sales"][cols].reset_index()
sub.fillna(0., inplace = True)
sub.sort_values("id", inplace = True)
sub.reset_index(drop=True, inplace = True)


sub2 = sub.copy()
sub2["id"] = sub2["id"].str.replace("validation$", "evaluation")
sub = pd.concat([sub, sub2], axis=0, sort=False)
sub.to_csv("submission.csv",index=False)


# In[46]:




