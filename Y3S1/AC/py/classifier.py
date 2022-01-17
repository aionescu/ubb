import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn import metrics

cols = ["num_keys", "avg_duration", "num_clicks", "num_backspace", "num_runs", "quiz1", "quiz2", "quiz3", "label"]
feature_cols = cols[:-1]

def main():
  data = pd.read_csv("~/Desktop/Study/Data.csv", names = cols)

  x = data[feature_cols]
  y = data.label

  x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.3, random_state = 1)

  clf = DecisionTreeClassifier()
  clf = clf.fit(x_train, y_train)

  y_pred = clf.predict(x_test)

  print("Accuracy:", metrics.accuracy_score(y_test, y_pred))

if __name__ == "__main__":
  main()
