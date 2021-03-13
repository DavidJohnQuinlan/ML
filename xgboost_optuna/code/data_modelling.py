import numpy as np
import xgboost as xgb
import optuna
from sklearn.metrics import accuracy_score
from xgboost import XGBClassifier

from hyperparameter_tuning import HyperparameterTuning


class DataModelling(HyperparameterTuning):
    """

    """

    def __init__(self, train, val, test):
        """
        """
        self.x_train = train.drop(['class'], axis=1)
        self.y_train = train['class']
        self.x_val = val.drop(['class'], axis=1)
        self.y_val = val['class']
        self.x_test = test.drop(['class'], axis=1)
        self.y_test = test['class']
        self.dtrain = xgb.DMatrix(self.x_train, label=self.y_train)
        self.dvalid = xgb.DMatrix(self.x_val, label=self.y_val)
        self.dtest = xgb.DMatrix(self.x_test, label=self.y_test)
        super().__init__(self.dtrain, self.dvalid)

    def fit_xgboost_model(self):
        """

        """

        # fit model to the training data
        model = XGBClassifier(eval_metric="logloss")
        model.fit(self.x_train, self.y_train)

        # make predictions for test data
        y_val_pred = model.predict(self.x_val)
        val_predictions = [round(value) for value in y_val_pred]

        # evaluate predictions
        accuracy = accuracy_score(self.y_val, val_predictions)
        print("Accuracy: %.2f%%" % (accuracy * 100.0))

    @staticmethod
    def evaluate_test_data(model: optuna.trial, test_data: xgb.DMatrix):
        """
        """

        optimal_model = xgb.train(model.params, test_data)
        predictions = optimal_model.predict(test_data)
        pred_labels = np.rint(predictions)
        accuracy = accuracy_score(test_data.get_label(), pred_labels)
        print("Accuracy: %.2f%%" % (accuracy * 100.0))

    def data_modelling(self):
        """
        """

        # train an xgboost model
        self.fit_xgboost_model()

        # apply hyperparameter tuning
        optimal_model = super().hyperparameter_tuning()

        # apply the optimal model to the test dataset
        self.evaluate_test_data(optimal_model, self.dtest)
