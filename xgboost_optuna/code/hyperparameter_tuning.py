import numpy as np
import optuna
from sklearn.metrics import accuracy_score
import xgboost as xgb


class HyperparameterTuning:
    """
    """

    def __init__(self, dtrain: xgb.DMatrix, dvalid: xgb.DMatrix):
        """
        """

        # define the training and validation datasets
        self.dtrain = dtrain
        self.dvalid = dvalid

    @staticmethod
    def objective(trial: optuna.trial, dtrain: xgb.DMatrix,
                  dvalid: xgb.DMatrix) -> float:
        """
        """

        # define the parameters to be tested
        param = {
            "verbosity": 0,
            "objective": "binary:logistic",
            "booster": trial.suggest_categorical(
                "booster", ["gbtree", "gblinear", "dart"]
            ),
            "lambda": trial.suggest_float("lambda", 1e-8, 1.0, log=True),
            "alpha": trial.suggest_float("alpha", 1e-8, 1.0, log=True),
        }

        # choose the different hyperparameter values
        if param["booster"] == "gbtree" or param["booster"] == "dart":
            param["max_depth"] = trial.suggest_int("max_depth", 1, 9)
            param["eta"] = trial.suggest_float("eta", 1e-8, 1.0, log=True)
            param["gamma"] = trial.suggest_float("gamma", 1e-8, 1.0, log=True)
            param["grow_policy"] = trial.suggest_categorical(
                "grow_policy", ["depthwise", "lossguide"]
            )

        if param["booster"] == "dart":
            param["sample_type"] = trial.suggest_categorical(
                "sample_type", ["uniform", "weighted"]
            )
            param["normalize_type"] = trial.suggest_categorical(
                "normalize_type", ["tree", "forest"]
            )
            param["rate_drop"] = trial.suggest_float(
                "rate_drop", 1e-8, 1.0, log=True
            )
            param["skip_drop"] = trial.suggest_float(
                "skip_drop", 1e-8, 1.0, log=True
            )

        # fit a model to the training dataset using the defined hyperparameter values
        xgboost_mod = xgb.train(param, dtrain)
        valid_preds = xgboost_mod.predict(dvalid)
        valid_pred_labels = np.rint(valid_preds)
        accuracy = accuracy_score(dvalid.get_label(), valid_pred_labels)

        return accuracy

    def hyperparameter_tuning(self) -> optuna.trial:
        """
        """

        # execute an optimization by using the above objective function wrapped by a
        # lambda function
        study = optuna.create_study()
        study.optimize(
            lambda trial: self.objective(trial, self.dtrain, self.dvalid),
            n_trials=100,
            timeout=600,
        )

        # identify the best model
        optimal_trail = study.best_trial

        return optimal_trail
