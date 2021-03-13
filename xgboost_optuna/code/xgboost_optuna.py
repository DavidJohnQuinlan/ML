import argparse

from data_modelling import DataModelling
from import_data import ImportData
from prepare_data import PrepareDataset


class XGBoostOptuna:
    """
    """

    def __init__(self, parsed_args):
        """
        """
        self.file_type = parsed_args['file_type']

    def xgboost_optuna(self):
        """
        """

        # load the pima indian diabetes dataset
        pima = ImportData(file_type=self.file_type).import_data()

        # prepare the pima dataset
        train, val, test = PrepareDataset(
            data=pima).prepare_data()

        # fit a xgboost model to the training dataset also apply hyperparmeter tuning
        DataModelling(train, val, test).data_modelling()


if __name__ == "__main__":
    """
    Entrypoint of the module. It requires the following arguments:
        - file_type: the type of file to download, either zip or csv.
    """

    # setup parser and arguments
    parser = argparse.ArgumentParser(description="Run Adthena text classification task")

    # parse the training file path
    parser.add_argument("-ft", "--file_type", help="Either zip or csv", default=None)

    # extract the parsed values
    parsed = vars(parser.parse_args())

    # instantiate XGBoostOptuna class
    xgboost_optuna = XGBoostOptuna(parsed)

    # run the text classification service
    xgboost_optuna.xgboost_optuna()
