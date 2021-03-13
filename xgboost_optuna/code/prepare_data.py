from sklearn.model_selection import train_test_split


class PrepareDataset:
    """
    """

    def __init__(self, data):
        """
        """
        self.dataset = data

    def prepare_data(self):
        """
        Prepares the pima dataset for use by the xgboost model.
        """

        # prepare the class variable
        self.dataset["class"] = self.dataset["class"].replace(
            {"tested_positive": 1, "tested_negative": 0}
        )

        # split the dataset into a training and validation dataset in a 70:30 split
        train, test = train_test_split(
            self.dataset,
            test_size=0.3,
            stratify=self.dataset["class"],
            random_state=7,
        )

        # split the test dataset into a validation and test dataset in a 15:15 split
        val, test = train_test_split(
            test, stratify=test['class'], test_size=0.50, random_state=42
        )

        return train, val, test
