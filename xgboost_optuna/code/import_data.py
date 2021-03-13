import os
from urllib.request import urlretrieve
from zipfile import ZipFile

import pandas as pd


class ImportData:
    """
    """

    def __init__(self, file_type: str):

        # define the file type
        self.file_type = file_type

        # define the location of the data zip file
        self.web_loc_url_zip = (
            "https://datahub.io/machine-learning/diabetes/r/diabetes_zip.zip"
        )

        # define the location of the online csv
        self.web_loc_url_csv = (
            "https://datahub.io/machine-learning/diabetes/r/diabetes.csv"
        )

        # define the root path
        self.root_path = os.getcwd()

        # define the local storage path
        self.local_storage = os.path.join(self.root_path, "data")

        # define the local zip filename
        self.local_file_path_zip = os.path.join(self.local_storage, "data.zip")

        # define the local csv filename
        self.local_file_path_zip_csv = os.path.join(self.local_storage,
                                                    "/data/diabetes_csv.csv")

    @staticmethod
    def create_local_folder(local_storage: str):
        """
        Identify if there is a local storage folder, if not create one.
        """

        # identify if the local storage location exists
        try:
            os.stat(local_storage)

        # if not create local storage
        except OSError:
            os.mkdir(local_storage)

    @staticmethod
    def download_data(web_loc_url: str, local_file_path: str):
        """
        Download a file from a supplied url address and store it locally at a
        pre-defined path.
        """

        # download a file from the a URL and save it at a local file path
        urlretrieve(web_loc_url, local_file_path)

    @staticmethod
    def extract_tar_file(local_file_path: str, local_storage: str):
        """
        The extract_tar_file function simply extracts all files contained within the
        supplied zipfile located at the zipfile_path.
        """

        # read the zipfile object
        with ZipFile(local_file_path, "r") as zipObj:

            # extract all the contents of zip file into the data directory
            zipObj.extractall(local_storage)

    @staticmethod
    def import_csv_file(file_location: str) -> pd.DataFrame:
        """
        Import a CSV file to pandas dataframe.
        """

        # import csv from url
        data = pd.read_csv(file_location)

        return data

    def import_data(self) -> pd.DataFrame:
        """
        Download the training and holdout files and store locally.
        """

        # define the data object
        data = None

        # create local storage folder
        self.create_local_folder(self.local_storage)

        # if importing a zip file
        if self.file_type == "zip":

            # download the data from the web and store locally
            self.download_data(self.web_loc_url_zip, self.local_file_path_zip)

            # extract the local zip file
            self.extract_tar_file(self.local_file_path_zip, self.local_storage)

            # import the csv from zip file
            data = self.import_csv_file(self.local_file_path_zip_csv)

        # if importing a csv file
        elif self.file_type == "csv":

            # import the dataset from url
            data = self.import_csv_file(self.web_loc_url_csv)

        return data
