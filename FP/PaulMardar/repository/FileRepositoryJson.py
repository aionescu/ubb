import json
from Domain.domain import Person
import os


class FileRepositoryPerson:
    def __init__(self, file_name):
        self.__filename = file_name
        self.__storage = {}

    def __load_from_file(self):
        try:
            with open(self.__filename, 'r') as f_read:
                saved_persons = json.load(f_read)
                self.__storage.clear()
                for saved_pers in saved_persons:
                    pers = Person(*saved_pers)
                    self.__storage[pers.id_entity] = pers
        except FileNotFoundError:
            self.__storage = {}

    def __save_to_file(self):
        to_save = []
        for pers in self.__storage.values():
            pers_repo = [pers.id_entity, pers.name, pers.phone_number]
            to_save.append(pers_repo)
        with open(self.__filename, 'w') as f_write:
            json.dump(to_save, f_write)

    def create(self, pers):
        if os.stat('person.txt').st_size != 0:
            self.__load_from_file()
        if pers.id_entity in self.__storage:
            raise KeyError('The vote id already exists')
        self.__storage[pers.id_entity] = pers
        self.__save_to_file()

    def read(self, pers_id=None):
        self.__load_from_file()
        if pers_id is None:
            return self.__storage.values()
        if pers_id in self.__storage:
            return self.__storage[pers_id]
        return None

    def delete(self, pers_id):
        self.__load_from_file()
        if pers_id not in self.__storage:
            raise KeyError('there is no vote with that id')
        del self.__storage[pers_id]
        self.__save_to_file()
