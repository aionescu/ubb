import pickle


class RepositoryError(Exception):
    pass


class GenericFileRepository:

    def __init__(self, filename):
        self.__storage = {}
        self.__filename = filename
        self.__load_from_file()

    def __load_from_file(self):
        with open(self.__filename, 'rb') as f_read:
            self.__storage = pickle.load(f_read)

    def __save_to_file(self):
        with open(self.__filename, 'wb') as f_write:
            pickle.dump(self.__storage, f_write)

    def create(self, entity):
        id_entity = entity.id_entity
        if id_entity in self.__storage:
            raise RepositoryError('The entity id already exists!')
        self.__storage[id_entity] = entity
        self.__save_to_file()

    def read(self, id_entity=None):
        if id_entity is None:
            return self.__storage

        if id_entity in self.__storage:
            return self.__storage[id_entity]
        return None

    def update(self, entity):
        id_entity = entity.id_entity
        if id_entity not in self.__storage:
            raise RepositoryError('There is no entity with that id!')
        self.__storage[id_entity] = entity
        self.__save_to_file()

    def delete(self, id_entity):
        if id_entity not in self.__storage:
            raise RepositoryError('There is no entity with that id!')
        del self.__storage[id_entity]
        self.__save_to_file()
