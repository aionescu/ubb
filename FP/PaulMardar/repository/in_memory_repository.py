from Domain.domain import Person


class RepositoryPerson:
    def __init__(self):
        self.__storage = {}

    def create(self, entity):
        """
        creator + validator from memory
        :param entity:
        :return:
        """
        id_entity = entity.id_entity
        if id_entity in self.__storage:
            raise KeyError('The id already exists')
        self.__storage[id_entity] = entity

    def read(self, the_id=None):
        """
        returns one of multiples variables ,
        functie facuta zmeker care returneaza fie tot storage-ul fie doar elementul cautat
        in functie de key from memory
        :param the_id:
        :return: mai multe , fie toate valorile,fie cea cautata in fucntie de id
        """
        if the_id is None:
            return self.__storage.values()
        if the_id in self.__storage:
            return self.__storage[the_id]
        return None

    def update(self, val):
        the_id = val.id_entity
        """
        updates the person daca o gaseste daca nu da eroare from memory
        :param the_id:
        :return: nimica
        """
        if the_id not in self.__storage:
            raise KeyError('id not in storage')
        else:
            self.__storage[the_id] = val

    def delete(self, the_id):
        """
        sterge un element daca il gaseste , daca nu da erroare from memory
        :param the_id:
        :return:
        """
        if the_id not in self.__storage:
            raise KeyError('id not in list')
        del self.__storage[the_id]
