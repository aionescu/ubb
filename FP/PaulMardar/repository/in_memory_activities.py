from Domain.domain import Activity


class RepositoryActivity:
    def __init__(self):
        self.__storage = {}

    def create(self, entity):
        """
        creazq un obiect si il pune in storage from memory
        :param entity:
        :return:
        """
        id_entity = entity.id_entity
        if id_entity in self.__storage:
            raise KeyError('The id already exists')
        self.__storage[id_entity] = entity

    def read(self, the_id=None):
        """
        functie de return fie toate el din storage fie dupa id :)) from memory
        :param the_id:
        :return:
        """
        if the_id is None:
            return self.__storage.values()
        if the_id in self.__storage:
            return self.__storage[the_id]
        return None

    def update(self, activity):
        the_id = activity.id_entity
        """
        update simplu dupa id  from memory
        :param the_id:
        :return:
        """
        if the_id not in self.__storage:
            raise KeyError('id not in storage')
        else:
            '''no_pers = int(input('number of persons participating: '))
            pers = []
            for i in range(no_pers):
                pers.append(int(input('id of person: ')))
            date = input('date: ')
            time = input('time: ')
            description = input('description')'''
            self.__storage[the_id] = activity


    def add_person_to_activ(self, p_id, id_activitaty):
        """

        :param p_id:
        :param id_activitaty:   lista
        :return:
        """

        for i in range(len(id_activitaty)):
            self.__storage[id_activitaty[i]].person_id.append(p_id)


    def delete_id_from_list(self,id_to_del):
        """
        deleting from memory
        :param id_to_del:
        :return:
        """
        rez = {}
        for i in self.__storage:
            for j in range(len(self.__storage[i].person_id)):
                if self.__storage[i].person_id[j] == id_to_del:
                    rez[i] = j
                    del self.__storage[i].person_id[j]
                    break
        return rez

    def delete(self, the_id):
        """
        from memory
        :param the_id:
        :return: nichts
        """
        if the_id not in self.__storage:
            raise KeyError('id not in list')
        del self.__storage[the_id]
