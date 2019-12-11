from Domain.domain import Person, Activity
from service.validator import validate
from service.action import *
from exceptions import *

class Service:
    def __init__(self, repository_pers, repository_activities):
        self.__repository_pers = repository_pers
        self.__repository_activities = repository_activities

        self.__done_actions, self.__undone_actions = [], []

    def undo(self):
        try:
            last = self.__done_actions.pop()
        except:
            raise InvalidUndoError()
    
        last.roll_back()
        self.__undone_actions.append(last)

    def redo(self):
        try:
            last = self.__undone_actions.pop()
        except:
            raise InvalidRedoError()
    
        last.apply()
        self.__done_actions.append(last)

    def __do(self, action):
        action.apply()
        self.__done_actions.append(action)
        self.__undone_actions.clear()

    def add_person_to_activitys(self, the_id, activitys_ids):
        """
        to use ca undo la delete person
        :param the_id:
        :param activitys_ids: lista cu id urile activitatilor
        :return:
        """
        self.__repository_activities.add_person_to_activ(the_id, activitys_ids)

    def add_person(self, the_id, name, phone_number):
        """
        basic create
        :param the_id:
        :param name:
        :param phone_number:
        :return:
        """
        person = Person(the_id, name, phone_number)
        self.__do(CreateAction(self.__repository_pers, person))

    def add_activity(self, activity_id, person_id, date, time, description):
        """
         basic create activ
        :param activity_id:
        :param person_id:
        :param date:
        :param time:
        :param description:
        :return:
        """
        activity = Activity(activity_id, person_id, date, time, description)
        if validate(self.__repository_activities.read(), activity):
            self.__do(CreateAction(self.__repository_activities, activity))

    def del_person(self, the_id):
        """
         delete persoana
        :param the_id:
        :return:
        """
        pers = self.__repository_pers.read(the_id)
        
        try:
            self.__do(DeleteAction(self.__repository_pers, pers))
        except Exception:
            print('deleting person from activity id list error')

    def del_activity(self, activ_id):
        """
        :param activ_id:
        :return:
        """
        activ = self.__repository_activities.read(activ_id)
        self.__do(DeleteAction(self.__repository_activities, activ))

    def update_person(self, the_id):
        """
         update
        :param the_id:
        :return:
        """
        pers = self.__repository_pers.read(the_id)
        name = input('new name: ')
        phone_number = input('new phone number: ')
        new_pers = Person(the_id, name, phone_number)

        self.__do(UpdateAction(self.__repository_pers, pers, new_pers))

    def update_activity(self, activ_id, date, persons, time, description):
        """

        :param activ_id:
        :return:
        """
        activ = Activity(activ_id, persons, date, time, description)
        old = self.__repository_activities.read(activ_id)

        self.__do(UpdateAction(self.__repository_activities, old, activ))

    def print_persons(self):
        """

        :return:  read-ul persoane
        """
        return self.__repository_pers.read()

    def print_activity(self):
        """

        :return:  read-ul activitaty
        """
        return self.__repository_activities.read()

    def sort_by_date(self):
        """

        :return: copie final rez -1
        """
        rez = {}
        for i in self.__repository_activities.read():
            rez[i.date] = [i.date, 0, []]
        for i in self.__repository_activities.read():
            rez[i.date][1] += 1
            rez[i.date][2].append(i.id_entity)
        final_rez = sorted(rez.values(), key=lambda x: x[1])
        return final_rez[::-1]

    def return_act(self, the_id):
        """

        :param the_id:
        :return:  un singur obiect
        """
        return self.__repository_activities.read(the_id)

    def search_persons_name(self, name_search):
        """

        :param name_search:
        :return: returneaza lista dupa parametrul cautat
        """
        maches = []
        for i in self.__repository_pers.read():
            if name_search.lower() in i.name.lower():
                maches.append(i)
        return maches

    def search_person_phone(self, phone_search):
        """

        :param phone_search:
        :return:  returneaza lista dupa parametrul cautat
        """
        maches = []
        for i in self.__repository_pers.read():
            if phone_search in i.phone_number:
                maches.append(i)
        return maches

    def search_activity_date(self, date_search):
        """

        :param date_search:
        :return: returneaza lista dupa parametrul cautat
        """
        maches = []
        for i in self.__repository_activities.read():
            if date_search in i.date:
                maches.append(i)
        return maches

    def search_activity_time(self, time_search):
        """

        :param time_search:
        :return: returneaza lista dupa parametrul cautat
        """
        maches = []
        for i in self.__repository_activities.read():
            if time_search in i.time:
                maches.append(i)
        return maches

    def search_activity_description(self, description_search):
        """

        :param description_search:
        :return: returneaza lista dupa parametrul cautat
        """
        maches = []
        for i in self.__repository_activities.read():
            if description_search.lower() in i.description.lower():
                maches.append(i)
        return maches

    def activities_given_date(self, given_date):
        """

        :param given_date:
        :return: returneaza lista dupa parametrul cautat
        """
        maches = []
        for i in self.__repository_activities.read():
            if given_date == i.date:
                maches.append(i)
        return maches

    def activities_given_person(self, person_given):
        """

        :param person_given:
        :return: returneaza lista dupa parametrul cautat
        """
        maches = []
        for i in self.__repository_activities.read():
            if person_given in i.person_id:
                maches.append(i)
        return maches


'''       
for i in self.__repository_pers.read():
    if i.name == name_search:
        return ('gaseste numele ')
'''
'''
statistics punem aici 
undo si redo tot aici punem 
'''
