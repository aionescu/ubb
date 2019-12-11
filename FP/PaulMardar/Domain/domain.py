from Domain.entity import Entity


class Person(Entity):
    def __init__(self, pers_id, name, phone_number):
        super(Person, self).__init__(pers_id)
        self.__name = name
        self.__phone_number = phone_number

    @property
    def name(self):
        return self.__name

    @property
    def phone_number(self):
        return self.__phone_number

    @name.setter
    def name(self, new_name):
        self.__name = new_name

    @phone_number.setter
    def phone_number(self, new_phone_number):
        self.__phone_number = new_phone_number

    def __str__(self):
        return 'id: {}\nname: {}\nphone number: {}\n'.format(self.id_entity, self.name, self.phone_number)


class Activity(Entity):
    def __init__(self, activity_id, person_id, date, time, description):
        super(Activity, self).__init__(activity_id)
        self.person_id = person_id
        self.__date = date
        self.__time = time
        self.__description = description

    def __eq__(self, other):
        return self.__date == other.__date and self.__time == other.__time

    @property
    def person_id(self):
        return self.__person_id

    @property
    def date(self):
        return self.__date

    @property
    def time(self):
        return self.__time

    @property
    def description(self):
        return self.__description

    @person_id.setter
    def person_id(self, new_value):
        self.__person_id = new_value

    @date.setter
    def date(self, new_date):
        self.__date = new_date

    @time.setter
    def time(self, new_time):
        self.__time = new_time

    @description.setter
    def description(self, new_description):
        self.__description = new_description

    def __str__(self):
        return 'id: {}\nperson id: {}\ndate: {}\n' \
               'time: {}\ndescription: {}'.format(self.id_entity, self.person_id, self.date,
                                                  self.time, self.description)
