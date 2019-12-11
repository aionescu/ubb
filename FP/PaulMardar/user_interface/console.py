import random
from exceptions import Repo_error

lista_nume = ['Popescu', 'Ionescu', 'Popa', 'Pop', 'Niță', 'Nițu', 'Constantinescu', 'Stan', 'Stanciu', 'Dumitrescu',
              'Dima', 'Gheorghiu', 'Ioniță', 'Marin', 'Tudor', 'Dobre', 'Barbu', 'Nistor', 'Florea', 'Frățilă', 'Dinu',
              'Dinescu', 'Georgescu', 'Stoica', 'Diaconu', 'Diaconescu', 'Mocanu', 'Voinea', 'Albu', 'Miller', 'Milner',
              'Goku', 'Petrescu', 'Manole', 'Cristea', 'Toma', 'Stănescu', 'Pușcașu', 'Tomescu', 'Sava', 'Ciobanu',
              'Rusu', 'Ursu', 'Lupu', 'Munteanu', 'Mehedințu', 'Andreescu', 'Bardea', 'Mihăilescu', 'Iancu', 'Blaga',
              'Teodoru', 'Teodorescu', 'Moise', 'Moisescu', 'Călinescu', 'Tabacu', 'Negoiță', 'Smith', 'Johnson',
              'Williams', 'Brown', 'Jones', 'Seed']
lista_prenume = ['Abel', 'Achim', 'Alexandru', 'Alex', 'Alfred', 'Alin', 'Amadeus', 'Ben', 'Benedikt', 'Bernard',
                 'Bobby', 'Bogdan', 'BORIS', 'Cain', 'Calin', 'Calistrat', 'Cezar', 'Cesaro', 'Cezarone', 'Claudiu',
                 'Claus', 'Codin', 'Constantin', 'Dominic', 'Dorel', 'Dorian', 'Dorin', 'Doru', 'Jasper', 'Jordan',
                 'Johnas', 'Joseph', 'Jared', 'Kaiser', 'Kaleb', 'Karl', 'Keanu', 'Kevin', 'Klaus', 'Leonidas', 'Lion',
                 'Liviu', 'Lazar', 'Marcel', 'Mihai', 'Miron', 'Mugur', 'Manea', 'Emanuel', 'Mirel', 'Adela', 'Adina',
                 'Abby', 'Ada', 'Nechita', 'Afrodita', 'Agata', 'Alice', 'Alina', 'ALexandra']
lista_descrieri_activ = [
    'The study of permeability will be conducted over a range of pressures with a maximum operating pressure',
    ' literalmente absolut nimic', 'uite stam ce sa facem ?',
    '…we shall fight on the beaches, we shall fight on the landing grounds, we shall fight in the fields and in the streets, we shall fight in the hills; we shall never surrender…',
    'The enemy win their battles from the air! They knock out my panzers with American armor-piercing shells.',
    'We could do with some of those razor blades, Herr Reichsmarshall.',
    'The fruits of victory are tumbling into our mouths too quickly.',
    'If everyone is thinking alike, someone isn’t thinking.',
    'We shall go on to the end, we shall fight in France, we shall fight on the seas and oceans, we shall fight with growing confidence and growing strength in the air, we shall defend our Island, then our Empire beyond the seas, armed and guarded by the British Fleet, would carry on the struggle, until, in God',
    'The BOOK IT! Program is offered to children in grades kindergarten through sixth grade only, including homeschoolers',
    'nichts',
    'nema niente, absolut nimic, m a lasat inspiratia',
    'Come join a team where striking out is a GOOD thing! The CHEA Bowling Club is open to ages 4 to 18 and is held at a local bowling alley.']


class Console:
    def __init__(self, service):
        self.__service = service

    def __handle_add_person(self):
        try:
            person_id = int(input('id: '))
            name = input('name: ')
            phone_number = input('phone number: ')
            self.__service.add_person(person_id, name, phone_number)
            print('person added!')
        except KeyError:
            print('ID already exists!')
        except ValueError as ve:
            print('Error', ve)

    def random_add(self):
        for i in range(20):
            x = random.choice(lista_prenume) + ' ' + random.choice(lista_nume)
            self.__service.add_person(i + 1, x, '0' + str(random.randrange(100000000, 999999999)))
        print('20 random persons added')

    def random_add_activity(self):
        for i in range(10):
            activity_id = i + 1
            person_id = [1, 2, i + 5]
            date = str(str(10 + i) + '.12')
            time = str(200 * (i + 1))
            description = random.choice(lista_descrieri_activ)
            # str('descrierea activitatii cu numarul ' + str(i + 1))
            self.__service.add_activity(activity_id, person_id, date, time, description)
        print('10 randoms activity added')

    '''
    
        def random_add_activity(self):
        for i in range(5)
            activity_id=i
            n=random.randrange(1,10)
            for j in range(n):
                person_id.append(random.choice(1,10).format(i + 1))))
            self.__service.add_activity(activity_id, person_id, date, time, description)
    '''

    def __handle_add_activity(self):
        try:
            activity_id = int(input('id: '))
            n = int(input('how many participate: '))
            person_id = []
            for i in range(n):
                person_id.append(int(input('id of the {} who participates: '.format(i + 1))))
            date = input('date: ')
            time = input('time: ')
            description = input('description: ')
            self.__service.add_activity(activity_id, person_id, date, time, description)
            print('activity added!')
        except KeyError:
            print('ID already exists')
        except ValueError as ve:
            print(ve)
        except Repo_error as ve:
            print(ve)

    def __handle_print_person(self):
        pers = self.__service.print_persons()
        for i in pers:
            print(str(i))

    def __handle_print_activity(self):
        activity = self.__service.print_activity()
        for i in activity:
            print(str(i))

    def __handle_delete_person(self):

        try:
            id_del = int(input('introdu id ul de sters'))
        except ValueError:
            print("id not found")

        # to do sterge din lista de id persoane de din activitati
        self.__service.del_person(id_del)

    def __hadle_update_activity(self):

        try:
            id_del_a = int(input('introdu id de update al activitatii'))
            nr_pers = int(input('introdu numarul de persoane'))
            lista_persoane = []
            for i in range(nr_pers):
                lista_persoane.append(int(input('id of person: ')))
            data = input('date: ')
            timpul = input('time: ')
            descriere = input('description')
            self.__service.update_activity(id_del_a, data, lista_persoane, timpul, descriere)
        except ValueError:
            print('introdu un id')

    def __hadle_update_person(self):
        try:
            id_del_a = int(input('introdu id de udpdatat al persoanei'))
            self.__service.update_person(id_del_a)
        except ValueError:
            print('introdu un id')

    @staticmethod
    def show_menu():
        print('1. add person')
        print('2. add activity')
        print('3. show persons')
        print('4. show activities')
        print('5  delete a person')
        print('6 delete a activity')
        print('7 update person')
        print('8 update activity')
        print('9 search for person  based on name')
        print('10 search for person based on phone')
        print('11 search for activity based on date')
        print('12 search for activity based on time')
        print('13 search for activity based on description')
        print('14 search for busiest day')
        print('15 activitys for a given date')
        print('16 activities for a given person')
        print('17 for adding a person to multiple activitys')
        print('undo for undo-ing')
        print('redo for redo-ing')
        print('exit. for exit')


    def run_console(self):
        while True:
            try:
                self.show_menu()
                op = input('Option: ')
                if op == '1':
                    self.__handle_add_person()
                elif op == '2':
                    self.__handle_add_activity()
                elif op == '3':
                    self.__handle_print_person()
                elif op == '4':
                    self.__handle_print_activity()
                elif op == '5':
                    new_id = int(input(' introdu id ul to delete '))
                    self.__service.del_person(new_id)
                elif op == '6':
                    new_activ_id = int(input(' introdu id ul to delete pt activitate '))
                    self.__service.del_activity(new_activ_id)
                elif op == '7':
                    self.__hadle_update_person()
                elif op == '8':
                    self.__hadle_update_activity()
                elif op == '9':
                    self.__handle_search_person_name()
                elif op == '10':
                    self.__handle_search_person_phone()
                elif op == '11':
                    self.__handle_search_activity_date()
                    print('based activity on date')
                elif op == '12':
                    self.__handle_search_activity_time()
                    print('based activity based on time')
                elif op == '13':
                    self.__handle_search_activity_description()
                    print('based on description ')
                elif op == '14':
                    self.__handle_busiest_date()
                elif op == '15':
                    self.__handle_activity_given_date()
                elif op == '16':
                    self.__handle_activity_given_person()
                elif op == 'exit':
                    break
                elif op == '17':
                    self.__handle_add_person_to_multiple_activ()
                elif op == 'undo':
                    try:
                        self.__service.undo()
                    except Exception as ve:
                        print(ve)
                elif op =='redo':
                    try:
                        self.__service.redo()
                    except Exception as ve:
                        print(ve)
            except TabError:
                print('erroare necunoscuta :((')

    def __handle_busiest_date(self):
        rez = self.__service.sort_by_date()
        for i in rez:
            for j in i[2]:
                print(self.__service.return_act(j))

    def __handle_add_person_to_multiple_activ(self):
        lista_activitati=[]
        personana_id=int(input('introdu id-ul persoanei'))
        lenght_list_activ=int(input('numarul de activitati'))
        for i in range(lenght_list_activ):
            new_activitate=int(input('introdu id ul unei activitati pt append'))
            lista_activitati.append(new_activitate)
        print(lista_activitati)
        self.__service.add_person_to_activitys(personana_id,lista_activitati)


    def __handle_search_person_name(self):
        name_to_find = input('name to find: ')
        rezult = self.__service.search_persons_name(name_to_find)
        for j in rezult:
            print(j)

    def __handle_search_person_phone(self):
        phone_to_find = input('phone to find: ')
        rezult = self.__service.search_person_phone(phone_to_find)
        for j in rezult:
            print(j)

    def __handle_search_activity_date(self):
        date_to_find = input('date to find: ')
        rezult = self.__service.search_activity_date(date_to_find)
        for j in rezult:
            print(j)

    def __handle_search_activity_time(self):
        """
        handler pentru partial string maching a
        :return: nimic
        """
        time_to_find = input('time to find: ')
        rezult = self.__service.search_activity_time(time_to_find)
        for j in rezult:
            print(j)

    def __handle_search_activity_description(self):
        description_to_find = input('description to find: ')
        rezult = self.__service.search_activity_description(description_to_find)
        for j in rezult:
            print(j)

    def __handle_activity_given_date(self):
        """
        handler pentru given date
        :return: nimic
        """
        given_date = input('date to print activitys for: ')
        rezult = self.__service.activities_given_date(given_date)
        for i in range(0, 2401):
            for j in rezult:
                if j.time == str(i):
                    print(j)

    def __handle_activity_given_person(self):
        """
        handler pentru  given person
        :return: 0
        """
        give_person = int(input('introdu id-ul unei perrsoane pentru'))
        rezult = self.__service.activities_given_person(give_person)
        for j in rezult:
            print(j)


'''
           print(rezult[j].pers_id)
            print(rezult[j].name)
            print(rezult[j].phone_number)

'''
