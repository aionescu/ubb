from service.service import *
from repository.in_memory_activities import *
from  repository.in_memory_repository import *


def test_clase():
    test_p=Person(100,'Marin',231)
    test_a=Activity(21,[100,1],'15.12',1400,'Nimicuri')
    assert (test_p.id_entity == 100)
    assert (test_p.name == 'Marin')
    assert (test_p.phone_number == 231)
    assert (test_a.time == 1400)
    assert (test_a.date =='15.12')
    assert (test_a.person_id == [100 ,1])
    assert (test_a.description == 'Nimicuri')
    assert (test_a.id_entity == 21)
    print('ok')


def test_add_person():
    repository = RepositoryPerson()
    test_serv  = Service(repository,None)
    test_serv.add_person(12, 'Bljat', 244)
    test_serv.add_person(1,'Abel',101)
    test_serv.add_person(2,'kain',102)
    test_serv.add_person(3,'Krimer',443)
    assert (repository.read(2).name == 'kain')
    assert (repository.read(2).name !='Kia')
    assert (repository.read(2).name != 'Kain')
    assert (repository.read(2).id_entity == 2)
    assert (repository.read(3).name == 'Krimer')
    assert (repository.read(3).id_entity == 3)
    assert (repository.read(3).phone_number == 443)
    assert (repository.read(3).name != 'krimer')
    assert (repository.read(12).name == 'Bljat')
    assert (repository.read(1).name == 'Abel')
    assert (repository.read(1).id_entity == 1)
    assert (repository.read(1).name != 'aeel')


def test_update():
    repository = RepositoryPerson()
    test_up  = Service(repository,None)



def test_delete():
    repository = RepositoryPerson()
    test_del  = Service(repository,None)
    test_del.add_person(1, 'Abel', 101)
    test_del.add_person(2, 'kain', 102)
    test_del.add_person(3, 'Krimer', 443)
    assert (repository.read(2).name == 'kain')
    assert (repository.read(2).name !='Kia')
    assert (repository.read(2).name != 'Kain')
    assert (repository.read(2).id_entity == 2)
    assert (repository.read(3).name == 'Krimer')
    assert (repository.read(3).id_entity == 3)
    assert (repository.read(3).phone_number == 443)
    assert (repository.read(3).name != 'krimer')
    assert (repository.read(1).name == 'Abel')
    assert (repository.read(1).id_entity == 1)
    assert (repository.read(1).name != 'aeel')


    #test_del.del_person(2)
    #assert (repository.read(1).name == None)
    #test_del.add_person(12, 'Paul', 112)
   # assert  (repository.read(4).name == 'Paul')


def test_service():
    pass


def toatetestele():
    test_clase()
    test_add_person()
    test_service()
    print('kk')
    test_update()
    test_delete()

toatetestele()



