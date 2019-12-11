from repository.in_memory_repository import RepositoryPerson
from repository.in_memory_activities import RepositoryActivity
from repository.FileRepositoryJson import FileRepositoryPerson
from repository.GenericFileRepo import GenericFileRepository
from service.service import Service
from user_interface.console import Console
import os

def main():
    option = int(input('choose option: '))
    if option == 1:
        repository = FileRepositoryPerson('person.txt')
        activities = RepositoryActivity()
        service = Service(repository, activities)
        ui = Console(service)
        if os.stat('person.txt').st_size == 0:
            ui.random_add()
    # adds 10 el
            ui.random_add_activity()
        ui.run_console()
    if option == 2:
        repository = GenericFileRepository('person.pkl')
        activities = GenericFileRepository('activities.pkl')
        service = Service(repository, activities)
        ui = Console(service)
        # if os.stat('person.pkl').st_size == 0:
        # ui.random_add()
        # adds 10 el
        # ui.random_add_activity()
        ui.run_console()
    if option == 3:
        repository = RepositoryPerson()
        activities = RepositoryActivity()
        service = Service(repository, activities)
        ui = Console(service)
        # if os.stat('person.pkl').st_size == 0:
        ui.random_add()
        # adds 10 el
        ui.random_add_activity()
        ui.run_console()


'''
to do exceptions 
'''
main()
