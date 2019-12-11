from exceptions import Repo_error

'''
def Validator(id_validate):
    pass
'''


def validate(storage, new_act):
    """
    validator simplu
    :return:
    :param storage:
    :param new_act:
    :return: daca e valida ,daca nu arunca eroare
    """
    for i in storage:
        if i == new_act:
            list_of_pers = i.person_id
            for j in new_act.person_id:
                if j in list_of_pers:
                    raise Repo_error('the pers is no Doctor Strange, we as HUMANS CAN NOT BEND SPACE AND TIME can not do 2 things at the same time')
    return True
