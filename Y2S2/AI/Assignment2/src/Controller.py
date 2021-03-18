from Map import Map
from Search import Path, Search

class Controller():
  def __init__(self, map: Map) -> None:
    self.__map = map

  @property
  def map(self) -> Map:
    return self.__map

  def run_search(self, search: Search) -> Path:
    (start, end) = self.__map.empty_points()
    return search(self.__map, start, end)
