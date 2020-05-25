#include <cassert>
#include "Test.hh"

void test_Services_illnessesByCategoryOrName_emptyCriteria_allIllnessesAreReturned() {
  Services services;
  services.add(Illness{"category", "name", {{"symptom"}}});

  auto expected = services.data();
  auto result = services.illnessesByCategoryOrName("");

  assert(result == expected);
}

void test_Services_illnessesByCategoryOrName_nonEmptyCriteria_illnessesAreFiltered() {
  Services services;
  services.add(Illness{"category1", "name1", {{"symptom1"}}});
  services.add(Illness{"category12", "name12", {{"symptom12"}}});
  services.add(Illness{"category123", "name123", {{"symptom123"}}});

  std::vector<Illness> expected{{
    Illness{"category12", "name12", {{"symptom12"}}},
    Illness{"category123", "name123", {{"symptom123"}}}}};

  auto result = services.illnessesByCategoryOrName("12");

  assert(result == expected);
}

void runAllTests() {
  test_Services_illnessesByCategoryOrName_emptyCriteria_allIllnessesAreReturned();
  test_Services_illnessesByCategoryOrName_nonEmptyCriteria_illnessesAreFiltered();
}
