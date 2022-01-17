#include <QtWidgets/QApplication>
#include "Services.hh"
#include "GUI.hh"

int main(int argc, char** argv) {
  auto services = new Service;
  services->loadFromFile<Question>("Questions.txt");
  services->loadFromFile<Participant>("Participants.txt");

  QApplication app{argc, argv};

  auto presenterWindow = new PresenterWindow{services};
  presenterWindow->show();

  for (auto participant : services->getAllData<Participant>()) {
    auto participantWindow = new ParticipantWindow{services, participant, presenterWindow};
    participantWindow->show();
  }

  auto result = app.exec();

  services->saveToFile<Question>("Questions.txt");

  return result;
}