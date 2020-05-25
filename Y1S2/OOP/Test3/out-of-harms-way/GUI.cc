#include <string>
#include "GUI.hh"

QFont FONT{"Consolas", 14};

GUI::GUI(QWidget* parent) : QWidget{parent} {
  _services.loadFromFile("../Illnesses.txt");

  _initialize();
  _connectSignalsAndSlots();

  _populateIllnessList();
}

void GUI::_initialize() {
  auto overallLayout = new QHBoxLayout{this};

  auto illnessesList = new QListWidget;
  overallLayout->addWidget(illnessesList);

  auto middleWidget = new QWidget;
  auto middleLayout = new QVBoxLayout{middleWidget};

  auto filterLabel = new QLabel{"&Filter by:"};
  auto filterLineEdit = new QLineEdit;

  filterLabel->setFont(FONT);
  filterLineEdit->setFont(FONT);

  filterLabel->setBuddy(filterLineEdit);

  middleLayout->addWidget(filterLabel);
  middleLayout->addWidget(filterLineEdit);

  auto symptomsLabel = new QLabel{"&Show symptoms for:"};
  auto symptomsLineEdit = new QLineEdit;

  symptomsLabel->setFont(FONT);
  symptomsLineEdit->setFont(FONT);

  symptomsLabel->setBuddy(symptomsLineEdit);

  middleLayout->addWidget(symptomsLabel);
  middleLayout->addWidget(symptomsLineEdit);

  auto showSymptomsButton = new QPushButton{"&Show symptoms"};
  showSymptomsButton->setFont(FONT);

  middleLayout->addWidget(showSymptomsButton);

  overallLayout->addWidget(middleWidget);

  auto symptomsList = new QListWidget;
  overallLayout->addWidget(symptomsList);

  _illnessList = illnessesList;
  _symptomsList = symptomsList;
  _filterLineEdit = filterLineEdit;
  _symptomsLineEdit = symptomsLineEdit;
  _symptomsButton = showSymptomsButton;
}

void GUI::_connectSignalsAndSlots() {
  QObject::connect(_symptomsButton, &QPushButton::clicked, this, &GUI::_populateSymptomsList);
  QObject::connect(_filterLineEdit, &QLineEdit::textChanged, this, &GUI::_populateIllnessList);
}

void GUI::_populateIllnessList() {
  _illnessList->clear();

  auto criteria = _filterLineEdit->text().toStdString();

  for (auto illness : _services.illnessesByCategoryOrName(criteria)) {
    QString itemInList = QString::fromStdString(illness.category() + " | " + illness.name());
      
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(FONT);

    _illnessList->addItem(item);
  }
}

void GUI::_populateSymptomsList() {
  _symptomsList->clear();

  auto nameToSearch = _symptomsLineEdit->text().toStdString();
  Illness illness;

  try {
    illness = _services.getByName(nameToSearch);
  } catch (...) {
    return;
  }

  for (auto symptom : illness.symptoms()) {
    QString itemInList = QString::fromStdString(symptom);
      
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(FONT);

    _symptomsList->addItem(item);
  }
}