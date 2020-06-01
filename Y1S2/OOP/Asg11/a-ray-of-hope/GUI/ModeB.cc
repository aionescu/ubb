#include "ModeB.hh"
#include "MylistWindow.hh"

const QFont FONT{"Cascadia Code", 14};
const std::vector<QString> BUTTON_TEXT{{"Next task", "Save Task", "Open External", "Filter Data", "Mylist"}};

ModeB::ModeB(Services& services, QWidget* parent) : QWidget{parent}, _services{services} {
  _initialize();
  _setupSlotsSignals();
  getFocus();
}

void ModeB::_initialize() {
  QHBoxLayout* layout = new QHBoxLayout{this};

  _mylistWidget = new QListWidget;
  _mylistWidget->setSelectionMode(QAbstractItemView::NoSelection);
  layout->addWidget(_mylistWidget);

  _filteredListWidget = new QListWidget;
  _filteredListWidget->setSelectionMode(QAbstractItemView::NoSelection);

  // right side - task information + buttons
  QWidget* rightSide = new QWidget;
  QVBoxLayout* vLayout = new QVBoxLayout{rightSide};

  // Task information
  QWidget* taskDataWidget = new QWidget;
  QFormLayout* formLayout = new QFormLayout{taskDataWidget};

  auto staticLabel = new QLabel{"Current Task:"};
  staticLabel->setFont(FONT);

  _currentTaskLabel = new QLabel{"No Task: Main repo is empty"};
  _currentTaskLabel->setFont(FONT);

  auto typeLabel = new QLabel{"Task type to filter by:"};
  _typeFilter = new QLineEdit;
  typeLabel->setBuddy(_typeFilter);


  typeLabel->setFont(FONT);
  _typeFilter->setFont(FONT);

  auto filterLabel = new QLabel{"Times Performed to filter by:"};
  _timesPerformedFilter = new QLineEdit;
  filterLabel->setBuddy(_timesPerformedFilter);

  filterLabel->setFont(FONT);
  _timesPerformedFilter->setFont(FONT);

  formLayout->addWidget(staticLabel);
  formLayout->addWidget(_currentTaskLabel);

  formLayout->addWidget(typeLabel);
  formLayout->addWidget(_typeFilter);

  formLayout->addWidget(filterLabel);
  formLayout->addWidget(_timesPerformedFilter);

  vLayout->addWidget(taskDataWidget);

  QWidget* buttonsWidget = new QWidget;
  QHBoxLayout* hLayout = new QHBoxLayout;
  auto hLayout2 = new QHBoxLayout;

  for (std::size_t i = 0; i < 2; ++i) {
    auto button = new QPushButton{BUTTON_TEXT.at(i)};
    button->setFont(FONT);

    _buttons.push_back(button);
    hLayout->addWidget(button);
  }

  for (std::size_t i = 2; i < BUTTON_TEXT.size(); ++i) {
    auto button = new QPushButton{BUTTON_TEXT.at(i)};
    button->setFont(FONT);

    _buttons.push_back(button);
    hLayout2->addWidget(button);
  }

  _mylistButton = new QPushButton{"&Mylist"};
  _mylistButton->setFont(FONT);

  auto buttonsLayout = new QVBoxLayout{buttonsWidget};
  buttonsLayout->addLayout(hLayout);
  buttonsLayout->addLayout(hLayout2);

  vLayout->addWidget(buttonsWidget);
  vLayout->addWidget(_mylistButton);

  layout->addWidget(rightSide);
  layout->addWidget(_filteredListWidget);
}

void ModeB::_openExternalButtonHandler() {
  auto filePath = _services.servantTasksFilePath();
  auto extension = filePath.substr(filePath.find_last_of(".") + 1);

  if (extension == "html")
    std::ignore = system(("google-chrome-stable \"" + filePath + "\"").c_str());
  else if (extension == "csv" || extension == "txt")
    std::ignore = system(("libreoffice --calc \"" + filePath + "\"").c_str());
}

void ModeB::_filterButtonHandler() {
  _filteredListWidget->clear();

  auto typeToFilter = _typeFilter->text().toStdString();
  auto maxTimesPerformed = std::stoi(_timesPerformedFilter->text().toStdString());

  for (auto task : _services.tasksByTimesPerformed(typeToFilter, maxTimesPerformed)) {
    auto qstring = QString::fromStdString(task.toString());
    auto item = new QListWidgetItem{qstring};

    item->setFont(FONT);
    _filteredListWidget->addItem(item);
  }
}

void ModeB::_updateMylist() {
  _services.notify();

  _currentTaskLabel->setText(QString::fromStdString(_currentTask.toString()));

  if (_mylistWidget->count() > 0)
    _mylistWidget->clear();

  auto data = _services.servantTasks();

  for (auto task : data) {
    QString itemInList = QString::fromStdString(task.toString());
      
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(FONT);

    _mylistWidget->addItem(item);
  }
}

void ModeB::_setupSlotsSignals() {
  QObject::connect(this, &ModeB::updateMylistSignal, this, &ModeB::_updateMylist);

  QObject::connect(_buttons.at(0), &QPushButton::clicked, this, &ModeB::_nextButtonHandler);
  QObject::connect(_buttons.at(1), &QPushButton::clicked, this, &ModeB::_saveButtonHandler);
  QObject::connect(_buttons.at(2), &QPushButton::clicked, this, &ModeB::_openExternalButtonHandler);
  QObject::connect(_buttons.at(3), &QPushButton::clicked, this, &ModeB::_filterButtonHandler);

  QObject::connect(_mylistButton, &QPushButton::clicked, this, &ModeB::_mylistButtonHandler);
}

void ModeB::_saveButtonHandler() {
  try {
    _services.save(_currentTask.title());
  } catch (...) { }

  emit updateMylistSignal();
}

void ModeB::_nextButtonHandler() {
  auto nextResult = _services.next();

  if (!nextResult.first)
    return;

  _currentTask = nextResult.second;

  emit updateMylistSignal();
}

void ModeB::_mylistButtonHandler() {
  auto mylistWindow = new MylistWindow{_services, _mylistWidget->size(), this};
  mylistWindow->show();
}

void ModeB::getFocus() {
  _services.setMode("B");

  _services.resetNext();
  _nextButtonHandler();
}