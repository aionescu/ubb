#include "ModeB.hh"

const QFont FONT{"Cascadia Code", 14};
const std::vector<QString> BUTTON_TEXT{{"Next task", "Save Task"}};

ModeB::ModeB(Services& services, QWidget* parent) : QWidget{parent}, _services{services} {
  _initialize();
  _setupSlotsSignals();
  getFocus();
}

void ModeB::_initialize() {
    QHBoxLayout* layout = new QHBoxLayout{this};

  // left side - just the list
  _mylistWidget = new QListWidget;
  // set the selection model
  _mylistWidget->setSelectionMode(QAbstractItemView::NoSelection);
  layout->addWidget(_mylistWidget);

  // right side - task information + buttons
  QWidget* rightSide = new QWidget;
  QVBoxLayout* vLayout = new QVBoxLayout{rightSide};

  // Task information
  QWidget* taskDataWidget = new QWidget;
  QFormLayout* formLayout = new QFormLayout{taskDataWidget};

  std::vector<QLabel*> labels;

  auto staticLabel = new QLabel{"Current Task:"};
  staticLabel->setFont(FONT);

  _currentTaskLabel = new QLabel{"No Task: Main repo is empty"};
  _currentTaskLabel->setFont(FONT);

  formLayout->addWidget(staticLabel);
  formLayout->addWidget(_currentTaskLabel);

  vLayout->addWidget(taskDataWidget);

  QWidget* buttonsWidget = new QWidget;
  QHBoxLayout* hLayout = new QHBoxLayout{buttonsWidget};

  for (std::size_t i = 0; i < BUTTON_TEXT.size(); ++i) {
    auto button = new QPushButton{BUTTON_TEXT.at(i)};
    button->setFont(FONT);

    _buttons.push_back(button);
    hLayout->addWidget(button);
  }

  vLayout->addWidget(buttonsWidget);
  layout->addWidget(rightSide);
}

void ModeB::_updateMylist() {
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

void ModeB::getFocus() {
  _services.setMode("B");

  _services.resetNext();
  _nextButtonHandler();
}