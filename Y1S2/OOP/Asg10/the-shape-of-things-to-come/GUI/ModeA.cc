#include <QDebug>
#include "ModeA.hh"

const QFont FONT{"Cascadia Code", 14};
const std::vector<QString> LABEL_TEXT{{"&Title:", "&Type:", "&Last performed:", "&Times performed:", "&Vision:"}};
const std::vector<QString> BUTTON_TEXT{{"Add task", "Update Task", "Delete task"}};

ModeA::ModeA(Services& services, QWidget* parent) : QWidget{parent}, _services{services} {
  this->_initialize();
  this->connectSignalsAndSlots();
  this->getFocus();
}

void ModeA::_initialize() {
  //Taskral layout of the window
  QHBoxLayout* layout = new QHBoxLayout{this};

  // left side - just the list
  _tasksList = new QListWidget;
  // set the selection model
  _tasksList->setSelectionMode(QAbstractItemView::SingleSelection);
  layout->addWidget(_tasksList);

  // right side - task information + buttons
  QWidget* rightSide = new QWidget;
  QVBoxLayout* vLayout = new QVBoxLayout{rightSide};

  // Task information
  QWidget* taskDataWidget = new QWidget;
  QFormLayout* formLayout = new QFormLayout{taskDataWidget};

  std::vector<QLabel*> labels;

  for (std::size_t i = 0; i < LABEL_TEXT.size(); ++i) {
    auto lineEdit = new QLineEdit;
    lineEdit->setFont(FONT);

    auto label = new QLabel{LABEL_TEXT.at(i)};
    label->setBuddy(lineEdit);
    label->setFont(FONT);

    formLayout->addRow(label, lineEdit);

    _lineEdits.push_back(lineEdit);
    labels.push_back(label);
  }

  vLayout->addWidget(taskDataWidget);

  // buttons
  QWidget* buttonsWidget = new QWidget;
  QHBoxLayout* hLayout = new QHBoxLayout{buttonsWidget};

  for (std::size_t i = 0; i < BUTTON_TEXT.size(); ++i) {
    auto button = new QPushButton{BUTTON_TEXT.at(i)};
    button->setFont(FONT);

    _buttons.push_back(button);
    hLayout->addWidget(button);
  }

  vLayout->addWidget(buttonsWidget);

  // add everything to the big layout
  layout->addWidget(_tasksList);
  layout->addWidget(rightSide);
}

void ModeA::connectSignalsAndSlots() {
  // when the vector of tasks is updated - re-populate the list
  //QObject::connect(this, SIGNAL(tasksUpdatedSignal()), this, SLOT(populateTasksList()));
  QObject::connect(this, &ModeA::tasksUpdatedSignal, this, &ModeA::populateTasksList);

  // add a connection: function listItemChanged() will be called when an item in the list is selected
  QObject::connect(_tasksList, &QListWidget::itemSelectionChanged, this, [this]() {this->listItemChanged(); });

  // add button connections
  QObject::connect(_buttons.at(0), &QPushButton::clicked, this, &ModeA::addTaskButtonHandler);
  QObject::connect(_buttons.at(1), &QPushButton::clicked, this, &ModeA::updateTaskButtonHandler);
  QObject::connect(_buttons.at(2), &QPushButton::clicked, this, &ModeA::deleteTaskButtonHandler);

  // connect the addTask signal to the addTask slot, which adds a task to vector
  QObject::connect(
    this, SIGNAL(addTaskSignal(const std::vector<std::string>&)),
    this, SLOT(addTask(const std::vector<std::string>&)));

  QObject::connect(
    this, SIGNAL(updateTaskSignal(const std::vector<std::string>&)),
    this, SLOT(updateTask(const std::vector<std::string>&)));
}

void ModeA::addTask(const std::vector<std::string>& parts)
{
  try {
    _services.add(taskOfParts(parts));
  } catch (...) { }

  emit tasksUpdatedSignal();
}

void ModeA::updateTask(const std::vector<std::string>& parts)
{
  try {
    _services.update(taskOfParts(parts));
  } catch (...) { }

  emit tasksUpdatedSignal();
}

void ModeA::addTaskButtonHandler() {
  std::vector<std::string> parts;

  // read data from the textboxes and add the new task
  for (auto lineEdit : _lineEdits)
    parts.push_back(lineEdit->text().toStdString());

  // emit the addTask signal
  emit addTaskSignal(parts);
}

void ModeA::updateTaskButtonHandler() {
  std::vector<std::string> parts;

  // read data from the textboxes and add the new task
  for (auto lineEdit : _lineEdits)
    parts.push_back(lineEdit->text().toStdString());

  // emit the addTask signal
  emit updateTaskSignal(parts);
}

void ModeA::deleteTaskButtonHandler() {
  // get the selected index and delete the task
  int idx = this->getSelectedIndex();

  auto tasks = _services.allTasks();

  if (idx < 0 || (std::size_t)idx >= tasks.size())
    return;

  auto task = tasks.at(idx);
  _services.remove(task.title());

  // emit the signal: the tasks were updated
  emit tasksUpdatedSignal();
}

void ModeA::populateTasksList() {
  // clear the list, if there are elements in it
  if (_tasksList->count() > 0)
    _tasksList->clear();

  auto tasks = _services.allTasks();

  for (auto task : tasks) {
    QString itemInList = QString::fromStdString(task.toString());
      
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(FONT);

    _tasksList->addItem(item);
  }

  // set the selection on the first item in the list
  if (tasks.size() > 0)
    _tasksList->setCurrentRow(0);
}

int ModeA::getSelectedIndex() {
  if (_tasksList->count() == 0)
    return -1;

  // get selected index
  QModelIndexList index = _tasksList->selectionModel()->selectedIndexes();

  if (index.size() == 0) {
    for (auto lineEdit : _lineEdits)
      lineEdit->clear();

    return -1;
  }

  int idx = index.at(0).row();
  return idx;
}

void ModeA::listItemChanged() {
  auto tasks = _services.allTasks();

  int idx = this->getSelectedIndex();
  if (idx < 0 || (std::size_t)idx >= tasks.size())
    return;

  auto task = tasks.at(idx);
  auto parts = task.toParts();

  for (std::size_t i = 0; i < LABEL_TEXT.size(); ++i)
    _lineEdits.at(i)->setText(QString::fromStdString(parts.at(i)));
}

void ModeA::getFocus() {
  _services.setMode("A");
  populateTasksList();
}