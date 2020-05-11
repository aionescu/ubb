#include <QDebug>
#include "GUI.hh"

const QFont FONT{"Cascadia Code", 14};
const std::vector<QString> LABEL_TEXT{{"&Title:", "&Type:", "&Last performed:", "&Times performed:", "&Vision:"}};
const std::vector<QString> BUTTON_TEXT{{"Add task", "Delete task"}};

GUI::GUI(QWidget* parent) : QWidget{parent} {
  _services.setFilePath("Tasks.csv");
  _services.setMode("A");

  this->initGUI();
  this->connectSignalsAndSlots();
  this->populateTasksList();
}

GUI::~GUI() { }

void GUI::initGUI() {
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

  for (int i = 0; i < LINE_EDIT_COUNT; ++i) {
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

  for (int i = 0; i < BUTTON_COUNT; ++i) {
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

void GUI::connectSignalsAndSlots() {
  // when the vector of tasks is updated - re-populate the list
  //QObject::connect(this, SIGNAL(tasksUpdatedSignal()), this, SLOT(populateTasksList()));
  QObject::connect(this, &GUI::tasksUpdatedSignal, this, &GUI::populateTasksList);

  // add a connection: function listItemChanged() will be called when an item in the list is selected
  QObject::connect(_tasksList, &QListWidget::itemSelectionChanged, this, [this]() {this->listItemChanged(); });

  // add button connections
  QObject::connect(_buttons.at(0), &QPushButton::clicked, this, &GUI::addTaskButtonHandler);
  QObject::connect(_buttons.at(1), &QPushButton::clicked, this, &GUI::deleteTaskButtonHandler);

  // connect the addTask signal to the addTask slot, which adds a task to vector
  QObject::connect(
    this, SIGNAL(addTaskSignal(const std::vector<std::string>&)),
    this, SLOT(addTask(const std::vector<std::string>&)));
}

void GUI::addTask(const std::vector<std::string>& parts)
{
  try {
    _services.add(taskOfParts(parts));
  } catch (...) { }

  // emit the signal: the tasks were updated
  emit tasksUpdatedSignal();
}

void GUI::addTaskButtonHandler() {
  std::vector<std::string> parts;

  // read data from the textboxes and add the new task
  for (auto lineEdit : _lineEdits)
    parts.push_back(lineEdit->text().toStdString());

  // emit the addTask signal
  emit addTaskSignal(parts);
}

void GUI::deleteTaskButtonHandler() {
  // get the selected index and delete the task
  int idx = this->getSelectedIndex();

  auto tasks = _services.allTasks();

  if (idx < 0 || (std::size_t)idx >= tasks.size())
    return;

  auto task = tasks[idx];
  _services.remove(task.title());

  // emit the signal: the tasks were updated
  emit tasksUpdatedSignal();
}

void GUI::populateTasksList() {
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

int GUI::getSelectedIndex() {
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

void GUI::listItemChanged() {
  auto tasks = _services.allTasks();

  int idx = this->getSelectedIndex();
  if (idx < 0 || (std::size_t)idx >= tasks.size())
    return;

  auto task = tasks[idx];
  auto parts = task.toParts();

  for (auto i = 0; i < LINE_EDIT_COUNT; ++i)
    _lineEdits.at(i)->setText(QString::fromStdString(parts.at(i)));
}