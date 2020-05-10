#include <QDebug>
#include "GUI.hh"

GUI::GUI(std::vector<Task> tasks, QWidget* parent): QWidget{parent}, tasks{tasks}
{
  this->initGUI();
  this->connectSignalsAndSlots();
  this->populateTasksList();
}

GUI::~GUI()
{
}

void GUI::initGUI()
{
  //Taskral layout of the window
  QHBoxLayout* layout = new QHBoxLayout{this};

  // left side - just the list
  this->tasksList = new QListWidget{};
  // set the selection model
  this->tasksList->setSelectionMode(QAbstractItemView::SingleSelection);
  layout->addWidget(this->tasksList);

  // right side - gene information + buttons
  QWidget* rightSide = new QWidget{};
  QVBoxLayout* vLayout = new QVBoxLayout{rightSide};

  // gene information
  QWidget* taskDataWidget = new QWidget{};
  QFormLayout* formLayout = new QFormLayout{taskDataWidget};

  this->titleEdit = new QLineEdit{};
  this->typeEdit = new QLineEdit{};
  this->lastPerformedEdit = new QLineEdit{};
  this->timesPerformedEdit = new QLineEdit{};
  this->visionEdit = new QLineEdit{};

  QFont f{"Verdana", 15};

  QLabel* titleLabel = new QLabel{"&Title:"};
  titleLabel->setBuddy(this->titleEdit);

  QLabel* typeLabel = new QLabel{ "&Type: " };
  typeLabel->setBuddy(this->typeEdit);

  QLabel* lastPerformedLabel = new QLabel{ "&Last performed:" };
  lastPerformedLabel->setBuddy(this->lastPerformedEdit);

  QLabel* timesPerformedLabel = new QLabel{ "&Times performed:" };
  timesPerformedLabel->setBuddy(this->timesPerformedEdit);

  QLabel* visionLabel = new QLabel{ "&Vision:" };
  visionLabel->setBuddy(this->visionEdit);

  titleLabel->setFont(f);
  typeLabel->setFont(f);
  lastPerformedLabel->setFont(f);
  timesPerformedLabel->setFont(f);
  visionLabel->setFont(f);

  this->titleEdit->setFont(f);
  this->typeEdit->setFont(f);
  this->lastPerformedEdit->setFont(f);
  this->timesPerformedEdit->setFont(f);
  this->visionEdit->setFont(f);

  formLayout->addRow(titleLabel, this->titleEdit);
  formLayout->addRow(typeLabel, this->typeEdit);
  formLayout->addRow(lastPerformedLabel, this->lastPerformedEdit);
  formLayout->addRow(timesPerformedLabel, this->timesPerformedEdit);
  formLayout->addRow(visionLabel, this->visionEdit);

  vLayout->addWidget(taskDataWidget);

  // buttons
  QWidget* buttonsWidget = new QWidget{};
  QHBoxLayout* hLayout = new QHBoxLayout{buttonsWidget};
  
  this->addTaskButton = new QPushButton("Add Task");
  this->addTaskButton->setFont(f);
  this->deleteTaskButton = new QPushButton("Delete Task");
  this->deleteTaskButton->setFont(f);
  hLayout->addWidget(this->addTaskButton);
  hLayout->addWidget(this->deleteTaskButton);

  vLayout->addWidget(buttonsWidget);

  // add everything to the big layout
  layout->addWidget(this->tasksList);
  layout->addWidget(rightSide);
}

void GUI::connectSignalsAndSlots()
{
  // when the vector of genes is updated - re-populate the list
  //QObject::connect(this, SIGNAL(genesUpdatedSignal()), this, SLOT(populateTasksList()));
  QObject::connect(this, &GUI::tasksUpdatedSignal, this, &GUI::populateTasksList);

  // add a connection: function listItemChanged() will be called when an item in the list is selected
  QObject::connect(this->tasksList, &QListWidget::itemSelectionChanged, this, [this]() {this->listItemChanged(); });

  // add button connections
  QObject::connect(this->addTaskButton, &QPushButton::clicked, this, &GUI::addTaskButtonHandler);
  QObject::connect(this->deleteTaskButton, &QPushButton::clicked, this, &GUI::deleteTaskButtonHandler);

  // connect the addTask signal to the addTask slot, which adds a gene to vector
    QObject::connect(this, SIGNAL(addTaskSignal(const std::string&, const std::string&, const std::string&, const std::string&, const std::string&)),
                        this, SLOT(addTask(const std::string&, const std::string&, const std::string&, const std::string&, const std::string&)));
}

void GUI::addTask(
    const std::string& title,
    const std::string& type,
    const std::string& lastPerformed,
    const std::string& timesPerformed,
    const std::string& vision) {
  this->tasks.push_back(Task{title, type, lastPerformed, std::stoi(timesPerformed), vision});

  // emit the signal: the genes were updated
  emit tasksUpdatedSignal();
}

void GUI::addTaskButtonHandler()
{
  // read data from the textboxes and add the new gene
  QString title = this->titleEdit->text();
  QString type = this->typeEdit->text();
  QString lastPerformed = this->lastPerformedEdit->text();
  QString timesPerformed = this->timesPerformedEdit->text();
  QString vision = this->visionEdit->text();

  // emit the addTask signal
  emit addTaskSignal(
    title.toStdString(),
    type.toStdString(),
    lastPerformed.toStdString(),
    timesPerformed.toStdString(),
    vision.toStdString());
}

void GUI::deleteTaskButtonHandler() {
  // get the selected index and delete the gene
  int idx = this->getSelectedIndex();

  if (idx < 0 || idx >= this->tasks.size())
    return;

  this->tasks.erase(this->tasks.begin() + idx);

  // emit the signal: the genes were updated
  emit tasksUpdatedSignal();
}

void GUI::populateTasksList() {
  // clear the list, if there are elements in it
  if (this->tasksList->count() > 0)
    this->tasksList->clear();

  for (auto g : this->tasks) {
    QString itemInList = QString::fromStdString(
      g.title()
      + ", " + g.type()
      + ", " + g.lastPerformed()
      + ", " + std::to_string(g.timesPerformed())
      + ", " + g.vision());
      
    QFont f{"Verdana", 15};
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(f);
    this->tasksList->addItem(item);
  }

  // set the selection on the first item in the list
  if (this->tasks.size() > 0)
    this->tasksList->setCurrentRow(0);
}

int GUI::getSelectedIndex() {
  if (this->tasksList->count() == 0)
    return -1;

  // get selected index
  QModelIndexList index = this->tasksList->selectionModel()->selectedIndexes();

  if (index.size() == 0) {
    this->titleEdit->clear();
    this->typeEdit->clear();
    this->lastPerformedEdit->clear();
    this->timesPerformedEdit->clear();
    this->visionEdit->clear();

    return -1;
  }

  int idx = index.at(0).row();
  return idx;
}

void GUI::listItemChanged() {
  int idx = this->getSelectedIndex();
  if (idx < 0 || idx >= this->tasks.size())
    return;

  auto task = this->tasks[idx];

  this->titleEdit->setText(QString::fromStdString(task.title()));
  this->typeEdit->setText(QString::fromStdString(task.type()));
  this->lastPerformedEdit->setText(QString::fromStdString(task.lastPerformed()));
  this->timesPerformedEdit->setText(QString::fromStdString(std::to_string(task.timesPerformed())));
  this->visionEdit->setText(QString::fromStdString(task.vision()));
}
