#include "GUI.hh"

static QFont FONT{"Consolas", 14};
const std::vector<QString> LABEL_TEXT{{"&Star name:", "&Right ascension:", "&Declination:", "&Diameter:"}};

AstronomerWindow::AstronomerWindow(Service* services, Astronomer astronomer, QWidget* parent)
  : QMainWindow{parent}, _services{services}, _model{new Model{services, astronomer}}, _astronomer{astronomer}
{
  _services->addObserver(this);
  _initialize();
  _connectSignalsAndSlots();
}

AstronomerWindow::~AstronomerWindow() {
  _services->removeObserver(this);
}

void AstronomerWindow::_initialize() {
  this->setWindowTitle(QString::fromStdString(_astronomer.get<Astronomer::name>()));

  auto centralWidget = new QWidget;
  auto layout = new QHBoxLayout{centralWidget};

  _table = new QTableView;
  _table->setModel(_model);

  layout->addWidget(_table);

  auto midWidget = new QWidget;
  auto midLayout = new QVBoxLayout{midWidget};

  for (std::size_t i = 0; i < LABEL_TEXT.size(); ++i) {
    auto label = new QLabel{LABEL_TEXT.at(i)};
    auto edit = new QLineEdit;

    label->setFont(FONT);
    edit->setFont(FONT);

    label->setBuddy(edit);
    midLayout->addWidget(label);
    midLayout->addWidget(edit);

    _edits.push_back(edit);
  }

  auto addButton = new QPushButton{"&Add"};
  addButton->setFont(FONT);

  _buttons.push_back(addButton);
  midLayout->addWidget(addButton);

  auto label = new QLabel{"&Filter:"};
  auto edit = new QLineEdit;

  label->setFont(FONT);
  edit->setFont(FONT);

  label->setBuddy(edit);
  midLayout->addWidget(label);
  midLayout->addWidget(edit);

  _edits.push_back(edit);

  auto viewButton = new QPushButton{"&View"};
  viewButton->setFont(FONT);

  _buttons.push_back(viewButton);
  midLayout->addWidget(viewButton);

  auto onlyMyWidget = new QWidget;
  auto onlyMyLayout = new QHBoxLayout{onlyMyWidget};

  auto onlyMyLabel = new QLabel{"&Only my constellation:"};
  onlyMyLabel->setFont(FONT);

  _checkBox = new QCheckBox;
  onlyMyLabel->setBuddy(_checkBox);

  onlyMyLayout->addWidget(onlyMyLabel);
  onlyMyLayout->addWidget(_checkBox);

  midLayout->addWidget(onlyMyWidget);
  layout->addWidget(midWidget);
  
  _filteredStars = new QListWidget;
  layout->addWidget(_filteredStars);

  this->setCentralWidget(centralWidget);
}

void AstronomerWindow::_connectSignalsAndSlots() {
  QObject::connect(_buttons.at(0), &QPushButton::clicked, this, &AstronomerWindow::_addStar);
  QObject::connect(_edits.at(4), &QLineEdit::textChanged, this, &AstronomerWindow::_populateFilteredStars);
  QObject::connect(_checkBox, &QCheckBox::stateChanged, this, &AstronomerWindow::_updateOnlyMy);
}

void AstronomerWindow::_populateFilteredStars() {
  _filteredStars->clear();

  auto text = _edits.at(4)->text().toStdString();

  if (text.empty())
    return;

  auto stars = _services->getAllData<Star>();

  for (auto star : stars) {
    if (star.get<Star::name>().find(text) == std::string::npos)
      continue;

    QString itemInList = QString::fromStdString(star.toString());
      
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(FONT);

    _filteredStars->addItem(item);
  }
}

int AstronomerWindow::_getSelectedIndex() { return -1; }

void AstronomerWindow::_addStar() {
  try {
    auto name = _edits.at(0)->text().toStdString();
    auto constellation = _astronomer.get<Astronomer::constellation>();
    auto rightAscension = std::stoi(_edits.at(1)->text().toStdString());
    auto declination = std::stoi(_edits.at(2)->text().toStdString());
    auto diameter = std::stoi(_edits.at(3)->text().toStdString());

    _services->tryAddStar(name, constellation, rightAscension, declination, diameter);

    this->update();
  } catch (...) { }
}

void AstronomerWindow::_updateOnlyMy(int state) {
  _model->setFiltering(state != 0);
  this->update();
}

void AstronomerWindow::update() {
    _model->reset();
    _table->reset();

    _populateFilteredStars();
}