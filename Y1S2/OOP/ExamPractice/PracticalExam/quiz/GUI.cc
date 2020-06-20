#include "GUI.hh"

QFont FONT{"Consolas", 14};
const std::vector<QString> LABEL_TEXT{{"&ID:", "&Text:", "&Correct Answer:", "&Score:"}};

void PresenterWindow::_initialize() {
  setWindowTitle("Presenter");

  auto centralWidget = new QWidget;
  auto overallLayout = new QHBoxLayout{centralWidget};

  _questions = new QListWidget;
  _questions->setSelectionMode(QAbstractItemView::NoSelection);
  overallLayout->addWidget(_questions);

  auto rightWidget = new QWidget;
  auto rightLayout = new QVBoxLayout{rightWidget};

  for (std::size_t i = 0; i < LABEL_TEXT.size(); ++i) {
    auto label = new QLabel{LABEL_TEXT.at(i)};
    auto edit = new QLineEdit;

    label->setFont(FONT);
    edit->setFont(FONT);

    label->setBuddy(edit);
    rightLayout->addWidget(label);
    rightLayout->addWidget(edit);

    _edits.push_back(edit);
  }

  _lastOperationError = new QLabel;
  rightLayout->addWidget(_lastOperationError);

  _addButton = new QPushButton{"&Add"};
  _addButton->setFont(FONT);
  rightLayout->addWidget(_addButton);

  overallLayout->addWidget(rightWidget);
  this->setCentralWidget(centralWidget);
}

void PresenterWindow::_connectSignalsAndSlots() {
  QObject::connect(_addButton, &QPushButton::clicked, this, &PresenterWindow::_addQuestion);
}

void PresenterWindow::_addQuestion() {
  try {
    auto id = std::stoi(_edits.at(0)->text().toStdString());

    try {
      _services->getByPredicate<Question>([&](const Question& q) { return q.get<Question::id>() == id; });
      _lastOperationError->setText("ID must be unique.");
      return;
    } catch (...) { }
  } catch (...) {
    _lastOperationError->setText("ID must be an integer.");
    return;
  }

  if (_edits.at(1)->text().toStdString().empty()) {
    _lastOperationError->setText("Text must not be empty.");
    return;
  }

  try {
    Question q{
      std::stoi(_edits.at(0)->text().toStdString()),
      _edits.at(1)->text().toStdString(),
      _edits.at(2)->text().toStdString(),
      std::stoi(_edits.at(3)->text().toStdString())};

    _services->add<Question>(q);
    this->_populateQuestions();
    this->notify();
  } catch (...) {
    return;
  }
}

void PresenterWindow::_populateQuestions() {
  _questions->clear();

  auto questions = _services->getAllData<Question>();
  std::sort(questions.begin(), questions.end(), [&](const Question& a, const Question& b) {
    return a.get<Question::id>() < b.get<Question::id>();
  });

  for (auto question : questions) {
    QString itemInList = QString::fromStdString(question.toString());
      
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(FONT);

    _questions->addItem(item);
  }
}

PresenterWindow::PresenterWindow(Service* services, QWidget* parent) : QMainWindow{parent}, _services{services} {
  _initialize();
  _connectSignalsAndSlots();

  _populateQuestions();
}

PresenterWindow::~PresenterWindow() { }

std::string questionWithoutAnswer(Question q) {
  return
    std::to_string(q.get<Question::id>())
    + "," + q.get<Question::text>()
    + "," + std::to_string(q.get<Question::score>());
}

void ParticipantWindow::_populateQuestions() {
  _questions->clear();

  auto questions = _services->getAllData<Question>();
  std::sort(questions.begin(), questions.end(), [&](const Question& a, const Question& b) {
    return a.get<Question::score>() > b.get<Question::score>();
  });

  for (auto question : questions) {
    QString itemInList = QString::fromStdString(questionWithoutAnswer(question));
      
    QListWidgetItem* item = new QListWidgetItem{itemInList};
    item->setFont(FONT);

    auto it = std::find(_answeredQuestions.begin(), _answeredQuestions.end(), question);

    if (it != _answeredQuestions.end())
      item->setBackground(QBrush{QColor{"Green"}});

    _questions->addItem(item);
  }
}

void ParticipantWindow::_updateTitle() {
  this->setWindowTitle(QString::fromStdString(
    _participant.get<Participant::name>() + " [Score " + std::to_string(_participant.get<Participant::score>()) + "]"
  ));
}

void ParticipantWindow::_initialize() {
  _updateTitle();
  _presenterWindow->addObserver(this);

  auto centralWidget = new QWidget;
  auto overallLayout = new QVBoxLayout{centralWidget};

  _questions = new QListWidget;
  overallLayout->addWidget(_questions);

  _answerButton = new QPushButton{"&Answer"};
  _answerEdit = new QLineEdit;

  _answerButton->setFont(FONT);
  _answerEdit->setFont(FONT);

  overallLayout->addWidget(_answerEdit);
  overallLayout->addWidget(_answerButton);

  this->setCentralWidget(centralWidget);
}

int ParticipantWindow::_getSelectedIndex() {
  if (_questions->count() == 0)
    return -1;

  QModelIndexList index = _questions->selectionModel()->selectedIndexes();

  if (index.size() == 0)
    return -1;

  return index.at(0).row();
}

void ParticipantWindow::_answerQuestion() {
  auto idx = this->_getSelectedIndex();

  auto questions = _services->getAllData<Question>();
  std::sort(questions.begin(), questions.end(), [&](const Question& a, const Question& b) {
    return a.get<Question::score>() > b.get<Question::score>();
  });

  auto question = questions.at(idx);
  auto it = std::find(_answeredQuestions.begin(), _answeredQuestions.end(), question);

  if (it != _answeredQuestions.end())
    return;

  if (_answerEdit->text().toStdString() == question.get<Question::correctAnswer>()) {
    auto p = _participant;
    p.set<Participant::score>(p.get<Participant::score>() + question.get<Question::score>());

    _services->update<Participant>(_participant, p);
    _participant = p;
    _updateTitle();
  }

  _answeredQuestions.push_back(question);
  _populateQuestions();
}

void ParticipantWindow::_selectionChanged() {
  auto idx = this->_getSelectedIndex();
  if (idx < 0)
    return;

  auto questions = _services->getAllData<Question>();
  std::sort(questions.begin(), questions.end(), [&](const Question& a, const Question& b) {
    return a.get<Question::score>() > b.get<Question::score>();
  });

  auto question = questions.at(idx);
  auto it = std::find(_answeredQuestions.begin(), _answeredQuestions.end(), question);

  if (it != _answeredQuestions.end())
    _answerButton->setEnabled(false);
  else
    _answerButton->setEnabled(true);
}

void ParticipantWindow::_connectSignalsAndSlots() {
  QObject::connect(_answerButton, &QPushButton::clicked, this, &ParticipantWindow::_answerQuestion);
  QObject::connect(_questions, &QListWidget::itemSelectionChanged, this, &ParticipantWindow::_selectionChanged);
}

ParticipantWindow::ParticipantWindow(Service* services, Participant participant, PresenterWindow* presenterWindow, QWidget* parent)
  : QMainWindow{parent}, _participant{participant}, _services{services}, _presenterWindow{presenterWindow}
{
  _initialize();
  _connectSignalsAndSlots();

  _populateQuestions();
}

ParticipantWindow::~ParticipantWindow() {
  _presenterWindow->removeObserver(this);
}

void ParticipantWindow::update() {
  _populateQuestions();
}