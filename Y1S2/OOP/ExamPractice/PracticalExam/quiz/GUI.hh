#ifndef GUI_HH
#define GUI_HH

#include <qwidget.h>
#include <QListWidget>
#include <QFormLayout>
#include <QLineEdit>
#include <QTextEdit>
#include <QPushButton>
#include <QLabel>
#include <QTabWidget>
#include <QSpinBox>
#include <QSlider>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QtWidgets/QMainWindow>
#include <vector>
#include "Observer.hh"
#include "Services.hh"

class PresenterWindow : public QMainWindow, public Observable {
  Q_OBJECT

private:
  Service* _services;

  QListWidget* _questions;
  std::vector<QLineEdit*> _edits;
  QPushButton* _addButton;
  QLabel* _lastOperationError;

  void _populateQuestions();
  void _initialize();
  void _connectSignalsAndSlots();
  void _addQuestion();
  
public:
  PresenterWindow(Service* services, QWidget* parent = nullptr);
  ~PresenterWindow();
};

class ParticipantWindow : public QMainWindow, public Observer {
  Q_OBJECT

private:
  Participant _participant;
  Service* _services;
  std::vector<Question> _answeredQuestions;
  PresenterWindow* _presenterWindow;

  QListWidget* _questions;
  QLineEdit* _answerEdit;
  QPushButton* _answerButton;
  QLabel* _scoreLabel;

  void _populateQuestions();
  void _initialize();
  void _connectSignalsAndSlots();
  
  void _updateTitle();
  void _answerQuestion();

  int _getSelectedIndex();
  void _selectionChanged();

public:
  ParticipantWindow(Service* services, Participant participant, PresenterWindow* presenterWindow, QWidget* parent = nullptr);
  ~ParticipantWindow();

  void update() override;
};

#endif