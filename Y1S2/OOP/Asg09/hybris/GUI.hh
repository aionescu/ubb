#ifndef GUI_HH
#define GUI_HH

#include <qwidget.h>
#include <QListWidget>
#include <QFormLayout>
#include <QLineEdit>
#include <QTextEdit>
#include <QPushButton>
#include <QLabel>
#include "Domain.hh"
#include "Services.hh"

constexpr const int LINE_EDIT_COUNT = 5;
constexpr const int BUTTON_COUNT = 2;

class GUI: public QWidget
{
  Q_OBJECT

public:
  GUI(QWidget* parent = nullptr);
  ~GUI();

private:
  Services _services;

  QListWidget* _tasksList;
  std::vector<QLineEdit*> _lineEdits;
  std::vector<QPushButton*> _buttons;

  void initGUI();
  int getSelectedIndex();
  void connectSignalsAndSlots();

  void populateTasksList();
  void listItemChanged();

  void addTaskButtonHandler();
  void deleteTaskButtonHandler();

signals:
  void tasksUpdatedSignal();
  void addTaskSignal(const std::vector<std::string>& parts);

public slots:
  void addTask(const std::vector<std::string>& parts);
};

#endif