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

class GUI: public QWidget
{
  Q_OBJECT

public:
  GUI(std::vector<Task> tasks, QWidget *parent = nullptr);
  ~GUI();

private:
  std::vector<Task> tasks;

  QListWidget* tasksList;

  QLineEdit* titleEdit;
  QLineEdit* typeEdit;
  QLineEdit* lastPerformedEdit;
  QLineEdit* timesPerformedEdit;
  QLineEdit* visionEdit;

  QPushButton* addTaskButton;
  QPushButton* deleteTaskButton;

  void initGUI();
  int getSelectedIndex();
  void connectSignalsAndSlots();

  void populateTasksList();
  // When an item in the list is clicked, the text boxes get filled with the item's information
  void listItemChanged();

  void addTaskButtonHandler();
  void deleteTaskButtonHandler();

signals:
  void tasksUpdatedSignal();
  void addTaskSignal(
    const std::string& title,
    const std::string& type,
    const std::string& lastPerformed,
    const std::string& timesPerformed,
    const std::string& vision);

public slots:
  void addTask(
    const std::string& title,
    const std::string& type,
    const std::string& lastPerformed,
    const std::string& timesPerformed,
    const std::string& vision);
};

#endif
