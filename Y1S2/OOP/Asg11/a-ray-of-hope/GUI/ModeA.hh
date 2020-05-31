#ifndef GUI_MODE_A_HH
#define GUI_MODE_A_HH

#include <qwidget.h>
#include <QListWidget>
#include <QFormLayout>
#include <QLineEdit>
#include <QTextEdit>
#include <QPushButton>
#include <QLabel>
#include <QtWidgets/QApplication>
#include <QShortcut>
#include <QSpinBox>
#include <QSlider>
#include <QHBoxLayout>
#include "Domain.hh"
#include "Services.hh"
#include "ModeWidget.hh"

class ModeA : public QWidget, public ModeWidget {
  Q_OBJECT

public:
  ModeA(Services& services, QWidget* parent = nullptr);
  void getFocus() override;

private:
  Services& _services;

  QListWidget* _tasksList;
  std::vector<QLineEdit*> _lineEdits;
  std::vector<QPushButton*> _buttons;

  QPushButton *_undoButton, *_redoButton, *_mylistButton;
  QShortcut *_undoShortcut, *_redoShortcut;

  void _initialize();
  int getSelectedIndex();
  void connectSignalsAndSlots();

  void populateTasksList();
  void listItemChanged();

  void addTaskButtonHandler();
  void updateTaskButtonHandler();
  void deleteTaskButtonHandler();
  
  void undoButtonHandler();
  void redoButtonHandler();
  void mylistButtonHandler();

signals:
  void tasksUpdatedSignal();
  void addTaskSignal(const std::vector<std::string>& parts);
  void updateTaskSignal(const std::vector<std::string>& parts);

public slots:
  void addTask(const std::vector<std::string>& parts);
  void updateTask(const std::vector<std::string>& parts);
};

#endif