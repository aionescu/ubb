#ifndef GUI_MODE_B_HH
#define GUI_MODE_B_HH

#include <qwidget.h>
#include <QListWidget>
#include <QFormLayout>
#include <QLineEdit>
#include <QTextEdit>
#include <QPushButton>
#include <QLabel>
#include <QtWidgets/QApplication>
#include <QSpinBox>
#include <QSlider>
#include <QHBoxLayout>
#include "Services.hh"
#include "ModeWidget.hh"

class ModeB : public QWidget, public ModeWidget {
  Q_OBJECT

private:
  Services& _services;
  Task _currentTask;

  QLabel* _currentTaskLabel;
  QListWidget* _mylistWidget;
  std::vector<QPushButton*> _buttons;

  void _initialize();
  void _setupSlotsSignals();
  void _updateMylist();

  void _saveButtonHandler();
  void _nextButtonHandler();

public:
  ModeB(Services& services, QWidget* parent = nullptr);
  void getFocus();

signals:
  void updateMylistSignal();
};

#endif