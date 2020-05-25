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
#include "Services.hh"

class GUI : public QWidget {
  Q_OBJECT

private:
  Services _services;

  QListWidget* _illnessList, *_symptomsList;
  QLineEdit* _filterLineEdit, *_symptomsLineEdit;
  QPushButton* _symptomsButton;

  void _populateIllnessList();
  void _populateSymptomsList();
  void _initialize();
  void _connectSignalsAndSlots();
  
public:
  GUI(QWidget* parent = nullptr);
};

#endif