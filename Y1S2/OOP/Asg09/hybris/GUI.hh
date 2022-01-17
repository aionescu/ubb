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
#include <QtCharts/QChartView>
#include <QtCharts/QPieSeries>
#include <QtCharts/QPieSlice>
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
  QTabWidget* _tabWidget;
  QtCharts::QChart* _chart;

  void initGUI();
  int getSelectedIndex();
  void connectSignalsAndSlots();

  void populateTasksList();
  void listItemChanged();

  void addTaskButtonHandler();
  void deleteTaskButtonHandler();

  void _updateChart();

signals:
  void tasksUpdatedSignal();
  void addTaskSignal(const std::vector<std::string>& parts);

public slots:
  void addTask(const std::vector<std::string>& parts);
};

#endif