#ifndef GUI_MYLIST_WINDOW_HH
#define GUI_MYLIST_WINDOW_HH

#include <QtWidgets/QMainWindow>
#include <QListView>
#include <QSize>
#include "Observer.hh"
#include "Services.hh"
#include "TaskListModel.hh"

class MylistWindow : public QMainWindow, public Observer {
  Q_OBJECT

private:
  Services& _services;
  TaskListModel* _model;
  QListView* _view;

public:
  MylistWindow(Services& services, QSize size, QWidget* parent = nullptr);
  ~MylistWindow();

  void update() override;
};

#endif
