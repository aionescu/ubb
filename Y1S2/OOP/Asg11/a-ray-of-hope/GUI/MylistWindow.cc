#include <QSortFilterProxyModel>
#include <QHBoxLayout>
#include "MylistWindow.hh"

MylistWindow::MylistWindow(Services& services, QSize size, QWidget* parent) : QMainWindow{parent}, _services{services} {
  _services.addObserver(this);

  _model = new TaskListModel{_services};

  _view = new QListView{this};
  _view->setModel(_model);
  _view->setFixedSize(size);

  setFixedSize(size);
}

MylistWindow::~MylistWindow() {
  _services.removeObserver(this);
}

void MylistWindow::update() {
  _view->reset();
}