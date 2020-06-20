#ifndef MODEL_HH
#define MODEL_HH

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
#include <QCheckBox>
#include <QTableView>
#include <QAbstractTableModel>
#include <QtWidgets/QMainWindow>
#include <vector>
#include "Services.hh"

class Model : public QAbstractTableModel {
  Q_OBJECT

private:
  Service* _services;
  Astronomer _astronomer;
  bool _filter;

public:
  Model(Service* services, Astronomer astronomer, QObject *parent = nullptr);
  int rowCount(const QModelIndex &parent = QModelIndex{}) const ;
  int columnCount(const QModelIndex &parent = QModelIndex{}) const;
  QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

  void reset();
  void setFiltering(bool filtering);
};

#endif