#include <QMetaEnum>
#include <QSize>
#include <QFont>
#include <QBrush>
#include "TaskListModel.hh"

const QFont FONT{"Cascadia Code", 14};

TaskListModel::TaskListModel(Services& services, QObject* parent) : QAbstractListModel{parent}, _services{services} { }

int TaskListModel::rowCount(const QModelIndex& parent) const {
  Q_UNUSED(parent);
  return _services.mylist().size();
}

QVariant TaskListModel::data(const QModelIndex& index, int role) const {
  switch (role) {
    case Qt::FontRole:
      return FONT;
    case Qt::DisplayRole:
      return QString::fromStdString(_services.mylist().at(index.row()).toString());
    default:
      return QVariant{};
  }
}