#ifndef GUI_TASK_LIST_MODEL_HH
#define GUI_TASK_LIST_MODEL_HH

#include <QAbstractListModel>
#include "Services.hh"

class TaskListModel : public QAbstractListModel {
  Q_OBJECT

private:
  Services& _services;

public:
  TaskListModel(Services& services, QObject* parent = nullptr);

  int rowCount(const QModelIndex& parent = QModelIndex{}) const override;
  QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
};

#endif
