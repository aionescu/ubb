#include "Model.hh"

static QFont FONT{"Consolas", 14};

Model::Model(Service* services, Astronomer astronomer, QObject* parent)
  : QAbstractTableModel{parent}, _services{services}, _astronomer{astronomer}, _filter{false}
{ }

int Model::rowCount(const QModelIndex& /*parent*/) const {
  if (_filter)
    return _services->starsByConstellation(_astronomer.get<Astronomer::constellation>()).size();
  else
    return _services->getAllData<Star>().size();
}

int Model::columnCount(const QModelIndex& /*parent*/) const {
  return Star::DatumCount;
}

QVariant Model::data(const QModelIndex& index, int role) const {
  if (role == Qt::FontRole)
    return FONT;

  if (role == Qt::DisplayRole) {
    auto stars =
      _filter
      ? _services->starsByConstellation(_astronomer.get<Astronomer::constellation>())
      : _services->getAllData<Star>();

    auto parts = splitString(stars.at(index.row()).toString(), ',');
    return QString::fromStdString(parts.at(index.column()));
  }

  return QVariant{};
}

void Model::reset() {
  this->beginResetModel();
  this->endResetModel();
}

void Model::setFiltering(bool filtering) {
  _filter = filtering;
}