#ifndef GUI_HH
#define GUI_HH

#include "Model.hh"
#include "Observer.hh"

class AstronomerWindow : public QMainWindow, public Observer {
  Q_OBJECT

private:
  Service* _services;
  Model* _model;
  Astronomer _astronomer;

  std::vector<QLineEdit*> _edits;
  std::vector<QPushButton*> _buttons;
  QTableView* _table;
  QListWidget* _filteredStars;
  QCheckBox* _checkBox;

  void _initialize();
  void _connectSignalsAndSlots();
  void _populateFilteredStars();

  int _getSelectedIndex();
  void _addStar();
  void _updateOnlyMy(int state);

public:
  AstronomerWindow(Service* services, Astronomer astronomer, QWidget* parent = nullptr);
  ~AstronomerWindow();

  void update() override;
};

#endif