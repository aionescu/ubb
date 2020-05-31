#ifndef GUI_GUI_HH
#define GUI_GUI_HH

#include <qwidget.h>
#include <QTabWidget>
#include "Services.hh"
#include "ModeWidget.hh"

class GUI : public QWidget {
  Q_OBJECT

private:
  Services& _services;
  QTabWidget* _tabWidget;
  std::vector<ModeWidget*> _modes;

  void _initialize();
  void _setupSignalsSlots();

  void _switchMode(int index);
  
public:
  GUI(Services& services, QWidget* parent = nullptr);
};

#endif