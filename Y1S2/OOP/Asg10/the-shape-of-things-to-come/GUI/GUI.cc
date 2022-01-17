
#include "GUI.hh"
#include "ModeA.hh"
#include "ModeB.hh"

GUI::GUI(Services& services, QWidget* parent) : QWidget{parent}, _services{services} {
  _initialize();
  _setupSignalsSlots();
}

void GUI::_initialize() {
  QHBoxLayout* layout = new QHBoxLayout{this};
  _tabWidget = new QTabWidget;

  auto modeA = new ModeA{_services, _tabWidget};
  auto modeB = new ModeB{_services, _tabWidget};

  _modes.push_back(modeA);
  _modes.push_back(modeB);

  _tabWidget->addTab(modeA, "Mode A");
  _tabWidget->addTab(modeB, "Mode B");

  layout->addWidget(_tabWidget);
  modeA->getFocus();
}

void GUI::_setupSignalsSlots() {
  QObject::connect(_tabWidget, &QTabWidget::currentChanged, this, &GUI::_switchMode);
}

void GUI::_switchMode(int index) {
  _modes.at(index)->getFocus();
}