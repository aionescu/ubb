#ifndef GUI_MODE_WIDGET_HH
#define GUI_MODE_WIDGET_HH

class ModeWidget {
public:
  virtual ~ModeWidget();
  virtual void getFocus() = 0;
};

#endif