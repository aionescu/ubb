#include <QtWidgets/QApplication>
#include <QSpinBox>
#include <QSlider>
#include <QHBoxLayout>
#include "GUI.hh"

int main(int argc, char *argv[])
{
    QApplication a{argc, argv};

    GUI gui;
    gui.show();

    return a.exec();
}
