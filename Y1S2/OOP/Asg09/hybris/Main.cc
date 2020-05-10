#include <QtWidgets/QApplication>
#include <QSpinBox>
#include <QSlider>
#include <QHBoxLayout>
#include "GUI.hh"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    std::vector<Task> tasks{
        Task{"1", "a", "11-11-1111", 1, "a"},
        Task{"2", "b", "22-22-2222", 2, "b"},
        Task{"3", "c", "33-33-3333", 3, "c"}
    };

    GUI gui{tasks};
    gui.show();

    return a.exec();
}
