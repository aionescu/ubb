#ifndef GUI_HH
#define GUI_HH

#include <chrono>
#include <iostream>
#include <qwidget.h>
#include <QTabWidget>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFileDialog>
#include <QMessageBox>
#include <QSlider>
#include <QLabel>
#include <QProcess>
#include <QStringList>
#include <QTemporaryFile>

const QFont FONT{"Fira Code", 14};
const QString OUT_PATH{"__blurred.png"};

class GUI: public QWidget {
  Q_OBJECT

private:
  QPushButton *_chooseFileBtn, *_blurBtn, *_blurMPIBtn;
  QSlider *_radiusSlider;
  QLabel *_imgLbl, *_timeLbl;
  QString _imgPath;
  std::chrono::time_point<std::chrono::high_resolution_clock> _blurStart;

  void _initialize() {
    auto layout = new QVBoxLayout{this};

    _chooseFileBtn = new QPushButton{"Choose File"};
    _chooseFileBtn->setFont(FONT);
    QObject::connect(_chooseFileBtn, &QPushButton::clicked, this, &GUI::_chooseFileBtnClicked);

    _blurBtn = new QPushButton{"Blur"};
    _blurBtn->setFont(FONT);
    _blurBtn->setDisabled(true);
    QObject::connect(_blurBtn, &QPushButton::clicked, this, [=] {
      _blurBtnClicked(false);
    });

    _blurMPIBtn = new QPushButton{"Blur MPI"};
    _blurMPIBtn->setFont(FONT);
    _blurMPIBtn->setDisabled(true);
    QObject::connect(_blurMPIBtn, &QPushButton::clicked, this, [=] {
      _blurBtnClicked(true);
    });

    auto btnsWidget = new QWidget{this};
    auto btnsLayout = new QHBoxLayout{btnsWidget};

    auto blurWidget = new QWidget{this};
    auto blurLayout = new QHBoxLayout{blurWidget};

    blurLayout->addWidget(_blurBtn);
    blurLayout->addWidget(_blurMPIBtn);
    blurWidget->setLayout(blurLayout);

    btnsLayout->addWidget(_chooseFileBtn);
    btnsLayout->addWidget(blurWidget);
    btnsWidget->setLayout(btnsLayout);

    auto sliderWidget = new QWidget{this};
    auto sliderLayout = new QHBoxLayout{sliderWidget};
    auto sliderLabel = new QLabel{"20"};

    _radiusSlider = new QSlider{Qt::Horizontal, this};
    _radiusSlider->setRange(0, 100);
    _radiusSlider->setValue(20);

    sliderLayout->addWidget(_radiusSlider);
    sliderLayout->addWidget(sliderLabel);
    sliderWidget->setLayout(sliderLayout);

    QObject::connect(_radiusSlider, &QSlider::valueChanged, this, [=] {
      sliderLabel->setText(QString::number(_radiusSlider->value()));
    });

    _imgLbl = new QLabel{this};
    _imgLbl->setText("No image selected");
    _imgLbl->setFont(FONT);

    _timeLbl = new QLabel{this};
    _timeLbl->setFont(FONT);

    layout->addWidget(btnsWidget);
    layout->addWidget(sliderWidget);
    layout->addWidget(_imgLbl);
    layout->addWidget(_timeLbl);
  }

  void _chooseFileBtnClicked() {
    auto path = QFileDialog::getOpenFileName(this, "Open file", "../../blur/img", "Image files (*.png *.jpg *.jpeg *.bmp *.gif *.tiff *.ico *.webp);; All files (*.*)");
    if (path.isEmpty())
      return;

    _imgPath = path;
    _blurBtn->setEnabled(true);
    _blurMPIBtn->setEnabled(true);
    _imgLbl->setPixmap(QPixmap{path});
  }

  void _blurBtnClicked(bool useMPI) {
    if (_imgPath.isEmpty())
      return;

    auto program = useMPI ? "mpirun" : "../../blur/blur-mt.out";

    auto process = new QProcess{this};
    connect(process, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),
    [=](int exitCode, QProcess::ExitStatus exitStatus){ _blurFinished(exitCode, exitStatus); });

    QStringList args;

    if (useMPI)
      args << "--mca" << "opal_warn_on_missing_libcuda" << "0" << "../../blur/blur-mpi.out";

    args << _imgPath << OUT_PATH << QString::number(_radiusSlider->value());

    _blurBtn->setDisabled(true);
    _blurMPIBtn->setDisabled(true);

    _blurStart = std::chrono::high_resolution_clock::now();
    process->start(program, args);
  }

  void _blurFinished(int exitCode, QProcess::ExitStatus exitStatus) {
    auto blurEnd = std::chrono::high_resolution_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(blurEnd - _blurStart).count();

    _blurBtn->setDisabled(false);
    _blurMPIBtn->setDisabled(false);

    if (exitCode == 0 && exitStatus == QProcess::NormalExit) {
      _imgLbl->setPixmap(QPixmap{OUT_PATH});
      _timeLbl->setText("Blurred in " + QString::number(elapsed) + "ms");
    }
    else
      QMessageBox::critical(this, "Error", "Error while blurring image");
  }

public:
  GUI(QWidget *parent = nullptr): QWidget{parent} {
    _initialize();
  }
};

#endif
