mkdir -p qt-build-output
cd qt-build-output
qmake ../hybris.pro -spec linux-g++ CONFIG+=debug CONFIG+=qml_debug && make qmake_all && make && ./hybris
