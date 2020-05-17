mkdir -p qt-build-output
cd qt-build-output

qmake ../the-shape-of-things-to-come.pro -spec linux-g++ CONFIG+=debug CONFIG+=qml_debug \
&& make qmake_all \
&& make \
&& ./the-shape-of-things-to-come