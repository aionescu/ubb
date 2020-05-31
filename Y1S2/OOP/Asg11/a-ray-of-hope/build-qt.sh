mkdir -p qt-build-output
cd qt-build-output

qmake ../a-ray-of-hope.pro -spec linux-g++ CONFIG+=debug CONFIG+=qml_debug \
&& make qmake_all \
&& make \
&& ./a-ray-of-hope