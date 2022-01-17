mkdir -p qt-build-output
cd qt-build-output

qmake ../out-of-harms-way.pro -spec linux-g++ CONFIG+=debug CONFIG+=qml_debug \
&& make qmake_all \
&& make \
&& ./out-of-harms-way