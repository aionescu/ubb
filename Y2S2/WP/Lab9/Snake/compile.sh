#!/usr/bin/env bash

set -e

cp=".:lib/javax.servlet.jar:lib/javax.servlet.jsp.jar:lib/javax.servlet.jsp.jstl.jar:lib/mysql-connector-java-5.1.34-bin.jar"

javac -cp "$cp" src/snake/model/*.java src/snake/controller/*.java

rm -rf apache-tomcat-9.0.46/webapps/ROOT
cp -r web apache-tomcat-9.0.46/webapps/ROOT
mkdir -p apache-tomcat-9.0.46/webapps/ROOT/WEB-INF/classes/snake/model
mkdir -p apache-tomcat-9.0.46/webapps/ROOT/WEB-INF/classes/snake/controller


cp src/snake/model/*.class apache-tomcat-9.0.46/webapps/ROOT/WEB-INF/classes/snake/model
cp src/snake/controller/*.class apache-tomcat-9.0.46/webapps/ROOT/WEB-INF/classes/snake/controller

rm src/snake/model/*.class src/snake/controller/*.class
