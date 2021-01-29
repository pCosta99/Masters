#!/bin/bash

ulimit -n 4096
make
./ats > setup_ats.sh
chmod +x setup_ats.sh
./setup_ats.sh
make clean
rm setup_ats.sh

# copy pom.xml to every project
echo projects_maven/* | xargs -n 1 cp pom.xml .project .classpath

# get main class
for p in projects_maven/*; do
  cd $p
  MainClass=$(grep -RH "void main[^a-zA-Z]" * | awk -F ":" {'print $1'} | sed 's/src\/main\/java\///;s/\.java//;s/\//./g')
  Project=$(echo $p | awk -F / '{print $2}')
  sed -i "s/TrazAqui/$Project/;s/<mainClass>/<mainClass>$MainClass/" pom.xml 2> /dev/null
  sed -i "s/Project/$Project/" .project
  cd ../..
done

cd projects_maven
LAST_COMMAND=$?
if [ $LAST_COMMAND -ne 0 ]; then
  exit
fi
echo ""
echo "It was impossible to determine the main class of the following projects:"
DELETE=$(grep -RH "<mainClass></mainClass>" * | awk -F / '{print $1}')
echo $DELETE
echo ""
echo "$(echo $DELETE | wc -w) projects were deleted"
rm -rf $DELETE


cd ..
mv projects_maven ..
