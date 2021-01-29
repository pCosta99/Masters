#!/bin/bash

# mkdir -p ../results/graphs

# echo "==============================" > ../results/metrics_analysis
# echo "======= NO REFACTORING =======" >> ../results/metrics_analysis
# echo "==============================" >> ../results/metrics_analysis

# Rscript script.r ../results/sonarqube.csv ../results/graphs >> ../results/metrics_analysis

# if [ $# -ne 0 ] && [ $1 == "refactor" ]; then
#   mkdir -p ../results/graphs_refactor

#   echo "=============================" >> ../results/metrics_analysis
#   echo "======== REFACTORING ========" >> ../results/metrics_analysis
#   echo "=============================" >> ../results/metrics_analysis
#   Rscript script.r ../results/sonarqube_refactor.csv ../results/graphs_refactor >> ../results/metrics_analysis
# else
#   echo "Skipping auto refactor analyze"
# fi

# mkdir projects

# cp -r ../projects_maven/67 projects
# cp -r ../projects_maven/83 projects
# cp -r ../projects_maven/2 projects

# cp pom.xml projects/67
# cp pom.xml projects/83
# cp pom.xml projects/2

# ../helpers/javaswitcher.sh 8

# for p in projects/*; do
#   cd $p
#   PROJECT=$(echo $p | awk -F '/' {'print $2'})
#   CLASS=$(grep -RH "void main[^a-zA-Z]" * | awk -F ":" {'print $1'} | sed 's/src\/main\/java\///;s/\.java//;s/\//./g')
#   sed -i "s/PROJECT_ID/$PROJECT/;s/CLASSE/$CLASS/" pom.xml 2> /dev/null
#   mvn -DmemoryInMB=2000 -Dcores=4 evosuite:generate evosuite:export surefire-report:report
#   mvn cobertura:cobertura
#   mkdir -p ../../../results/tests/$PROJECT
#   cp -r target/site/* ../../../results/tests/$PROJECT
#   cd ../..
# done

../helpers/javaswitcher.sh 11

cd RAPL
sudo modprobe msr
make

for p in projects/*; do
  cd $p
  mvn package
  cd ../..
done

mkdir -p ../../results/RAPL

for l in logs/*; do
  cp $l logs.txt
  FILE=$(echo $l | awk -F '/' '{print $2}' | awk -F '.' '{print $1}')
  sudo ./main 'java -jar projects/2/target/2-1.0.jar' 10 '../../results/RAPL/2_'$FILE
  sudo ./main 'java -jar projects/2_refactored/target/2-1.0.jar' 10 '../../results/RAPL/2_refactored_'$FILE
  sudo ./main 'java -jar projects/67/target/67-1.0.jar' 10 '../../results/RAPL/67_'$FILE
  sudo ./main 'java -jar projects/67_refactored/target/67-1.0.jar' 10 '../../results/RAPL/67_refactored_'$FILE
  sudo ./main 'java -jar projects/83/target/83-1.0.jar' 10 '../../results/RAPL/83_'$FILE
  sudo ./main 'java -jar projects/83_refactored/target/83-1.0.jar' 10 '../../results/RAPL/83_refactored_'$FILE
  rm logs.txt
done

make clean
