#!/bin/bash

echo "Running sonarqube on background"

# ./sonarqube-8.5.1.38104/bin/linux-x86-64/sonar.sh console &

# sleep 30

mkdir -p ../results


# Sends to sonarqube and extract information for auto refactored projects

if [ $# -ne 0 ] && [ $1 == "refactor" ]; then
  for p in ../refactor/projects_refactor/*; do
    cd $p
    mvn clean verify sonar:sonar -Dsonar.scm.disabled=true
    cd ../../../sonarqube
  done

  python3 extract_sonarqube.py sonarqube_refactor yes
else
  echo "Skipping auto refactor analyze"
fi


# Sends to sonarqube and extract information for manual refactored projects

for p in ../refactor/manual_refactor/*; do
  cd $p
  mvn clean verify sonar:sonar -Dsonar.scm.disabled=true
  cd ../../../sonarqube
done

python3 extract_sonarqube.py sonarqube_manual yes


# Sends to sonarqube and extract information for original projects

for p in ../projects_maven/*; do
  cd $p
  mvn clean verify sonar:sonar -Dsonar.scm.disabled=true
  cd ../../sonarqube
done

python3 extract_sonarqube.py sonarqube no



echo "It was impossible to generate a .jar file for the following projects:"

cd ../projects_maven
comm -3 <(find . -iname '*.jar' | awk -F / '{print $2}' | sort) <(ls | sort) | awk '{print $1}'


