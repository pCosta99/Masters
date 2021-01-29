#!/bin/bash

echo "=========================================="
echo "========== Starting Structuring =========="
echo "=========================================="

cd structuring
./structuring.sh
cd ..


if [ $# -ne 0 ] && [ $1 == "refactor" ]; then
  echo "========================================="
  echo "========= Starting Autorefactor ========="
  echo "========================================="

  cd refactor
  ./autorefactor.sh
  cd ..

  echo "=========================================="
  echo "=========== Starting Sonarqube ==========="
  echo "=========================================="

  cd sonarqube
  ./sonarqube.sh $1
  cd ..

  echo "========================================="
  echo "=========== Starting Analysis ==========="
  echo "========================================="

  cd analysis
  ./analysis.sh $1
  cd ..
else
  echo "Skipping auto refactor"

  echo "=========================================="
  echo "=========== Starting Sonarqube ==========="
  echo "=========================================="

  cd sonarqube
  ./sonarqube.sh
  cd ..

  echo "========================================="
  echo "=========== Starting Analysis ==========="
  echo "========================================="

  cd analysis
  ./analysis.sh
  cd ..
fi


