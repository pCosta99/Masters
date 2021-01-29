#!/bin/bash


mkdir projects_refactor
cp -r ../projects_maven/* projects_refactor

for ID in projects_refactor/*; do
  echo "STARTING REFACTOR OF PROJECT $ID"
  AUTOREFACTOR_ECLIPSE=autorefactor/eclipse/eclipse ./autorefactor/AutoRefactorCli/cli/target/autorefactor/bin/autorefactor apply --project $ID/.project --source src/main/java --refactorings $(cat refactors.txt) &> /dev/null
done


echo "Refactor completed"
