#! /bin/bash
set -uo pipefail

exs=("All" "Equality")

ix=0
for exercise in ${exs[*]}; do
  echo -e "[$ix]\t: $exercise"
  ix=$(($ix + 1))
done

read -p "Which exercise would you like to check? " ans
if [[ $ans -gt ${#exs[*]} ]]; then
  echo "Out of bounds"
fi

if [[ $ans -eq 0 ]]; then
  stack test
else
  theex=${exs[$ans]}
  stack test --test-arguments="--match=${theex// /\//}"
fi
  

