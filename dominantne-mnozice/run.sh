#! /bin/bash

cat $2 > run-content.txt
echo $3 >> run-content.txt

$1 < run-content.txt > cnf.txt
cryptominisat5 --verb 0 < cnf.txt
