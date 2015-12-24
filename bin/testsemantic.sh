#!/usr/bin/env bash

dir=$(dirname $0)
bin="$dir/LatteC"
testDir="$dir/../tests/"

declare -a good=($testDir/good/*.lat $testDir/extensions/*/*.lat)
declare -a bad=($testDir/bad/*.lat)

# Colors
red=$(tput setaf 5)
green=$(tput setaf 2)
normal=$(tput sgr0)

numOk=0
numBad=0

echo "Testing valid programs:";

for file in "${good[@]}"; do
    file_base=$(basename ${file} ".lat")
    file_path=$(dirname ${file})

    "$bin" < "$file" > /dev/null #> "$file_out_tst"

    e_code=$?
    if [ $e_code != 0 ]; then
            printf "[${red}FAIL${normal}] "; (( numBad++ ))
            printf "${file_base} \n"
    else
            printf "[ ${green}OK${normal} ] "; (( numOk++ ))
            printf "${file_base} \r"
    fi
done

echo "Testing invalid programs:";

for file in "${bad[@]}"; do
    file_base=$(basename ${file} ".lat")
    file_path=$(dirname ${file})

    "$bin" < "$file" > /dev/null
    e_code=$?
    if [ $e_code == 0 ]; then
            printf "[${red}FAIL${normal}] "; (( numBad++ ))
            printf "${file_base} \n"
    else
            printf "[ ${green}OK${normal} ] "; (( numOk++ ))
            printf "${file_base} \r"
    fi
done

(( total = numOk + numBad ))
(( percentage = (100 * numOk) / total ))
echo "Total: $numOk tests passed out of ${total}. (${percentage}%)"

exit 0