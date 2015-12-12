#!/usr/bin/env bash

dir=$(dirname $0)
bin="$dir/LatteC"  # The application (from command arg)
testDir="$dir/../tests/"
diff="diff -iad"   # Diff command, or what ever

# An array, do not have to declare it, but is supposedly faster
declare -a good=($testDir/good/*.lat $testDir/extensions/*/*.lat)
declare -a bad=($testDir/bad/*.lat)

# Colors
red=$(tput setaf 5)
green=$(tput setaf 2)
normal=$(tput sgr0)

numOk=0
numBad=0

for file in "${good[@]}"; do
    file_base=$(basename ${file} ".lat")
    file_path=$(dirname ${file})

    "$bin" < "$file" > /dev/null #> "$file_out_tst"

    e_code=$?
    if [ $e_code != 0 ]; then
            printf "[${red}FAIL${normal}] "; (( numBad++ ))
    else
            printf "[ ${green}OK${normal} ] "; (( numOk++ ))
    fi
    echo ${file_base}

done

echo "TEST INVALID PROGRAMS:";

for file in "${bad[@]}"; do
    file_base=$(basename ${file} ".lat")
    file_path=$(dirname ${file})

    "$bin" < "$file" > /dev/null
    e_code=$?
    if [ $e_code == 0 ]; then
            printf "[${red}FAIL${normal}] "; (( numBad++ ))
    else
            printf "[ ${green}OK${normal} ] "; (( numOk++ ))
    fi
    echo ${file_base}

done

(( total = numOk + numBad ))
(( percentage = (100 * numOk) / total ))
echo "Total: $numOk tests passed out of ${total}. (${percentage}%)"

# Clean exit with status 0
exit 0