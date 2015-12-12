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

# Loop the array
for file in "${good[@]}"; do
    file_base=$(basename ${file} ".lat")
    file_path=$(dirname ${file})

    # file_out_val="$file.out"       # The out file to check against
    # file_out_tst="$file.out.tst"   # The outfile from test application
    # Validate infile exists (do the same for out validate file)
    #    if [ ! -f "$file_out_val" ]; then
    #        printf "Validation file %s is missing\n" "$file_out_val"
    #        continue;
    #    fi

    # Run application, redirect in file to app, and output to out file
    "$bin" < "$file" > /dev/null #> "$file_out_tst"

    # Execute diff
    #$diff "$file_out_tst" "$file_out_val"

    # Check exit code from previous command (ie diff)
    # We need to add this to a variable else we can't print it
    # as it will be changed by the if [
    # Iff not 0 then the files differ (at least with diff)
    e_code=$?
    if [ $e_code != 0 ]; then
            printf "[${red}FAIL${normal}] "
    else
            printf "[ ${green}OK${normal} ] "
    fi
    echo ${file_base}

    # Pause by prompt
    #read -p "Enter a to abort, anything else to continue: " input_data
    # Iff input is "a" then abort
    #[ "$input_data" == "a" ] && break

done

echo "TEST INVALID PROGRAMS:";

for file in "${bad[@]}"; do
    file_base=$(basename ${file} ".lat")
    file_path=$(dirname ${file})

    "$bin" < "$file" > /dev/null
    e_code=$?
    if [ $e_code == 0 ]; then
            printf "[${red}FAIL${normal}] "
    else
            printf "[ ${green}OK${normal} ] "
    fi
    echo ${file_base}
done

# Clean exit with status 0
exit 0