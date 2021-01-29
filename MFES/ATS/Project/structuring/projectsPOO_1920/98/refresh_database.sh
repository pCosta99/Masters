#!/bin/bash

check_program() {
    if ! command -v "$1" &>/dev/null; then
        echo "$0: $1 requried."
        exit 1
    fi
}

check_program curl
check_program realpath
check_program tree

if [ $# -eq 0 ]; then
    echo "$0: Expected database directory as argument." >&2
    exit 1
fi


project_db_dir="$1"
tmp_file='/tmp/poo-19-20_logs.txt'
file_url='https://elearning.uminho.pt/bbcswebdav/pid-981796-dt-content-rid-2808290_1/courses/1920.H504N2_2/logs_20200416%281%29.txt'


echo -en "username: a\033[0;31mXXXXX\033[0m\033[5D"
read -r username
username=a"$username"

echo -n 'password: '
read -rs password
echo

if [ -e "$tmp_file" ]; then
    zflag=(-z "$tmp_file")
fi

echo -en '\nFetching database file...'
curl_result=$(curl                  \
    --silent                        \
    --location                      \
    --output $tmp_file              \
    "${zflag[@]}"                   \
    --user "$username":"$password"  \
    --write-out "%{http_code}"      \
    $file_url)
case $curl_result in
2*)
    echo ' done!'
    ;;
304)
    echo -e '\nFile version unchanged, skipping download.\n'
    ;;
401)
    echo -e "\nInvalid credentials." >&2
    rm -f $tmp_file
    exit 1
    ;;
3*|4*)
    echo -e "\nFailed download with http error $curl_result." >&2
    rm -f $tmp_file
    exit 1
    ;;
*)
    echo -e "\nCurl failed to execute with error code $curl_result." >&2
    rm -f $tmp_file
    exit 1
    ;;
esac

mkdir -p "$project_db_dir"

mv $tmp_file "${project_db_dir}/logs.txt"

chmod 644 "$project_db_dir"/*

echo -e "\nFile stored in directory '$(realpath "$project_db_dir")':"
tree -C "$project_db_dir" | head -2
