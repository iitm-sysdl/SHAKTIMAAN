### shakti-soc manager

### Constants
VERSION=0.1.0
REPO_BASE=https://gitlab.com/shaktiproject
DEPS_FOLDER=.

repo_list=(
  $REPO_BASE/common_bsv 
  $REPO_BASE/uncore/fabrics 
  $REPO_BASE/common_verilog 
  )

branch_list=(
  master
  1.1.0
  master
  )
COUNT=${#repo_list[*]}

### Functions to be re-used

## Usage help function
usage () {
    printf "\n"
    printf "shakti-soc manager ${VERSION}\n"
    printf "========================\n"
    printf "\n"
    printf "Usage: ./manager.sh <command> [options]\n"
    printf "\n"
    printf "Available commands: \n\n"
    printf "./manager.sh help \t\t Displays help\n"
    printf "./manager.sh update_deps \t Clones/Updates the Dependent Repos\n"
    printf "./manager.sh nuke \t\t Resets the folder to original settings\n"
    exit 1
}

nuke () {
  make restore
  for i in ${!repo_list[*]}; do
    local dirname=$(extract_name "${repo_list[$i]}") 
    echo "Deleting folder: ${dirname}"
    rm -rf $dirname
  done
}

check_version () {
  local version=$($1 --version | rev | cut -d' ' -f1 | rev)
  local minimum=$(echo -e "$version\n$2" | sort -V | head -n1)
  if [ $2 == $version ] && return 1 || [ $version == $minimum ]; then
    echo "Please update $1 to $2 or above"
    exit 1
  fi
}

## Die function
err() { echo "$*" 1>&2 ;}

## Extract repo directory name
extract_name () {
  echo "$1"| rev | cut -d'/' -f1 | rev
}

## Parse through the list of repos and either clone them if they don't exist or update the existing
## folders
update_deps () {
  for i in ${!repo_list[*]}; do
    local dirname=$(extract_name "${repo_list[$i]}") 
    if [ -d $DEPS_FOLDER/$dirname ]; then
      echo "Updating Repo: " ${repo_list[$i]} 
      (cd $DEPS_FOLDER/$dirname; git pull origin ${branch_list[$i]})
    else
      echo "Cloning Repo: " ${repo_list[$i]} 
      git clone ${repo_list[$i]} $DEPS_FOLDER/$dirname --recursive
      (cd $DEPS_FOLDER/$dirname; git checkout ${branch_list[$i]})
      (cd $DEPS_FOLDER/$dirname; git submodule update --init --recursive)
    fi
  done
}

### Main Script

## Check if no command line args passed, print help and exit
if [ "$#" -eq 0 ]; then
    usage
fi

case $1 in
    help)
        usage
        ;;
    update_deps)
        printf "\nshakti-soc manager ${VERSION} - update_deps\n"
        printf "======================================\n"
        check_version dtc 1.4.7
        update_deps
        ;;
    nuke)
        printf "\nshakti-soc manager ${VERSION} - nuking all changes\n"
        printf "======================================\n"
        nuke
        ;;
esac
