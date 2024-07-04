repo=icfp2024
old_repo=icfp2023
bin=./target/release/$repo
curl_options=(--silent --show-error)
max_problem_id=90
url=https://boundvariable.space/communicate

# For gnuplot in xpra
export GNUTERM=wxt

export API_TOKEN=(my-keyring $repo)

# * Setup

init_from_last_year_repo() {
  cd ~/src
  gh repo create $repo --private --clone

  cd ~/src/$old_repo
  git archive --format 'tar' HEAD | tar -C ~/src/icfp2024 -xvf -

  cd ~/src/$repo

  rustup update

  cargo upgrade --imcompatible
  carog build

  git ls-files | xargs sd $old_repo $repo

  init_dirs
}

init_dirs() {
  mkdir -p {problem,solution,plot,render}
  mkdir -p solution/submission
}

# * Post

post_entry() {
  post_string "get index"
}

post() {
  cat $1 > ./comm/request.txt
  $bin encode $1 > ./comm/request-encode.txt
  $bin send ./comm/request-encode.txt > ./comm/response-encode.txt
  $bin eval ./comm/response-encode.txt > ./comm/response.txt
  cat ./comm/response.txt
}

post_string() {
  echo $1 > ./comm/request-echo.txt
  post ./comm/request-echo.txt
}

post_icfp() {
  cat $1 > ./comm/request-raw.txt
  $bin send ./comm/request-raw.txt > ./comm/response-encode.txt
  $bin eval ./comm/response-encode.txt > ./comm/response.txt
  cat ./comm/response.txt
}

# * lambdaman

lambdaman_get_problem() {
  local id=${1:-1}
  post_string "get lambdaman$id"
}

lambdaman_download_problem() {
  local id=${1:-1}
  post_string "get lambdaman$id" > ./problem/lambdaman/$id.txt
}

lambdaman_download_problems() {
  # 6, 9, 10, 21 is not string
  for i in {1..21}; do
    echo "Downloading problem: $i..."
    lambdaman_download_problem $i
  done
}

lambdaman_solve() {
  build
  local id=${1:-1}
  local solution=$($bin lambdaman-solve $id)
  if [[ -n $solution ]]; then
    echo "solved: $solution"
    echo $solution > ./solution/lambdaman/$id.txt
  else
    echo "Unresolved?" > /dev/stderr
  fi
}

lambdaman_post() {
  local id=${1:-1}
  local f=./comm/lambdaman-post-$id.txt

  if [[ -s ./solution/lambdaman/$id.txt ]]; then
    echo -n "solve lambdaman$id " > $f
    cat ./solution/lambdaman/$id.txt >>  $f
    post $f
  else
    echo "Skip $id. No solution" 1>&2
  fi
}

# * spaceship

spaceship_solve() {
  local id=${1:-1}
  local f=./solution/spaceship/$id.txt

  $bin spaceship-solve2 $id > $f
  if [[ -s $f ]]; then
    cat $f
  else
    echo "$id: Unresolved?" 1>&2
  fi
}

spaceship_solve_all() {
  local id;
  for id in {1..25}; do
    spaceship_solve $id
  done
}

spaceship_send() {
  local id=${1:-1}
  local f=./comm/spaceship-post-$id.txt

  if [[ -s ./solution/spaceship/$id.txt ]]; then
    echo -n "solve spaceship$id " > $f
    cat ./solution/spaceship/$id.txt >> $f
    post $f
  else
    echo "Skip $id. No solution" 1>&2
  fi
}

spaceship_post_all() {
  for i in {1..25}; do
    spaceship_post $i
  done
}

# * 3d

threed_post() {
  local id=${1:-1}
  local f=./comm/threed-post-$id.txt

  if [[ -s ./solution/threed/$id.txt ]]; then
    echo "solve 3d$id" > $f
    cat ./solution/threed/$id.txt >> $f
    post $f
  else
    echo "Skip $id. No solution" 1>&2
  fi
}

# * efficiency

efficientcy_download_problem() {
  local id=${1:-1}
  post_string "get efficiency$id" > ./problem/efficiency/$id.txt
}
