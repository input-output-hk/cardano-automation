#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
SCRIPT=$0

COMMAND=$1
SCHEMA=$2
ARG1=$3
ARG2=$4

META=""

load_meta() {
if [ ! -d "$ARG1" ]; then
    echo "Error: run directory '${ARG1}' not found."
    exit 1
fi

META="${ARG1}/meta.json"

if [ ! -f "$META" ]; then
    echo "Error: metadata '${META}' not found."
    exit 1
fi
}

# The command_XXX() functions construct a Main.JSONWrapper value.
#
# * The command must match the serialization of Command.Command
# * The payload is the argument to the command and has to match
#   the way CLI arguments are wrapped up in Main.toJSONWrapper

command_list() {
jq      -L "$SCRIPT_DIR"                           \
        --arg cmd List                             \
        --arg schema "$SCHEMA"                     \
        --null-input                               \
        'include "json_wrapper"; wrap(null)'
}

command_bootstrap() {
local SQL=$1
if [ ! -f "$SQL" ]; then
    echo "Error: schema table definition .sql file '${SQL}' not found."
    exit 1
fi

jq      -L "$SCRIPT_DIR"                           \
        --arg cmd Bootstrap                        \
        --arg schema "$SCHEMA"                     \
        --rawfile sql $SQL                         \
        --null-input                               \
        'include "json_wrapper"; wrap([true, $sql])'
# Note: the true literal here corresponds to -f (force) on the CLI
}

command_updateviews() {
local ROLE=$1
local SQL=$2
if [ ! -f "$SQL" ]; then
    echo "Error: schema view definition .sql file '${SQL}' not found."
    exit 1
fi

jq      -L "$SCRIPT_DIR"                           \
        --arg cmd UpdateViews                      \
        --arg schema "$SCHEMA"                     \
        --arg role "$ROLE"                         \
        --rawfile sql $SQL                         \
        --null-input                               \
        'include "json_wrapper"; wrap([$role, $sql])'
}   

command_publish() {
load_meta

MODE=$(case "$COMMAND" in
    publish)    echo true;;
    unpublish)  echo false;;
esac)

jq      -L "$SCRIPT_DIR"                           \
        --arg cmd Publish                          \
        --arg schema "$SCHEMA"                     \
        --argjson mode $MODE                       \
        'include "json_wrapper"; normalize(.) | [$mode, .] | wrap(.)'   \
        $META
}

command_import() {
load_meta

local BLOCKPROP="${ARG1}/analysis/blockprop.json"
local CLUSTERPERF="${ARG1}/analysis/clusterperf.json"

local ARG_BPROP="--argjson bprop null"
local ARG_CPERF="--argjson cperf null"

if [ -f "$BLOCKPROP" ]; then
    ARG_BPROP="--slurpfile bprop ${BLOCKPROP}"
fi

if [ -f "$CLUSTERPERF" ]; then
    ARG_CPERF="--slurpfile cperf ${CLUSTERPERF}"
fi

jq      -L "$SCRIPT_DIR"                            \
        --arg cmd Import                            \
        --arg schema "$SCHEMA"                      \
        $ARG_BPROP                                  \
        $ARG_CPERF                                  \
        'include "json_wrapper"; combine(.) | wrap(.)'  \
        $META
}

usage() {
    echo "usage: $SCRIPT <command> <schema> [<arg1>, ...]"
    echo "  where (by command):"
    echo "    import        <schema> <rundir>           -- Import/update specified run in the DB schema"
    echo "    list          <schema>                    -- List all runs in the DB schema"
    echo "    publish       <schema> <rundir>           -- Publish specified run to API"
    echo "    unpublish     <schema> <rundir>           -- Unpublish specified run from API"
    echo "    bootstrap     <schema> <file.sql>         -- Bootstrap schema onto DB, CLEARING PREVIOUS SCHEMA"
    echo "    update-views  <schema> <role> <file.sql>  -- Only update API facing views for role in the schema"
    exit 1;
}

main() {
case "$COMMAND" in
    list)                   command_list;;
    publish | unpublish)    command_publish;;
    import)                 command_import;;
    bootstrap)              command_bootstrap $ARG1;;
    update-views)           command_updateviews $ARG1 $ARG2;;
    *)                      echo "Error: unknown command '${COMMAND}'"; exit 1;;
esac
}

[[ -z "$@" ]] && usage
if [ -z "$SCHEMA" ]; then
    echo "Error: please specify DB schema."
    exit 1
fi
main
exit 0;
