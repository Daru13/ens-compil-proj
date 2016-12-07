#!/bin/bash

# Script appellant le compilateur "adac" sur un ensemble de fichiers
# de tests, dont les chemins sont définis plus bas.

# Dossier à parcourir (les fichiers d'extension ".adb" sont cherchés)
TEST_PATH="$1"

# Arguments à utiliser pour adac
ADAC_ARG="$2"

# Fonction vérifiant que adac est bien disponible
function check_adac_is_exec {
	local path="adac"
	test -x $path
}

# Fonction appelant adac sur tous les fichiers de tests
function test_adac {
	find $TEST_PATH -name *.adb -exec ./adac "{}" $ADAC_ARG \;
}

check_adac_is_exec && test_adac
