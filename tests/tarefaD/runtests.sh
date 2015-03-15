#!/bin/bash
#
# "runtests.sh <prefix> <cmd>" executa os testes com prefixo "<prefix>",
# utilizando para o efeito o comando "<cmd>".
# Cada caso de teste ´e especificado com um par de ficheiros: a entrada
# com extens~ao ".in", e a sa´ıda esperada com extens~ao ".out".
#
TESTS="${1}*.in"
for i in $TESTS;
do TEST=$(basename $i .in)
$2 < $TEST.in > ../../tarefaD_results/$TEST.out
echo Comandos gerados para $TEST
done
echo
echo Consultar pasta tarefaD_results na raiz.
echo