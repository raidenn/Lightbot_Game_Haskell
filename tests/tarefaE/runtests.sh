#!/bin/bash
#
# "runtests.sh <prefix> <cmd>" executa os testes com prefixo "<prefix>",
# utilizando para o efeito o comando "<cmd>".
# Cada caso de teste ´e especificado com um par de ficheiros: a entrada
# com extens~ao ".in", e a sa´ıda esperada com extens~ao ".out".
#
TESTS="${1}*.in"
for i in ../tests/tarefaE/$TESTS;
do TEST=$(basename $i .in)
$2 < ../tests/tarefaE/$TEST.in > ../tarefaE_results/$TEST.html
echo X3dom gerado para $TEST
done
echo 
echo Consultar pasta tarefaE_results na raiz.
echo