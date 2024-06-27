#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/duwe/Projetos/jupiterTools/desktop/out/jupiter
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2     -L. -o /home/duwe/Projetos/jupiterTools/desktop/out/jupiter -T /home/duwe/Projetos/jupiterTools/desktop/out/link32834.res -e _start
if [ $? != 0 ]; then DoExitLink /home/duwe/Projetos/jupiterTools/desktop/out/jupiter; fi
IFS=$OFS
